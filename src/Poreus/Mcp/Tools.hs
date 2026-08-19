{-# LANGUAGE ConstraintKinds #-}

module Poreus.Mcp.Tools
  ( -- * Environment
    McpEnv (..)
  , ToolM

    -- * Registry
  , ToolDef (..)
  , toolDefs
  , runTool
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)

import Poreus.Catalog (DiscoverFilters (..), discover, noFilters)
import Poreus.Deliver (Delivered, deliverPending)
import Poreus.Doorbell (doorbellFor)
import Poreus.Effects.Env (CanEnv)
import Poreus.Effects.FileSystem (CanFileSystem)
import Poreus.Effects.Process (CanProcess)
import Poreus.Effects.Random (CanRandom)
import Poreus.Effects.SystemInfo (CanSystemInfo)
import Poreus.Effects.Time (CanTime)
import Poreus.Identity (Identity (..))
import Poreus.Mcp.Errors (toolFailure, toolSuccess)
import Poreus.Name (ClaimOutcome (..), RetireOutcome (..), boundNameOf, claimName, mailboxesOf, releaseName, retireName, suggestRoleName)
import Poreus.Post (Sender (..), postCall, postNotify, postReply, postRequest)
import Poreus.Profile (EndpointInput (..), PublishResult (..), publishProfile)
import Poreus.Query (QueryFilters (..), QueryResult (..), noQueryFilters, parseScope, runQuery)
import qualified Poreus.Repo as Repo
import Poreus.Retention (retentionDays, sweep)
import Poreus.Session (SessionRow (..), ensureSession, getSession)
import Poreus.Time (Timestamp (..), parseUtcLoose)
import Poreus.Types

-- | Everything a tool handler needs: the shared connection, the
-- session identity resolved at startup, and the serving process facts
-- for liveness corroboration.
data McpEnv = McpEnv
  { envConn :: !Connection
  , envIdentity :: !Identity
  , envPid :: !(Maybe Int)
  , envBootId :: !(Maybe Text)
  }

-- | The capability bundle every handler runs under. `TestIOM`
-- satisfies it, which is what makes the whole tool surface
-- deterministic under test (C-9).
type ToolM m =
  ( CanTime m
  , CanRandom m
  , CanEnv m
  , CanFileSystem m
  , CanProcess m
  , CanSystemInfo m
  , MonadIO m
  )

-- | Static tool metadata for tools/list.
data ToolDef = ToolDef
  { tdName :: !Text
  , tdDescription :: !Text
  , tdSchema :: !Value
  }

-- ---------------------------------------------------------------------
-- Argument helpers
-- ---------------------------------------------------------------------

badInput :: Text -> Either PoreusError a
badInput msg = Left (mkError InvalidInput msg)

reqText :: A.Object -> Text -> Either PoreusError Text
reqText o k = case KM.lookup (Key.fromText k) o of
  Just (String t) -> Right t
  Just _ -> badInput ("'" <> k <> "' must be a string")
  Nothing -> badInput ("missing required argument '" <> k <> "'")

optText :: A.Object -> Text -> Either PoreusError (Maybe Text)
optText o k = case KM.lookup (Key.fromText k) o of
  Nothing -> Right Nothing
  Just Null -> Right Nothing
  Just (String t) -> Right (Just t)
  Just _ -> badInput ("'" <> k <> "' must be a string")

optBool :: A.Object -> Text -> Bool -> Either PoreusError Bool
optBool o k dflt = case KM.lookup (Key.fromText k) o of
  Nothing -> Right dflt
  Just Null -> Right dflt
  Just (Bool b) -> Right b
  Just _ -> badInput ("'" <> k <> "' must be a boolean")

optInt :: A.Object -> Text -> Either PoreusError (Maybe Int)
optInt o k = case KM.lookup (Key.fromText k) o of
  Nothing -> Right Nothing
  Just Null -> Right Nothing
  Just (Number n) -> Right (Just (round n))
  Just _ -> badInput ("'" <> k <> "' must be a number")

optValue :: A.Object -> Text -> Maybe Value
optValue o k = case KM.lookup (Key.fromText k) o of
  Just Null -> Nothing
  v -> v

optTextList :: A.Object -> Text -> Either PoreusError [Text]
optTextList o k = case KM.lookup (Key.fromText k) o of
  Nothing -> Right []
  Just Null -> Right []
  Just (Array xs) -> traverse asText (foldr (:) [] xs)
  Just _ -> badInput ("'" <> k <> "' must be an array of strings")
  where
    asText (String t) = Right t
    asText _ = badInput ("'" <> k <> "' must be an array of strings")

-- ---------------------------------------------------------------------
-- Result envelope
-- ---------------------------------------------------------------------

-- | Wrap a handler outcome: domain errors become isError results;
-- successes carry warnings and the piggyback delivery (`new_messages`,
-- the acknowledged path that advances the cursor — RECV-1). Every
-- successful result also carries the `session-unnamed` nudge while it
-- applies: fail fast on a missing role name at the moment poreus is
-- actually being used, instead of letting the delegation graph degrade
-- silently.
finish :: ToolM m => McpEnv -> Either PoreusError (Value, [Warning]) -> m Value
finish env = \case
  Left e -> pure (toolFailure e)
  Right (v, ws) -> do
    boxes <- mailboxesOf (envConn env) (idAddress (envIdentity env))
    delivered <- deliverPending (envConn env) boxes
    nudge <- namelessNudge env
    pure (toolSuccess (withExtras v (ws <> nudge) delivered))

-- | The point-of-use half of the role nudge: a session working through
-- poreus while holding no name, in a git workspace whose derived role
-- is available. The system never claims on its own (REG-3: claiming is
-- voluntary) — it says so, once per result, until the model or user
-- decides.
namelessNudge :: ToolM m => McpEnv -> m [Warning]
namelessNudge env = do
  suggestion <-
    suggestRoleName
      (envConn env)
      (idAddress (envIdentity env))
      (T.unpack (idWorkspace (envIdentity env)))
  pure
    [ Warning
        "session-unnamed"
        ( "this session holds no name, so peers cannot address it by role; the role '"
            <> unAgentName nm
            <> "' is available for this workspace. If this session represents the repo, call claim_name (ask the user when unsure)."
        )
    | Just nm <- [suggestion]
    ]

withExtras :: Value -> [Warning] -> [Delivered] -> Value
withExtras v ws delivered = case v of
  Object o ->
    Object
      ( o
          `KM.union` KM.fromList
            ( [("warnings", A.toJSON ws) | not (null ws)]
                <> [("new_messages", A.toJSON delivered) | not (null delivered)]
            )
      )
  other -> other

ok :: [(Key.Key, Value)] -> Either PoreusError (Value, [Warning])
ok fields = Right (Object (KM.fromList fields), [])

-- ---------------------------------------------------------------------
-- Dispatch
-- ---------------------------------------------------------------------

-- | Run one tool by name. Returns Nothing for an unknown tool (the
-- protocol layer maps that to a JSON-RPC invalid-params error). Every
-- call auto-provisions and heartbeats the session (REG-1/REG-2: no
-- operation ever fails with "not initialized").
runTool :: ToolM m => McpEnv -> Text -> A.Object -> m (Maybe Value)
runTool env name args = case lookup name handlers of
  Nothing -> pure Nothing
  Just h -> do
    let Identity{idAddress, idWorkspace} = envIdentity env
    _ <- ensureSession (envConn env) idAddress idWorkspace (envPid env) (envBootId env)
    Just <$> h env args

handlers :: ToolM m => [(Text, McpEnv -> A.Object -> m Value)]
handlers =
  [ ("whoami", toolWhoami)
  , ("claim_name", toolClaimName)
  , ("release_name", toolReleaseName)
  , ("retire_name", toolRetireName)
  , ("publish_profile", toolPublishProfile)
  , ("discover", toolDiscover)
  , ("request", toolRequest)
  , ("call", toolCall)
  , ("reply", toolReply)
  , ("notify", toolNotify)
  , ("messages", toolMessages)
  , ("purge", toolPurge)
  ]

sender :: ToolM m => McpEnv -> m Sender
sender env = do
  let me = idAddress (envIdentity env)
  Sender me <$> boundNameOf (envConn env) me

-- ---------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------

toolWhoami :: ToolM m => McpEnv -> A.Object -> m Value
toolWhoami env _ = do
  let Identity{idAddress, idWorkspace} = envIdentity env
  bound <- boundNameOf (envConn env) idAddress
  row <- getSession (envConn env) idAddress
  finish env $
    ok
      [ ("address", A.toJSON idAddress)
      , ("name", A.toJSON bound)
      , ("workspace", A.toJSON idWorkspace)
      , -- What the host calls this session. Peers ring it by this
        -- name, so it is worth knowing that it changed under you.
        ("host_name", A.toJSON (row >>= sessHostName))
      ]

toolClaimName :: ToolM m => McpEnv -> A.Object -> m Value
toolClaimName env args = do
  let c = envConn env
      me = idAddress (envIdentity env)
  r <- case (,) <$> optText args "name" <*> optBool args "takeover" False of
    Left e -> pure (Left e)
    Right (mname, takeover) -> do
      name <- case mname of
        Just n -> pure n
        Nothing -> Repo.repoAlias (T.unpack (idWorkspace (envIdentity env)))
      outcome <- claimName c me name takeover
      pure $ do
        ClaimOutcome{coName, coPreviousHolder, coReleased} <- outcome
        ok $
          [("name", A.toJSON coName)]
            <> [("previous_holder", A.toJSON h) | Just h <- [coPreviousHolder]]
            <> [("released", A.toJSON n) | Just n <- [coReleased]]
  finish env r

toolReleaseName :: ToolM m => McpEnv -> A.Object -> m Value
toolReleaseName env _ = do
  released <- releaseName (envConn env) (idAddress (envIdentity env))
  finish env $ ok [("released", A.toJSON released)]

toolRetireName :: ToolM m => McpEnv -> A.Object -> m Value
toolRetireName env args = do
  r <- case (,) <$> reqText args "name" <*> optBool args "force" False of
    Left e -> pure (Left e)
    Right (name, force) -> do
      outcome <- retireName (envConn env) name force
      pure $ do
        RetireOutcome{roOpenRequests, roDiscarded} <- outcome
        ok
          [ ("retired", A.toJSON name)
          , ("open_requests", A.toJSON roOpenRequests)
          , ("discarded", A.toJSON roDiscarded)
          ]
  finish env r

toolPublishProfile :: ToolM m => McpEnv -> A.Object -> m Value
toolPublishProfile env args = do
  let parsed = do
        mname <- optText args "name"
        summary <- reqText args "summary"
        tags <- optTextList args "tags"
        epsRaw <- case KM.lookup "endpoints" args of
          Nothing -> Right []
          Just Null -> Right []
          Just (Array xs) -> Right (foldr (:) [] xs)
          Just _ -> badInput "'endpoints' must be an array"
        eps <- traverse parseEndpoint epsRaw
        pure (mname, summary, tags, eps)
  r <- case parsed of
    Left e -> pure (Left e)
    Right (mname, summary, tags, eps) -> do
      outcome <-
        publishProfile (envConn env) (idAddress (envIdentity env)) mname summary tags eps
      pure $ do
        PublishResult{prName, prEndpointCount, prUpdatedAt, prPreviousHolder, prReleased} <- outcome
        ok $
          [ ("name", A.toJSON prName)
          , ("endpoints", A.toJSON prEndpointCount)
          , ("updated_at", A.toJSON prUpdatedAt)
          ]
            <> [("previous_holder", A.toJSON h) | Just h <- [prPreviousHolder]]
            <> [("released", A.toJSON n) | Just n <- [prReleased]]
  finish env r

parseEndpoint :: Value -> Either PoreusError EndpointInput
parseEndpoint (Object o) = do
  verb <- reqText o "verb"
  description <- reqText o "description"
  autonomyT <- reqText o "autonomy"
  autonomy <-
    maybe
      (Left (mkErrorWithAction InvalidInput ("invalid autonomy '" <> autonomyT <> "'") "use 'auto' (pre-approved) or 'confirm' (ask the user first)"))
      Right
      (parseAutonomy autonomyT)
  hint <- optText o "usage_hint"
  pure (EndpointInput verb description autonomy hint)
parseEndpoint _ = badInput "each endpoint must be an object with verb, description, autonomy"

toolDiscover :: ToolM m => McpEnv -> A.Object -> m Value
toolDiscover env args = do
  r <- case parsed of
    Left e -> pure (Left e)
    Right filters -> do
      catalog <- discover (envConn env) filters
      pure (Right (A.toJSON catalog, []))
  finish env r
  where
    parsed = do
      tag <- optText args "tag"
      verb <- optText args "verb"
      address <- optText args "address"
      pure noFilters{dfTag = tag, dfVerb = verb, dfAddress = address}

toolRequest :: ToolM m => McpEnv -> A.Object -> m Value
toolRequest env args = do
  r <- case parsed of
    Left e -> pure (Left e)
    Right (to, description, expected, create) -> do
      s <- sender env
      outcome <- postRequest (envConn env) s to description expected (optValue args "payload") create
      messageResult env outcome
  finish env r
  where
    parsed = do
      to <- reqText args "to"
      description <- reqText args "description"
      expected <- optText args "expected_outcome"
      create <- optBool args "create_role" False
      pure (to, description, expected, create)

toolCall :: ToolM m => McpEnv -> A.Object -> m Value
toolCall env args = do
  r <- case (,,) <$> reqText args "to" <*> reqText args "verb" <*> optBool args "create_role" False of
    Left e -> pure (Left e)
    Right (to, verb, create) -> do
      s <- sender env
      outcome <- postCall (envConn env) s to verb (optValue args "args") create
      messageResult env outcome
  finish env r

toolReply :: ToolM m => McpEnv -> A.Object -> m Value
toolReply env args = do
  r <- case parsed of
    Left e -> pure (Left e)
    Right (inReplyTo, event, summary) -> do
      s <- sender env
      outcome <-
        postReply (envConn env) s (MessageId inReplyTo) event summary (optValue args "artifacts")
      messageResult env outcome
  finish env r
  where
    parsed = do
      inReplyTo <- reqText args "in_reply_to"
      event <- reqText args "event"
      summary <- optText args "summary"
      pure (inReplyTo, event, summary)

toolNotify :: ToolM m => McpEnv -> A.Object -> m Value
toolNotify env args = do
  r <- case parsed of
    Left e -> pure (Left e)
    Right (to, event, summary, create) -> do
      s <- sender env
      outcome <- postNotify (envConn env) s to event summary (optValue args "payload") create
      messageResult env outcome
  finish env r
  where
    parsed = do
      to <- reqText args "to"
      event <- optText args "event"
      summary <- optText args "summary"
      create <- optBool args "create_role" False
      pure (to, event, summary, create)

-- | A post's result: the stored message, plus the optional doorbell —
-- the one thing the sending model may do to shorten latency. The
-- message is already durably stored by the time this runs, so a
-- missing doorbell changes nothing about delivery.
messageResult ::
  ToolM m =>
  McpEnv ->
  Either PoreusError (Message, [Warning]) ->
  m (Either PoreusError (Value, [Warning]))
messageResult _ (Left e) = pure (Left e)
messageResult env (Right (m, ws)) = do
  bell <- doorbellFor (envConn env) (msgTo m)
  pure . Right $
    ( object (["message" .= m] <> maybe [] (\b -> ["doorbell" .= b]) bell)
    , ws
    )

toolMessages :: ToolM m => McpEnv -> A.Object -> m Value
toolMessages env args = do
  r <- case parsed of
    Left e -> pure (Left e)
    Right (scope, filters) -> do
      boxes <- mailboxesOf (envConn env) (idAddress (envIdentity env))
      outcome <- runQuery (envConn env) boxes scope filters
      pure $ do
        QueryResult{qrMessages, qrThreadStatus} <- outcome
        ok $
          [("messages", A.toJSON qrMessages)]
            <> [("thread_status", A.toJSON s) | Just s <- [qrThreadStatus]]
  finish env r
  where
    parsed = do
      scopeT <- reqText args "scope"
      scope <-
        maybe
          (Left (mkErrorWithAction InvalidInput ("invalid scope '" <> scopeT <> "'") "use one of: inbox, open, history, thread"))
          Right
          (parseScope scopeT)
      thread <- optText args "thread"
      from <- optText args "from"
      involving <- optText args "involving"
      kindT <- optText args "kind"
      kind <- case kindT of
        Nothing -> Right Nothing
        Just k ->
          maybe
            (Left (mkErrorWithAction InvalidInput ("invalid kind '" <> k <> "'") "use 'request' or 'notice'"))
            (Right . Just)
            (parseMessageKind k)
      sinceT <- optText args "since"
      since <- case sinceT of
        Nothing -> Right Nothing
        Just s ->
          maybe
            (Left (mkErrorWithAction InvalidInput ("invalid since timestamp '" <> s <> "'") "use ISO 8601, e.g. 2026-08-14T12:00:00Z"))
            (Right . Just . Timestamp)
            (parseUtcLoose s)
      limit <- optInt args "limit"
      pure
        ( scope
        , noQueryFilters
            { qfThread = MessageId <$> thread
            , qfFrom = from
            , qfInvolving = involving
            , qfKind = kind
            , qfSince = since
            , qfLimit = limit
            }
        )

toolPurge :: ToolM m => McpEnv -> A.Object -> m Value
toolPurge env args = do
  r <- case (,) <$> optBool args "confirm" False <*> optInt args "older_than_days" of
    Left e -> pure (Left e)
    Right (False, _) ->
      pure . Left $
        mkErrorWithAction
          InvalidInput
          "purge is destructive and requires confirmation"
          "pass confirm: true to delete messages and ended-session records older than the window"
    Right (True, mdays) -> do
      days <- maybe retentionDays pure mdays
      result <- sweep (envConn env) days
      pure (Right (A.toJSON result, []))
  finish env r

-- ---------------------------------------------------------------------
-- tools/list metadata
-- ---------------------------------------------------------------------

toolDefs :: [ToolDef]
toolDefs =
  [ ToolDef
      "whoami"
      "Identity check: this session's poreus address (auto-provisioned, always available), its claimed name if any, and its workspace. Use the address never as something to type by hand — peers discover it via the discover tool."
      (objSchema [] [])
  , ToolDef
      "claim_name"
      "Claim a stable, human-friendly name (e.g. the repo role like 'nixos') so peers can address this session by role. Optional: names only enrich discovery and routing — sending, receiving, and replying work without one. Omitting 'name' claims the workspace-derived default. If the name is held by another live session the call fails with name-held; pass takeover: true to displace it (the displaced session keeps its address, mailbox, and in-flight threads)."
      ( objSchema
          [ ("name", strProp "Name to claim: lowercase kebab-case, must not start with 's-'. Defaults to the repo-root basename.")
          , ("takeover", boolProp "Displace a live holder. Only after the user confirmed, or when the holder is stale.")
          ]
          []
      )
  , ToolDef
      "release_name"
      "Release this role so another session can claim it (e.g. when handing over). The role, its profile, its mailbox and its delivery cursor all stay intact — mail addressed to the role keeps arriving and the next holder drains it from where you stopped. Nothing is lost by releasing."
      (objSchema [] [])
  , ToolDef
      "retire_name"
      "Delete a role outright: its profile, endpoints, mailbox and catalog entry (delivered history is not rewritten). Use when a repo is deleted or a role should stop advertising capabilities. Distinct from release_name, which keeps the role for the next claimant. Refuses while undelivered mail is queued for the role — pass force: true to retire anyway, and the result reports how many messages that discarded."
      ( objSchema
          [ ("name", strProp "The role to retire.")
          , ("force", boolProp "Retire even though mail is still queued for the role, discarding it. The result reports the count.")
          ]
          ["name"]
      )
  , ToolDef
      "publish_profile"
      "Publish this agent's capability profile: a summary, 3-8 tags, and typed RPC endpoints. Atomically replaces the previous profile. Implies claiming the name if not yet held. Endpoint autonomy: 'auto' means peers may invoke it unattended (pre-approved); 'confirm' means the executing side asks its user first. Rule of thumb: read-shaped verbs auto, mutating verbs confirm, unsure confirm. Show the draft to your user before publishing."
      ( objSchema
          [ ("name", strProp "Name to attach the profile to. Defaults to this session's bound name.")
          , ("summary", strProp "One or two sentences: what this repo/agent can do.")
          , ("tags", object ["type" .= str "array", "items" .= object ["type" .= str "string"], "description" .= str "3-8 lowercase topic tags for discovery."])
          ,
            ( "endpoints"
            , object
                [ "type" .= str "array"
                , "description" .= str "Typed capabilities. Keep the set lean — only verbs you actually serve."
                , "items"
                    .= objSchema
                      [ ("verb", strProp "Kebab-case verb, unique per name, e.g. 'deploy-poreus'.")
                      , ("description", strProp "What invoking this verb does.")
                      , ("autonomy", object ["type" .= str "string", "enum" .= [str "auto", str "confirm"], "description" .= str "auto: execute unattended; confirm: ask the user first."])
                      , ("usage_hint", strProp "Optional free-text hint about expected args, e.g. \"args: {sha}\".")
                      ]
                      ["verb", "description", "autonomy"]
                ]
            )
          ]
          ["summary"]
      )
  , ToolDef
      "discover"
      "Browse the agent catalog: roles (profile, endpoints, holder_process, holder_host_name, queued) and sessions (address, workspace, process). Address roles, not sessions: a role's mailbox is durable, so mail to a role with no live holder is queued and delivered when a session claims it, while a session address dies with its process. holder_process is an annotation, never a reason to skip a role — it says a process exists, not that anyone is reading. Filters: tag, verb (exact match — use to find providers before call), address (one role or session)."
      ( objSchema
          [ ("tag", strProp "Only roles carrying this tag.")
          , ("verb", strProp "Only roles offering exactly this endpoint verb.")
          , ("address", strProp "Restrict to one role name or session address.")
          ]
          []
      )
  , ToolDef
      "request"
      "Delegate work described in prose to another agent (free-text request). Address 'to' with a role name from discover — the message lands in that role's durable mailbox whether or not a session is serving it right now. Keep the returned message_id: replies correlate to it. The responder owes you exactly one terminal notice (completed/failed/aborted); check with messages scope: thread. If the result carries a 'doorbell', that is an optional latency step — call SendMessage once with its agent and body, then continue; never retry it, never wait on it, and never put content in it."
      ( objSchema
          [ ("to", strProp "Target: a role name (e.g. 'nixos') or a session address ('s-...'). Prefer the role.")
          , ("description", strProp "What to do, in prose. Be specific about context the peer lacks.")
          , ("expected_outcome", strProp "What done looks like, if useful.")
          , ("payload", object ["type" .= str "object", "description" .= str "Optional extra structured fields, stored verbatim under 'data'."])
          , ("create_role", boolProp "Queue for a role that does not exist yet, creating it. Off by default so a typo fails instead of creating a mailbox nobody drains.")
          ]
          ["to", "description"]
      )
  , ToolDef
      "call"
      "Invoke a typed RPC endpoint on a role (found via discover). Prefer this over request when a matching verb exists — the target knows exactly what to do. Warns (but still posts) if the endpoint is not currently in the catalog. Same doorbell rule as request: ring once if offered, never retry."
      ( objSchema
          [ ("to", strProp "Target role name (endpoints attach to roles) or session address.")
          , ("verb", strProp "The endpoint verb, exactly as advertised.")
          , ("args", object ["type" .= str "object", "description" .= str "Named arguments object, per the endpoint's usage hint."])
          , ("create_role", boolProp "Queue for a role that does not exist yet, creating it.")
          ]
          ["to", "verb"]
      )
  , ToolDef
      "reply"
      "Emit a lifecycle notice for a request you received (the reply duty): exactly one terminal event per request — 'completed', 'failed', or 'aborted' — with a summary; plus 'started' when the work is more than momentary and 'stuck' when blocked. Routes automatically to the session that sent the request. Custom event names are legal. Warns when the thread already has a terminal notice."
      ( objSchema
          [ ("in_reply_to", strProp "The message_id of the request being answered.")
          , ("event", strProp "Lifecycle event: started | stuck | completed | failed | aborted (recommended; custom names allowed).")
          , ("summary", strProp "One or two sentences: what happened / what was produced.")
          , ("artifacts", object ["type" .= str "array", "description" .= str "Optional artifact records, by convention {type, value, description} — e.g. a commit sha, a file path, a URL.", "items" .= object ["type" .= str "object"]])
          ]
          ["in_reply_to", "event"]
      )
  , ToolDef
      "notify"
      "Send an uncorrelated notice: broadcast-style information ('protocol upgraded, please re-register') or an unsolicited ping. Not for answering requests — use reply for that, so the requester can correlate."
      ( objSchema
          [ ("to", strProp "Target role name or session address.")
          , ("event", strProp "Optional event label.")
          , ("summary", strProp "Optional human-readable summary.")
          , ("payload", object ["type" .= str "object", "description" .= str "Optional extra structured fields, stored verbatim under 'data'."])
          , ("create_role", boolProp "Queue for a role that does not exist yet, creating it.")
          ]
          ["to"]
      )
  , ToolDef
      "messages"
      "The one query surface over the message store (side-effect-free; never touches delivery cursors). Reads both mailboxes I drain — my session's and my role's. Scopes: 'inbox' — everything addressed to me; 'open' — requests to me still lacking any reply notice, including ones a former holder of my role left unanswered (reply to adopt one); 'history' — recent traffic involving a role or address (default me, newest first, limit 10); 'thread' — one request plus all its reply notices, with a derived thread_status (open/active/terminal) answering \"is it finished?\"."
      ( objSchema
          [ ("scope", object ["type" .= str "string", "enum" .= map str ["inbox", "open", "history", "thread"], "description" .= str "Which view to query."])
          , ("thread", strProp "For scope thread: the root message_id.")
          , ("from", strProp "Filter by sender (address or name).")
          , ("involving", strProp "For scope history: the role name or session address to look at (default: me).")
          , ("kind", object ["type" .= str "string", "enum" .= [str "request", str "notice"], "description" .= str "Filter by message kind."])
          , ("since", strProp "Only messages created after this ISO 8601 timestamp.")
          , ("limit", object ["type" .= str "number", "description" .= str "Cap the result count."])
          ]
          ["scope"]
      )
  , ToolDef
      "purge"
      "Operator command: delete messages and ended-session records older than the retention window (default 30 days, or older_than_days). Names and profiles are never purged. Destructive — requires confirm: true, and the user should have asked for it."
      ( objSchema
          [ ("older_than_days", object ["type" .= str "number", "description" .= str "Override the retention window for this purge."])
          , ("confirm", boolProp "Must be true. Confirms the user asked for a purge.")
          ]
          ["confirm"]
      )
  ]

-- Schema-building helpers.

str :: Text -> Value
str = String

objSchema :: [(Key.Key, Value)] -> [Text] -> Value
objSchema props required =
  object $
    [ "type" .= str "object"
    , "properties" .= Object (KM.fromList props)
    , "additionalProperties" .= False
    ]
      <> (["required" .= required | not (null required)])

strProp :: Text -> Value
strProp desc = object ["type" .= str "string", "description" .= desc]

boolProp :: Text -> Value
boolProp desc = object ["type" .= str "boolean", "description" .= desc]
