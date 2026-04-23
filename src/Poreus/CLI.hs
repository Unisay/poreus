module Poreus.CLI
  ( run
  ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

import Poreus.Config (poreusHome)
import qualified Poreus.DB as DB
import Poreus.Effects.Env (getCurrentDir)
import Poreus.Effects.Random (randomHex4)
import Poreus.Effects.Time (currentTime)
import qualified Poreus.Endpoint as Endpoint
import qualified Poreus.Exit as Exit
import qualified Poreus.Inspect as Inspect
import qualified Poreus.JSON as J
import qualified Poreus.Message as Msg
import qualified Poreus.Migrate as Migrate
import qualified Poreus.Profile as Profile
import qualified Poreus.Repo as Repo
import qualified Poreus.Task as Task
import Poreus.Time (Timestamp (..))
import qualified Poreus.Watch as Watch
import Poreus.Types

data InboxKind = InboxKRequest | InboxKReply | InboxKAll
  deriving stock (Show, Eq)

data Cmd
  = CmdInit
  | CmdRegister Alias Text
  | CmdPutProfile Alias
  | CmdInspectRepo (Maybe FilePath)
  | CmdDiscover (Maybe Text) (Maybe Text) (Maybe Alias)
  | CmdMatch Text (Maybe Text) (Maybe Text)
  | CmdSend
  | CmdInbox (Maybe Alias) (Maybe TaskStatus) Bool InboxKind
  | CmdClaim TaskId
  | CmdComplete TaskId
  | CmdReject TaskId Text
  | CmdStatus (Maybe TaskId) Bool Bool
  | CmdWatchCheck
  | CmdMigrate
  deriving stock (Show, Eq)

parser :: Parser Cmd
parser =
  hsubparser $
    mconcat
      [ command "init" (info (pure CmdInit) (progDesc "Create $POREUS_HOME and run schema migration"))
      , command "register" (info registerP (progDesc "Register an agent alias at a path"))
      , command "put-profile" (info putProfileP (progDesc "Replace summary/tags/endpoints from stdin JSON"))
      , command "inspect-repo" (info inspectRepoP (progDesc "Emit signals about the target repo"))
      , command "discover" (info discoverP (progDesc "List agents with their endpoints"))
      , command "match-endpoint" (info matchP (progDesc "Find agents offering a given verb"))
      , command "send" (info (pure CmdSend) (progDesc "Send a task (read JSON from stdin)"))
      , command "inbox" (info inboxP (progDesc "List tasks in this alias' inbox"))
      , command "claim" (info claimP (progDesc "Claim a pending task"))
      , command "complete" (info completeP (progDesc "Complete/fail a claimed task (JSON on stdin)"))
      , command "reject" (info rejectP (progDesc "Reject a pending/claimed task"))
      , command "status" (info statusP (progDesc "Show task status"))
      , command "watch-check" (info (pure CmdWatchCheck) (progDesc "Emit unseen messages addressed to this alias"))
      , command "migrate" (info (pure CmdMigrate) (progDesc "Import legacy a2a-queue/ files"))
      ]

registerP :: Parser Cmd
registerP =
  CmdRegister
    <$> argument aliasR (metavar "ALIAS")
    <*> argument textR (metavar "PATH")

putProfileP :: Parser Cmd
putProfileP = CmdPutProfile <$> argument aliasR (metavar "ALIAS")

inspectRepoP :: Parser Cmd
inspectRepoP =
  CmdInspectRepo
    <$> optional (strOption (long "path" <> metavar "DIR" <> help "Target repo (default: cwd)"))

discoverP :: Parser Cmd
discoverP =
  CmdDiscover
    <$> optional (strOption (long "tag" <> metavar "T"))
    <*> optional (strOption (long "verb" <> metavar "V"))
    <*> optional (aliasOption (long "agent" <> metavar "A"))

matchP :: Parser Cmd
matchP =
  CmdMatch
    <$> strOption (long "verb" <> metavar "V" <> help "Verb to match")
    <*> optional (strOption (long "arg" <> metavar "A"))
    <*> optional (strOption (long "tag" <> metavar "T"))

inboxP :: Parser Cmd
inboxP =
  CmdInbox
    <$> optional (aliasOption (long "alias" <> metavar "A"))
    <*> optional (statusOption (long "status" <> metavar "S"))
    <*> switch (long "all" <> help "Include all statuses (overrides default pending)")
    <*> inboxKindP

inboxKindP :: Parser InboxKind
inboxKindP =
  option
    kindR
    ( long "kind"
        <> metavar "request|reply|all"
        <> value InboxKRequest
        <> showDefaultWith (const "request")
        <> help "Which kind of messages to return (default: request = pending tasks)"
    )
  where
    kindR = do
      t <- T.pack <$> str
      case t of
        "request" -> pure InboxKRequest
        "reply" -> pure InboxKReply
        "all" -> pure InboxKAll
        _ -> readerError ("invalid --kind value: " <> T.unpack t)

claimP :: Parser Cmd
claimP = CmdClaim <$> argument taskIdR (metavar "TASK-ID")

completeP :: Parser Cmd
completeP = CmdComplete <$> argument taskIdR (metavar "TASK-ID")

rejectP :: Parser Cmd
rejectP =
  CmdReject
    <$> argument taskIdR (metavar "TASK-ID")
    <*> strOption (long "reason" <> metavar "TEXT")

statusP :: Parser Cmd
statusP =
  CmdStatus
    <$> optional (argument taskIdR (metavar "TASK-ID"))
    <*> switch (long "sent")
    <*> switch (long "received")

aliasR :: ReadM Alias
aliasR = Alias <$> textR

taskIdR :: ReadM TaskId
taskIdR = TaskId <$> textR

textR :: ReadM Text
textR = T.pack <$> str

aliasOption :: Mod OptionFields Alias -> Parser Alias
aliasOption m = option aliasR m

statusOption :: Mod OptionFields TaskStatus -> Parser TaskStatus
statusOption m = option statusR m
  where
    statusR = do
      t <- T.pack <$> str
      case parseTaskStatus t of
        Just s -> pure s
        Nothing -> readerError ("invalid status: " <> T.unpack t)

opts :: ParserInfo Cmd
opts =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "Deterministic CLI for A2A task delegation"
        <> header "poreus — single binary for agent-to-agent task state"
    )

run :: IO ()
run = execParser opts >>= dispatch

dispatch :: Cmd -> IO ()
dispatch = \case
  CmdInit -> cmdInit
  CmdRegister alias path -> cmdRegister alias path
  CmdPutProfile alias -> cmdPutProfile alias
  CmdInspectRepo mp -> cmdInspectRepo mp
  CmdDiscover tag verb mAgent -> cmdDiscover tag verb mAgent
  CmdMatch verb marg mtag -> cmdMatch verb marg mtag
  CmdSend -> cmdSend
  CmdInbox malias mstatus allFlag k -> cmdInbox malias mstatus allFlag k
  CmdClaim tid -> cmdClaim tid
  CmdComplete tid -> cmdComplete tid
  CmdReject tid reason -> cmdReject tid reason
  CmdStatus mtid sentF recvF -> cmdStatus mtid sentF recvF
  CmdWatchCheck -> cmdWatchCheck
  CmdMigrate -> cmdMigrate

-- ---------------------------------------------------------------------
-- Command implementations
-- ---------------------------------------------------------------------

now :: IO Timestamp
now = Timestamp <$> currentTime

cmdInit :: IO ()
cmdInit = do
  home <- poreusHome
  cwd <- getCurrentDir
  root <- Repo.repoRoot cwd
  alias <- Alias <$> Repo.repoAlias cwd
  ts <- now
  DB.withDB $ \c -> do
    DB.migrate c
    _ <- Profile.registerAgent c alias (T.pack root) ts
    J.emitJSON $
      A.object
        [ "home" A..= home
        , "alias" A..= alias
        , "path" A..= T.pack root
        ]

cmdRegister :: Alias -> Text -> IO ()
cmdRegister alias path = do
  ts <- now
  DB.withDB $ \c -> do
    DB.migrate c
    (reg, upd) <- Profile.registerAgent c alias path ts
    J.emitJSON $
      A.object
        [ "alias" A..= alias
        , "path" A..= path
        , "registered_at" A..= reg
        , "updated_at" A..= upd
        ]

cmdPutProfile :: Alias -> IO ()
cmdPutProfile alias = do
  raw <- BL.getContents
  case A.eitherDecode' raw of
    Left err -> Exit.exitJsonError Exit.ExitBadArgs (T.pack ("invalid profile JSON: " <> err))
    Right (profile :: Profile.ProfileInput) -> do
      ts <- now
      DB.withDB $ \c -> do
        DB.migrate c
        magent <- Endpoint.loadAgent c alias
        case magent of
          Nothing ->
            Exit.exitJsonError
              Exit.ExitNotFound
              ("agent not registered: " <> unAlias alias)
          Just _ -> do
            (n, upd) <- Profile.putProfile c alias profile ts
            J.emitJSON $
              A.object
                [ "alias" A..= alias
                , "endpoints_count" A..= n
                , "updated_at" A..= upd
                ]

cmdInspectRepo :: Maybe FilePath -> IO ()
cmdInspectRepo mp = do
  dir <- maybe getCurrentDir pure mp
  r <- Inspect.inspectRepo dir
  J.emitJSON r

cmdDiscover :: Maybe Text -> Maybe Text -> Maybe Alias -> IO ()
cmdDiscover mtag mverb mAgent = DB.withDB $ \c -> do
  DB.migrate c
  agents <- case mAgent of
    Just a -> maybe [] (: []) <$> Endpoint.loadAgent c a
    Nothing -> Endpoint.loadAllAgents c
  let filtered = filter byVerb . filter byTag $ agents
  J.emitJSON filtered
  where
    byTag a = case mtag of
      Nothing -> True
      Just t -> t `elem` agentTags a
    byVerb a = case mverb of
      Nothing -> True
      Just v -> any (\e -> endpointVerb e == v) (agentEndpoints a)

cmdMatch :: Text -> Maybe Text -> Maybe Text -> IO ()
cmdMatch verb _marg mtag = DB.withDB $ \c -> do
  DB.migrate c
  agents <- Endpoint.agentsMatchingVerb c verb
  let filtered = case mtag of
        Nothing -> agents
        Just t -> filter (\a -> t `elem` agentTags a) agents
      candidates =
        [ A.object
            [ "alias" A..= agentAlias a
            , "endpoint" A..= e
            , "summary" A..= agentSummary a
            , "tags" A..= agentTags a
            ]
        | a <- filtered
        , e <- Endpoint.matchEndpoints a verb
        ]
  J.emitJSON candidates

cmdSend :: IO ()
cmdSend = do
  raw <- BL.getContents
  case A.eitherDecode' raw of
    Left err -> Exit.exitJsonError Exit.ExitBadArgs (T.pack ("invalid send JSON: " <> err))
    Right (input :: Task.SendInput) -> do
      ts <- now
      hex <- randomHex4
      cwd <- getCurrentDir
      root <- Repo.repoRoot cwd
      from <- Alias <$> Repo.repoAlias cwd
      let tid = Task.newTaskId from ts hex
      DB.withDB $ \c -> do
        DB.migrate c
        _ <- Profile.registerAgent c from (T.pack root) ts
        t <- Task.sendTask c from tid ts input
        J.emitJSON t

cmdInbox :: Maybe Alias -> Maybe TaskStatus -> Bool -> InboxKind -> IO ()
cmdInbox malias mstatus allFlag kind = DB.withDB $ \c -> do
  DB.migrate c
  alias <- case malias of
    Just a -> pure a
    Nothing -> Alias <$> Repo.cwdAlias
  case kind of
    InboxKRequest -> do
      let status = if allFlag then Nothing else Just (maybe TSPending id mstatus)
      ts <- Task.inboxTasks c alias status
      J.emitJSON ts
    InboxKReply -> do
      ms <- Msg.messagesToKind c alias Msg.MKReply
      J.emitJSON ms
    InboxKAll -> do
      ms <- Msg.messagesTo c alias
      J.emitJSON ms

cmdClaim :: TaskId -> IO ()
cmdClaim tid = do
  ts <- now
  DB.withDB $ \c -> do
    DB.migrate c
    res <- Task.claimTask c tid ts
    case res of
      Left err -> reportTaskErr err
      Right t -> J.emitJSON t

cmdComplete :: TaskId -> IO ()
cmdComplete tid = do
  raw <- BL.getContents
  case A.eitherDecode' raw of
    Left err -> Exit.exitJsonError Exit.ExitBadArgs (T.pack ("invalid result JSON: " <> err))
    Right (input :: Task.CompleteInput) -> do
      ts <- now
      hex <- randomHex4
      cwd <- getCurrentDir
      replyFrom <- Alias <$> Repo.repoAlias cwd
      let replyId = Task.newTaskId replyFrom ts hex
      DB.withDB $ \c -> do
        DB.migrate c
        res <- Task.completeTask c tid replyId ts input
        case res of
          Left err -> reportTaskErr err
          Right (t, r) ->
            J.emitJSON $
              A.object ["task" A..= t, "result" A..= r]

cmdReject :: TaskId -> Text -> IO ()
cmdReject tid reason = do
  ts <- now
  hex <- randomHex4
  cwd <- getCurrentDir
  replyFrom <- Alias <$> Repo.repoAlias cwd
  let replyId = Task.newTaskId replyFrom ts hex
  DB.withDB $ \c -> do
    DB.migrate c
    res <- Task.rejectTask c tid replyId ts reason
    case res of
      Left err -> reportTaskErr err
      Right (t, r) ->
        J.emitJSON $
          A.object ["task" A..= t, "result" A..= r]

reportTaskErr :: Task.TaskErr -> IO a
reportTaskErr = \case
  Task.TaskNotFound msg -> Exit.exitJsonError Exit.ExitNotFound msg
  Task.TaskBadTransition msg -> Exit.exitJsonError Exit.ExitBadTransition msg

cmdStatus :: Maybe TaskId -> Bool -> Bool -> IO ()
cmdStatus mtid sentF recvF = DB.withDB $ \c -> do
  DB.migrate c
  case mtid of
    Just tid -> do
      mt <- Task.loadTask c tid
      case mt of
        Nothing ->
          Exit.exitJsonError Exit.ExitNotFound ("task not found: " <> unTaskId tid)
        Just t -> do
          r <- Task.loadResult c tid
          J.emitJSON $
            A.object
              [ "task" A..= t
              , "result" A..= r
              ]
    Nothing -> do
      alias <- Alias <$> Repo.cwdAlias
      ts <-
        if sentF
          then Task.sentTasks c alias
          else
            if recvF
              then Task.inboxTasks c alias Nothing
              else Task.allTasksForAlias c alias
      J.emitJSON ts

cmdWatchCheck :: IO ()
cmdWatchCheck = do
  alias <- Alias <$> Repo.cwdAlias
  ts <- now
  DB.withDB $ \c -> do
    DB.migrate c
    msgs <- Watch.watchCheck c alias ts
    J.emitJSON msgs

cmdMigrate :: IO ()
cmdMigrate = do
  t <- currentTime
  DB.withDB $ \c -> do
    DB.migrate c
    stats <- Migrate.migrateFromLegacy c t
    J.emitJSON stats
