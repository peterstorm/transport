module Main where

import System.FilePath
import System.Directory
import Options.Applicative
import Data.List (transpose, intercalate)
import Data.Foldable
import Control.Lens hiding (argument)
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple hiding (withConnection)
import Database.SQLite.SimpleErrors.Types (SQLiteResponse (..), Constraint (..))
import Database.SQLite.SimpleErrors
import Data.Text (Text, pack, unpack)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Error.Hoist
import Control.Monad.Error.Lens
import Control.Error
import Control.Exception
import Control.Exception.Lens hiding (throwing)
import qualified Turtle as T

import Util

-- | Create datatype for the path we want to save in the database. When we save a path we give it a name, and a directory
-- | Beam also uses this datatype to talk to the database. The naming convention is, as far as I can see that it converts
-- | the camelCase to column names, by taking the capitalized parts and doing toLower and then adding an underscore.
-- | So 'pathPathName' becomes 'path_name' in the database.
data PathT f = Path
           { _pathPathName :: C f Text
           , _pathPathDir  :: C f Text
           } deriving Generic

makeLenses ''PathT

-- | Here we create some type synonyms so we can use 'Path' in our regular Haskell program.
-- | We also create a 'PathId' we can use in our regular program as the primary key in the table.
type Path = PathT Identity
type PathId = PrimaryKey PathT Identity

deriving instance Show Path
deriving instance Eq Path

-- | We have have an instane of 'Beamable' to be able to talk to the database provided.
-- | We also define the primary key here.
instance Beamable PathT
instance Table PathT where
  data PrimaryKey PathT f = PathId (C f Text) deriving Generic
  primaryKey = PathId . _pathPathName

instance Beamable (PrimaryKey PathT)

-- | We define the table in the database, in terms of the 'PathT' datatype, that will make up the columns.
data TransportDb f = TransportDb
                   { _transportPathList         :: f (TableEntity PathT) }
                   deriving Generic

makeLenses ''TransportDb

instance Database be TransportDb

-- | This is a way to initiate the database, as far as I understand.
transportDb :: DatabaseSettings be TransportDb
transportDb = defaultDbSettings

-- | We create the DbConfig datatype, to be able to hold a connection and the path to the db.
data DbConfig = DbConfig
              { _dbCon :: Connection 
              , _dbPath :: FilePath
              } 

makeClassy ''DbConfig

data DbError = DbErrorCode SQLiteResponse | UnknownError
  deriving Show

makeClassyPrisms ''DbError

data AppError = AppDbError DbError 
              | FilePathNotFound FilePath
              | PathNotFound Text
  deriving Show

makeClassyPrisms ''AppError

instance AsDbError AppError where
  _DbError      = _AppDbError . _DbError
  _UnknownError = _AppDbError . _UnknownError


-- | PARSER STUFF

data Command = CommandList
             | CommandAdd { addName :: Text
                          , addPath :: Maybe FilePath }
             | CommandRemove { removeName :: Text }
             | CommandTP { tpName :: Text }
             deriving Show

transportDesc :: String
transportDesc = "use transport to setup transport paths, and move to them at will"


transportHeader :: String
transportHeader = "transport: move quickly around your filesystem"

parseCommand :: Parser Command
parseCommand = subparser $
  (command "add"    (info (helper <*> parseAddCommand)    (fullDesc <> progDesc "add a transport path"))) <>
  (command "list"   (info (helper <*> parseListCommand)   (fullDesc <> progDesc "list all transport paths"))) <>
  (command "remove" (info (helper <*> parseRemoveCommand) (fullDesc <> progDesc "remove a transport path"))) <>
  (command "tp"     (info (helper <*> parseTPCommand)     (fullDesc <> progDesc "transport to existing path")))

parseListCommand :: Parser Command
parseListCommand = pure CommandList

parseAddCommand :: Parser Command
parseAddCommand = CommandAdd <$> nameParser <*> optional pathParser

parseRemoveCommand :: Parser Command
parseRemoveCommand = CommandRemove <$> nameParser

parseTPCommand :: Parser Command
parseTPCommand = CommandTP <$> nameParser

nameParser :: Parser Text
nameParser = argument str (metavar "NAME" <> help "name of the transport path you want to save")

pathParser :: Parser FilePath
pathParser = argument str (metavar "PATH" <> help ("path you want to save. " <>
                                                                                    "default: current working dir"))

-- | DATABASE STUFF

initDb :: Script DbConfig
initDb = do
  path <- getDbPath
  conn <- scriptIO $ open path
  scriptIO $ execute_ (conn) "CREATE TABLE IF NOT EXISTS path_list (path_name VARCHAR NOT NULL, path_dir VARCHAR NOT NULL, PRIMARY KEY( path_name ))"
  pure $ DbConfig conn path

getDbPath :: Script FilePath
getDbPath = do
  home <- scriptIO $ getHomeDirectory
  pure $ (home </> (".transport.db" :: FilePath))

withConnection :: (MonadError e m, MonadReader r m, MonadIO m, HasDbConfig r, AsDbError e) => (Connection -> IO (DatabaseResponse a)) -> m a
withConnection f = do
  c <- view dbCon
  (liftIO  (f c)) >>= rethrowDbError


selectAllPathsDebug :: (MonadError e m, MonadReader r m, MonadIO m, HasDbConfig r, AsDbError e) => m ()
selectAllPathsDebug = withConnection (\c -> 
  runDBAction . runBeamSqliteDebug putStrLn c $ do
    paths <- runSelectReturningList $ select (all_ (transportDb ^. transportPathList))
    traverse_ (liftIO . putStrLn . show) paths)


selectAllPaths :: (MonadError e m, MonadReader r m, MonadIO m, HasDbConfig r, AsDbError e) => m [Path]
selectAllPaths = withConnection (\c ->
  runDBAction . runBeamSqlite c $ do
    paths <- runSelectReturningList $ select (all_ (transportDb ^. transportPathList))
    pure paths)

selectWhereDebug :: (MonadError e m, MonadReader r m, MonadIO m, HasDbConfig r, AsDbError e) => Text -> m ()
selectWhereDebug name = withConnection (\c -> 
  runDBAction . runBeamSqliteDebug putStrLn c $ do
    user <- runSelectReturningOne $ lookup_ (transportDb ^. transportPathList) (PathId name)
    case user of
      Nothing -> liftIO . putStrLn $ "no such user"
      Just x  -> (liftIO . putStrLn . show) x)

selectWhere :: (MonadError e m, MonadReader r m, MonadIO m, HasDbConfig r, AsDbError e) => Text -> m (Maybe Path)
selectWhere name = withConnection (\c ->
  runDBAction . runBeamSqlite c $ do
    user <- runSelectReturningOne $ lookup_ (transportDb ^. transportPathList) (PathId name)
    pure user)

insertTestPaths :: (MonadError e m, MonadReader r m, MonadIO m, HasDbConfig r, AsDbError e) => m ()
insertTestPaths = withConnection (\c ->
  runDBAction . runBeamSqliteDebug putStrLn c $ runInsert $
    insert (transportDb ^. transportPathList) $
      insertValues [ Path "peter" "~/peter"
                   , Path "johanna" "~/"])

insertPath :: (MonadError e m, MonadReader r m, MonadIO m, HasDbConfig r, AsDbError e) => Text -> Text -> m ()
insertPath name dir = withConnection (\c ->
  runDBAction . runBeamSqlite c $ runInsert $
    insert (transportDb ^. transportPathList) $
      insertValues [ Path name dir ])

removePath :: (MonadError e m, MonadReader r m, MonadIO m, HasDbConfig r, AsDbError e) => Text -> m ()
removePath name = withConnection (\c ->
  runDBAction . runBeamSqlite c $ runDelete $
    delete (transportDb ^. transportPathList) (\path -> path ^. pathPathName ==. (val_ name)))

rethrowDbError :: (MonadError e m, AsDbError e) => DatabaseResponse a -> m a
rethrowDbError x = case x of
                     Left x -> throwError $ _DbErrorCode # x
                     Right x -> pure x 

handleDbError :: SQLiteResponse -> String
handleDbError (SQLConstraintError Unique _) = "There is already a path with that name" 
handleDbError x                             = show x


-- | APP STUFF

runList :: (MonadError e m, MonadReader r m, MonadIO m, HasDbConfig r, AsDbError e) => m ()
runList = do
  paths <- selectAllPaths
  liftIO $ putStrLn $ showTable [ ColDesc center "name" left (unpack . _pathPathName)
                                , ColDesc center "path" left (unpack . _pathPathDir) ]
                                paths

runAdd :: (MonadError e m, MonadReader r m, MonadIO m, HasDbConfig r, AsDbError e, AsAppError e) => Text -> Maybe FilePath -> m ()
runAdd name path = do
  case path of
    Nothing -> do
      pwd <- liftIO $ getCurrentDirectory
      insertPath name (pack pwd) >> liftIO (putStrLn $ show pwd <> " added!\n")
    Just path' -> do
      valid <- liftIO $ doesDirectoryExist path'
      if valid
         then insertPath name (pack path') >> liftIO (putStrLn $ show path' <> " added!\n")
         else throwing _FilePathNotFound path'

runRemove :: (MonadError e m, MonadReader r m, MonadIO m, HasDbConfig r, AsDbError e, AsAppError e) => Text -> m ()
runRemove path = do
  path' <- selectWhere path
  case path' of
    Nothing -> throwing _PathNotFound path
    Just x  -> removePath (x ^. pathPathName) >> liftIO (putStrLn $ show (x ^. pathPathName) <> " removed!\n")

runCommand :: (MonadError e m, MonadReader r m, MonadIO m, HasDbConfig r, AsDbError e, AsAppError e) => Command -> m ()
runCommand c = case c of
                CommandList       -> runList
                CommandAdd{..}    -> runAdd (addName) (addPath) 
                CommandRemove{..} -> runRemove removeName
                CommandTP{..}     -> runTP tpName

runTP :: (MonadError e m, MonadReader r m, MonadIO m, HasDbConfig r, AsDbError e, AsAppError e) => Text -> m ()
runTP path = do
  path' <- selectWhere path
  case path' of
    Nothing -> throwing _PathNotFound path
    Just x  -> do
      let dir = T.textToLine (x ^. pathPathDir)
      case dir of
        Just dir' -> do 
          T.echo dir'
          T.exit (T.ExitFailure 2)
        Nothing   -> pure ()


runApp :: DbConfig -> ExceptT AppError (ReaderT DbConfig IO) a -> IO (Either AppError a)
runApp config = flip runReaderT config . runExceptT

appErrorString :: AppError -> String
appErrorString (AppDbError db)      = dbErrorString db
appErrorString (FilePathNotFound e) = "Filepath " <> show e <> " not found!\n" 
appErrorString (PathNotFound e)     = "Path " <> show e <> " not found!\n"

dbErrorString :: DbError -> String
dbErrorString (DbErrorCode c) = "There was a problem with the command: " <> (handleDbError c)

main :: IO ()
main = do
  c <- runScript $ initDb
  command <- execParser (info (helper <*> parseCommand) (fullDesc <> progDesc transportDesc <> header transportHeader))
  e <- runApp c $ runCommand command
  case e of
    Left x -> putStrLn $ appErrorString x 
    Right _ -> pure ()
