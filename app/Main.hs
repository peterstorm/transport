{-# LANGUAGE 
   DeriveGeneric, GADTs, OverloadedStrings, FlexibleContexts, FlexibleInstances, TypeFamilies, TypeApplications, StandaloneDeriving, TypeSynonymInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Main where

import Control.Lens
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Data.Text (Text)
import Control.Monad.Reader
import Data.Foldable

-- | create datatype for a path, and make it usable by Beam
data PathT f = Path
           { _pathPathName :: C f Text
           , _pathPathDir  :: C f Text
           } deriving Generic

makeLenses ''PathT

type Path = PathT Identity
type PathId = PrimaryKey PathT Identity

deriving instance Show Path
deriving instance Eq Path

instance Beamable PathT
instance Table PathT where
  data PrimaryKey PathT f = PathId (C f Text) deriving Generic
  primaryKey = PathId . _pathPathName

instance Beamable (PrimaryKey PathT)

data TransportDb f = TransportDb
                   { _transportPathList         :: f (TableEntity PathT) }
                   deriving Generic

makeLenses ''TransportDb

instance Database be TransportDb

transportDb :: DatabaseSettings be TransportDb
transportDb = defaultDbSettings





data DbConfig = DbConfig
              { _dbConf :: Connection }

makeClassy ''DbConfig

getConnection :: MonadIO m => m DbConfig
getConnection = liftIO $ fmap DbConfig $ open ".transport.db"


selectAllPathsDebug :: (MonadReader r m, MonadIO m, HasDbConfig r) => m ()
selectAllPathsDebug = do
  conf <- view dbConf
  liftIO . runBeamSqliteDebug putStrLn conf $ do
    paths <- runSelectReturningList $ select (all_ (transportDb ^. transportPathList))
    traverse_ (liftIO . putStrLn . show) paths

selectAllPaths :: (MonadReader r m, MonadIO m, HasDbConfig r) => m [Path]
selectAllPaths = do
  conf <- view dbConf
  liftIO . runBeamSqlite conf $ do
    paths <- runSelectReturningList $ select (all_ (transportDb ^. transportPathList))
    pure paths

selectWhereDebug :: (MonadReader r m, MonadIO m, HasDbConfig r) => Text -> m ()
selectWhereDebug name = do
  conf <- view dbConf
  liftIO . runBeamSqliteDebug putStrLn conf $ do
    user <- runSelectReturningOne $ lookup_ (transportDb ^. transportPathList) (PathId name)
    case user of
      Nothing -> error "error"
      Just x  -> (liftIO . putStrLn . show) x 

selectWhere :: (MonadReader r m, MonadIO m, HasDbConfig r) => Text -> m (Maybe Path)
selectWhere name = do
  conf <- view dbConf
  liftIO . runBeamSqlite conf $ do
    user <- runSelectReturningOne $ lookup_ (transportDb ^. transportPathList) (PathId name)
    pure user

insertTestPaths :: (MonadReader r m, MonadIO m, HasDbConfig r) => m ()
insertTestPaths = do
  conf <- view dbConf
  liftIO . runBeamSqliteDebug putStrLn conf $ runInsert $
    insert (transportDb ^. transportPathList) $
      insertValues [ Path "peter" "~/peter"
                   , Path "johanna" "~/"]

insertPath :: (MonadReader r m, MonadIO m, HasDbConfig r) => Text -> Text -> m ()
insertPath name dir = do
  conf <- view dbConf
  liftIO . runBeamSqlite conf $ runInsert $
    insert (transportDb ^. transportPathList) $
      insertValues [ Path name dir ]

deletePathDebug :: (MonadReader r m, MonadIO m, HasDbConfig r) => Text -> m ()
deletePathDebug name = do
  conf <- view dbConf
  liftIO . runBeamSqlite conf $ runDelete $
    delete (transportDb ^. transportPathList) (\path -> path ^. pathPathName ==. (val_ name))


insertPathIO :: DbConfig -> Text -> Text -> IO ()
insertPathIO conn name dir = runReaderT (insertPath name dir) conn

insertPathsIO :: DbConfig -> IO ()
insertPathsIO = runReaderT insertTestPaths

selectWhereDebugIO :: DbConfig -> Text -> IO ()
selectWhereDebugIO conn name = runReaderT (selectWhereDebug name) conn

selectAllPathsIO :: DbConfig -> IO [Path]
selectAllPathsIO = runReaderT selectAllPaths

selectAllPathsDebugIO :: DbConfig -> IO ()
selectAllPathsDebugIO = runReaderT selectAllPathsDebug

main :: IO ()
main = do
  c <- getConnection
  --insertPathIO c "johanna" "~/"
  runReaderT selectAllPathsDebug c
  selectWhereDebugIO c "johanna"
