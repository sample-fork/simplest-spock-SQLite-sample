{-# LANGUAGE OverloadedStrings          #-}

-- this pragmas related to persist-TH package
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

-- this pragmas related to persistent package
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- this pragmas for deriving Generic used by Aeson
{-# LANGUAGE DeriveGeneric              #-}

module Main where

import qualified Data.Text                   as T
import           Data.Time                   (UTCTime, addUTCTime)
import           Data.Time.Clock             (getCurrentTime)

import           Data.Monoid

import           Database.Persist.Sql        (SqlBackend, runMigrationSilent, toSqlKey)

import           Database.Persist
import qualified Database.Persist.Sqlite     as S
import           Database.Persist.TH         (mkMigrate, mkPersist,
                                             persistLowerCase, share,
                                             sqlSettings)

import qualified Web.Spock.Safe             as W
import           Data.Aeson                 (ToJSON)
import           GHC.Generics               (Generic)

-- needed for spock monad
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Logger
import           Control.Monad.IO.Class      (liftIO, MonadIO)

data Status = Status {
      code     :: Int
      , msg    :: T.Text
} deriving (Show, Generic)

instance ToJSON Status

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Gawe
     desc           T.Text
     urgensi        Int      default=1 sqltype=int
     added_at       UTCTime  default=CURRENT_TIMESTAMP
     deriving Show Eq
|]

-- wrapper for SQLite to Spock
runSQL :: S.SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runSQL = runNoLoggingT . runResourceT . S.withSqliteConn "gaweku.sqlite3" . S.runSqlConn

insertGawe :: MonadIO m => T.Text -> W.ActionCtxT () m ()
insertGawe g = do
   now   <- liftIO getCurrentTime
   sId   <- liftIO $ runSQL $ S.insert $ Gawe g 0 now
   W.json $ Status 200 "Data has been successfully added"

deleteGawe :: MonadIO m => Integer -> W.ActionCtxT () m ()
deleteGawe i = do
   liftIO $ runSQL $ S.delete (toSqlKey $ fromIntegral i::GaweId)
   W.json $ Status 200 "Data has been successfully deleted"

runApp :: W.SpockT IO ()
runApp = do
   W.get W.root $ W.text "Hello World!"
   W.get ("i" W.<//> W.var) $ \name -> insertGawe name
   W.get ("d" W.<//> W.var) $ \gId  -> deleteGawe gId

main :: IO ()
main = do
   runSQL $ runMigrationSilent migrateAll
   liftIO $ W.runSpock 80 $ W.spockT id runApp
