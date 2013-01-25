{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, GADTs, EmptyDataDecls, FlexibleContexts, FlexibleInstances, OverloadedStrings #-}

module Main where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Database.Persist.EntityDef
import Database.Persist.Store
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Data.Fixed
import qualified Data.Map as M
import qualified Data.Text as T

instance (HasResolution a) => PersistField (Fixed a) where
  toPersistValue a = PersistText $ T.pack $ show a
  fromPersistValue (PersistDouble d) = Right $ fromRational $ toRational d
  fromPersistValue (PersistText d) = case reads dpt of
    [(a, "")] -> Right a
    _         -> Left $ T.pack $ "Could not read value " ++ dpt ++ " as fixed value"
    where dpt = T.unpack d

  fromPersistValue a = Left $ T.append "Unexpected data value can not be converted to Fixed: " $ T.pack $ show a

  sqlType a = SqlOther $ T.pack $ "NUMERIC(" ++ (show l) ++ "," ++ (show p) ++ ")"
    where
      p = round $ (log $ fromIntegral $ resolution a) / (log 10)
      l = p + 15                --  FIXME: this is maybe not very good
  isNullable _ = False

share [mkPersist sqlSettings, mkMigrate "migrateName"] [persist|
Name
  name String
  deriving Show Eq
Comment
  nameId NameId
  comment String
  deriving Show Eq
Thing
  money Micro
  deriving Show Eq
|]

main = do
  runResourceT $ withPostgresqlConn "host=127.0.0.1 user=test password=test dbname=test" $ \conn -> runSqlConn action conn


  where
    action = do
      runMigration migrateName
      let ins = Thing 24.2424
      x <- insert ins
      mx <- selectFirst [ThingId ==. x] []
      liftIO $ case mx of
        Nothing -> print "nothing here"
        Just (Entity {entityVal = v}) -> do
          print $ v == ins
          print v