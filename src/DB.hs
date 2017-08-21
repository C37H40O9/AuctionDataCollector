{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module DB 
    where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Migration

-- TODO read all configuration and credentials from file
connInfo = ConnectInfo {
    connectHost = "127.0.0.1"
  , connectPort = 5432
  , connectUser = "wow_adp"
  , connectPassword = "pass"
  , connectDatabase = "wowadp_db"
  }

--conn = connect connInfo
initMigrations = do
    conn <- connect connInfo
    res<- runMigrations False conn [MigrationInitialization, MigrationDirectory "migrations"]
    case res of
        MigrationSuccess -> return ()
        MigrationError reason -> print reason
initTables = undefined


{-
wowadp_db
wow_adp@pass
-}