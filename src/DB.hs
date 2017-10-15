{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module DB 
    where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Migration






initMigrations :: Connection -> IO ()
initMigrations conn = do    
    res<- runMigrations False conn [MigrationInitialization, MigrationDirectory "migrations"]
    case res of
        MigrationSuccess -> return ()
        MigrationError reason -> print reason



{-
wowadp_db
wow_adp@pass
-}