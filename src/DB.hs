{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB (initMigrations, writeBoxInTBid)
    where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Migration
import Types.Types
import Data.Monoid ((<>))
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Format
import Data.Int (Int64)
import Data.Pool
import Control.Exception





initMigrations :: Connection -> IO ()
initMigrations conn = do    
    res<- runMigrations False conn [MigrationInitialization, MigrationDirectory "migrations"]
    case res of
        MigrationSuccess -> return ()
        MigrationError reason -> print reason

writeBoxInTBid :: UTCTime -> Slug -> Int -> WBox -> Pool Connection -> IO Int64
writeBoxInTBid date slug' iid' box connPool' = withResource connPool' execute' `catches` [Handler (\ (ex::SqlError) -> hSqlErr ex)    ,
                                                                                          Handler (\ (ex::ResultError) -> hSqlErr ex) ,
                                                                                          Handler (\ (ex::QueryError) -> hSqlErr ex)  ,
                                                                                          Handler (\ (ex::FormatError) -> hSqlErr ex) ]
        where qdata =  (date,  slug' , iid') :. box
              q = "insert into bid (bid_date, server_slug, item_id, item_count, bot_w, p_25, p_50, p_75, top_w) values (?,?,?,?,?,?,?,?,?)"
              execute' conn = execute conn q qdata
              hSqlErr e = do
                print e
                return 0 