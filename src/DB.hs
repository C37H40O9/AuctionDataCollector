{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB (initMigrations, writeBoxInTBuyout , writeBoxInDB)
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
import Data.Maybe (fromJust)





initMigrations :: Connection -> IO ()
initMigrations conn = do    
    res<- runMigrations False conn [MigrationInitialization, MigrationDirectory "migrations"]
    case res of
        MigrationSuccess -> pure ()
        MigrationError reason -> print reason

hSqlErr ::(Show a) => a -> IO Int64
hSqlErr e = print e >> pure 0

sqlHandlers :: [Handler Int64]
sqlHandlers = [Handler (\ (ex::SqlError) -> hSqlErr ex)    ,
               Handler (\ (ex::ResultError) -> hSqlErr ex) ,
               Handler (\ (ex::QueryError) -> hSqlErr ex)  ,
               Handler (\ (ex::FormatError) -> hSqlErr ex) ]

writeBoxInTBuyout :: UTCTime -> Slug -> Int -> WBox -> Pool Connection -> IO Int64
writeBoxInTBuyout date slug' iid' box connPool' = withResource connPool' execute' `catches` sqlHandlers
        where qdata =  (date,  slug' , iid') :. box
              q = "insert into buyout (buyout_date, server_slug, item_id, item_count, bot_w, p_25, p_50, p_75, top_w) values (?,?,?,?,?,?,?,?,?)"
              execute' conn = execute conn q qdata

writeBoxInDB :: UTCTime -> Slug -> [(Int, WBoxedStats)] -> Pool Connection -> IO Int64
writeBoxInDB date slug' kv connPool' = withResource connPool' executeMany' `catches` sqlHandlers
        where q = "insert into buyout (buyout_date, server_slug, item_id, item_count, min_w, bot_w, p_25, p_50, p_75, top_w, max_w) values (?,?,?,?,?,?,?,?,?,?,?)"
              qdata = map (\ (i,b) -> (date, slug', i) :. fromJust (bbuyout b)) kv
              executeMany' conn = executeMany conn q qdata
