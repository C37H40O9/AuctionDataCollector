{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB (initMigrations, writeBoxInTBuyout , writeBoxInDB, updLastModified)
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
import Data.ByteString (ByteString)





initMigrations :: Connection -> IO ()
initMigrations conn = do    
    res<- runMigrations False conn [MigrationInitialization, MigrationDirectory "migrations"]
    case res of
        MigrationSuccess -> pure ()
        MigrationError reason -> print reason

hSqlErr ::(Show a) => a -> IO Int64
hSqlErr e = print e *> pure 0

uniqueViolation :: ByteString
uniqueViolation = "23505"

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
writeBoxInDB date slug' kv connPool' = do 
    iBout <- withResource connPool' eManyBuyout `catches` sqlHandlers
    iBid <- withResource connPool' eManyBid `catches` sqlHandlers
    pure (iBout + iBid)
        where qBuyout = "insert into buyout (buyout_date, server_slug, item_id, item_count, min_w, bot_w, p_25, p_50, p_75, top_w, max_w) values (?,?,?,?,?,?,?,?,?,?,?)"
              dBuyout = map (\ (i,b) -> (date, slug', i) :. fromJust (bbuyout b)) kv
              qBid = "insert into bid (bid_date, server_slug, item_id, item_count, min_w, bot_w, p_25, p_50, p_75, top_w, max_w) values (?,?,?,?,?,?,?,?,?,?,?)"
              dBid = map (\ (i,b) -> (date, slug', i) :. fromJust (bbid b)) kv
              eManyBuyout conn = executeMany conn qBuyout dBuyout
              eManyBid conn = executeMany conn qBid dBid

updLastModified :: UTCTime -> Slug -> Pool Connection -> IO Int64
updLastModified date slug' connPool' = withResource connPool' execute' `catches` sqlHandlers
        where q = "update last_modified set upd_date = ? where server_slug = ?"
              qdata = (date, slug')
              execute' c = execute c q qdata

