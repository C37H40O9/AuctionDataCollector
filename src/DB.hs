{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

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



--millisToUTC :: Integer -> UTCTime
--millisToUTC t = posixSecondsToUTCTime $ fromInteger t / 1000


initMigrations :: Connection -> IO ()
initMigrations conn = do    
    res<- runMigrations False conn [MigrationInitialization, MigrationDirectory "migrations"]
    case res of
        MigrationSuccess -> return ()
        MigrationError reason -> print reason

writeBoxInTBid :: UTCTime -> Slug -> Int -> WBox -> Pool Connection -> IO Int64
writeBoxInTBid date slug' iid' box connPool = do withResource connPool execute'
        where qdata =  (date,  slug' , iid' , ic box, botW box, p25 box, p50 box, p75 box, topW box)
              q = "insert into bid (bid_date, server_slug, item_id, item_count, bot_w, p_25, p_50, p_75, top_w) values (?,?,?,?,?,?,?,?,?)"
              execute' conn = execute conn q qdata 
        --TODO withResource do bla bla bla      

{-
wowadp_db
wow_adp@pass
-}