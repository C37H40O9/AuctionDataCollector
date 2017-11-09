{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module DB 
    where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Migration
import Types.Types
import Data.Monoid ((<>))
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Format
import Data.Int (Int64)



--millisToUTC :: Integer -> UTCTime
--millisToUTC t = posixSecondsToUTCTime $ fromInteger t / 1000


initMigrations :: Connection -> IO ()
initMigrations conn = do    
    res<- runMigrations False conn [MigrationInitialization, MigrationDirectory "migrations"]
    case res of
        MigrationSuccess -> return ()
        MigrationError reason -> print reason

writeBoxInTBid :: Integer -> Slug -> Int -> WBox -> Connection -> IO Int64
writeBoxInTBid date slug' iid' box conn = execute conn q qdata
        where qdata =  (date,  slug' , iid' , box)
              q = "insert into bid (bid_date, server_slug, item_id, item_count, min_w, bot_w, p_25, p_50, p_75, top_w, max_w) values (?,?,?,?,?,?,?,?,?,?,?)"
              

{-
wowadp_db
wow_adp@pass
-}