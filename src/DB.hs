{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module DB 
    where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Migration
import Types.Types
import Types.Locale
import Data.Monoid ((<>))
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Format
import qualified Data.ByteString.Char8 as B



--millisToUTC :: Integer -> UTCTime
--millisToUTC t = posixSecondsToUTCTime $ fromInteger t / 1000


initMigrations :: Connection -> IO ()
initMigrations conn = do    
    res<- runMigrations False conn [MigrationInitialization, MigrationDirectory "migrations"]
    case res of
        MigrationSuccess -> return ()
        MigrationError reason -> print reason
{-
writeBoxInDB :: String -> Integer -> Slug -> Int -> WBox -> Connection -> IO Int
writeBoxInDB table date slug iid box conn = execute conn q qdata
        where qdata =  [date',  slug , iid' ] ++ box'
              box' = map show $ [ic, minW, botW, p25, p50, p75, topW, maxW] <*> pure box
              date' = show (millisToUTC date)
              iid' = show iid
              q :: Query
              s :: Query
              t :: Query
              t = Query {fromQuery = table}
              s = "insert into " `mappend` t `mappend` " (bid_date, server_slug, item_id, item_count, min_w, bot_w, p_25, p_50, p_75, top_w, max_w) values (?,?,?,?,?,?,?,?,?,?,?)"
              q = s
-}
{-
wowadp_db
wow_adp@pass
-}