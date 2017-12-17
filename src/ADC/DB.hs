{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE ExistentialQuantification #-}

module ADC.DB
          (initMigrations
          ,writeBoxInDB
          ,updLastModified
          ,readLastModified
          ,trackingItems)
  where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Migration
import ADC.Types.Types
import Data.Monoid ((<>))
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Format
import Data.Int (Int64)
import Data.Pool
import Control.Exception
import Data.Maybe (fromJust)
import Data.ByteString (ByteString)
import Data.Functor (($>))





initMigrations :: Connection -> IO ()
initMigrations conn = do    
  res<- runMigrations False conn [MigrationInitialization, MigrationDirectory "migrations"]
  case res of
    MigrationSuccess -> pure ()
    MigrationError reason -> print reason

hSqlWErr' ::(Show a) => a -> IO Int64
hSqlWErr' e = print e $> 0

hSqlRErr' ::(Show a) => a -> IO [(Slug, UTCTime)]
hSqlRErr' e = print e $> []

--uniqueViolation :: ByteString
--uniqueViolation = "23505"

hSqlWErr :: [Handler Int64]
hSqlWErr =  mkSqlEHandlers hSqlWErr'

hSqlRErr :: [Handler [(Slug, UTCTime)]]
hSqlRErr = mkSqlEHandlers hSqlRErr'

hSE' ::(Show a) => a -> IO [Only Int]
hSE' e = print e $> []


mkSqlEHandlers :: (SqlE -> IO b) -> [Handler b]
mkSqlEHandlers h = [Handler (\(ex::SqlE) -> h ex)]

hSE :: [Handler [Only Int]]
hSE = mkSqlEHandlers hSE'

writeBoxInDB :: UTCTime -> Slug -> [(Int, WBoxedStats)] -> Pool Connection -> IO Int64
writeBoxInDB date slug' kv connPool' = do 
  iBout <- withResource connPool' eManyBuyout `catches` hSqlWErr
  iBid <- withResource connPool' eManyBid `catches` hSqlWErr
  pure (iBout + iBid)
    where 
      qBuyout =
        "insert into buyout \
        \(buyout_date, server_slug, item_id, item_count, min_w, bot_w, p_25, p_50, p_75, top_w, max_w) \
        \values (?,?,?,?,?,?,?,?,?,?,?)"
      dBuyout = map (\ (i,b) -> (date, slug', i) :. fromJust (bbuyout b)) kv
      qBid =
        "insert into bid \
        \(bid_date, server_slug, item_id, item_count, min_w, bot_w, p_25, p_50, p_75, top_w, max_w) \
        \values (?,?,?,?,?,?,?,?,?,?,?)"
      dBid = map (\ (i,b) -> (date, slug', i) :. fromJust (bbid b)) kv
      eManyBuyout conn = executeMany conn qBuyout dBuyout
      eManyBid conn = executeMany conn qBid dBid

updLastModified :: UTCTime -> Slug -> Pool Connection -> IO Int64
updLastModified date slug' connPool' =  do 
  u <- withResource connPool' executeUp `catches` hSqlWErr
  if u == 0 then withResource connPool' executeIns `catches` hSqlWErr
  else pure u
    where
      qUp = "update last_modified set upd_date = ? where server_slug = ?"
      qIns = "insert into last_modified (upd_date, server_slug) values (?,?)"
      qdata = (date, slug')
      executeUp c = execute c qUp qdata
      executeIns c = execute c qIns qdata

readLastModified :: Pool Connection -> IO [(Slug, UTCTime)]
readLastModified c = withResource c q `catches` hSqlRErr
  where q conn = query_ conn "select server_slug, upd_date from last_modified"

trackingItems' :: Pool Connection -> IO [Only Int]
trackingItems' c = withResource c q `catches` hSE
  where q conn = query_ conn "select item_id from items"

trackingItems :: Pool Connection -> IO TrackingItems
trackingItems c = do
  oi <- trackingItems' c
  pure $ fromOnly <$> oi