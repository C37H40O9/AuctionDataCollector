{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
--{-# LANGUAGE TransformListComp #-}
--{-# LANGUAGE OverloadedRecordFields #-}

module Lib where
import Types.Types
import Types.Locale
import DB
import Config
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid ((<>))
import Data.List (foldl')
import Network.HTTP.Client ( setRequestIgnoreStatus)
import qualified Network.HTTP.Conduit as C
import Control.Monad.Trans.Resource
import Control.Monad (forever, unless)
import Control.Concurrent.MVar
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.Async
import Data.Pool
import Control.Exception




trackingItems :: IO TrackingItems
trackingItems = do
  res <- itemsDec
  case res of
    Left _ -> pure []
    Right i -> pure $ map iid (items i)


parseToBox :: [Int] -> WBox
parseToBox [i, a, b, c, d, e, f, g] = WBox i a b c d e f g


seqToBox :: S.Seq Int -> Maybe WBox
seqToBox s | S.null s = Nothing
           | otherwise = Just $ parseToBox  $ l : ws
           where
             l = S.length s
             maxIndex = l - 1
             s' = S.sort s
             ix :: [Int]
             ix = quot . (* maxIndex) <$> [0,9,25,50,75,91,100] <*> pure 100
             ws = S.index s' <$> ix
                   
seqStatsToWBoxed :: IStats -> WBoxedStats
seqStatsToWBoxed s = WBoxedStats (seqToBox $ bid' s) (seqToBox $ buyout' s)

itemsFile :: FilePath
itemsFile = "items.json"

getItemsJSON :: IO B.ByteString
getItemsJSON = B.readFile itemsFile

itemsDec ::  IO (Either String ItemS)
itemsDec = fmap eitherDecode' getItemsJSON





aucFilesParser :: Value -> Parser [AucFile]
aucFilesParser = withObject "aucFilesParser" $ \o -> o .: "files"

parseAucFile :: B.ByteString -> Maybe AucFile
parseAucFile x = fmap head $ parseMaybe aucFilesParser =<< decode x

auctionsParser :: Value -> Parser [Auction]
auctionsParser = withObject "auctionsParser" $ \o -> o .: "auctions"

parseAuctions :: B.ByteString -> Maybe [Auction]
parseAuctions x = parseMaybe auctionsParser =<< decode x


itemsParser :: Value -> Parser [Item]
itemsParser = withObject "itemsParser" $ \o -> o .: "items"

parseItems :: B.ByteString -> Maybe [Item]
parseItems x = parseMaybe itemsParser =<< decode x


realmsParser :: Value -> Parser [Realm]
realmsParser = withObject "realmsParser" $ \o -> o .: "realms"

parseRealms :: B.ByteString -> Maybe [Realm]
parseRealms x = parseMaybe realmsParser =<< decode x


aucToIStats :: Auction -> IStats
aucToIStats a = IStats {bid' = S.replicate (quantity a) (quot  (bid a) (quantity a))
                       ,buyout' = if buyout a > 0 then S.replicate (quantity a) (quot (buyout a) (quantity a)) else S.empty
                       }

statsConcat :: IStats -> IStats -> IStats
statsConcat s1 s2 = IStats bi bu
  where
    bi = bid' s1 S.>< bid' s2
    bu = buyout' s1 S.>< buyout' s2
          

--foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b

collect :: [Auction] -> M.Map Int IStats
collect = foldl' (\b a -> M.insertWith statsConcat (itemId a) (aucToIStats a) b ) M.empty

allHttpExHandler :: SomeException -> IO B.ByteString
allHttpExHandler e = print e *> pure B.empty

updateRealms :: Config -> IO ()
updateRealms cfg = do
  req <- C.parseRequest $ "https://" <> show (region cfg) <> ".api.battle.net/wow/realm/status?locale=" <> show (langLocale cfg) <> "&apikey=" <> apiKey cfg
  let res = runResourceT $ do
        response <- C.httpLbs (setRequestIgnoreStatus req) $ manager cfg
        pure $ C.responseBody response
  rj <- res `catch` allHttpExHandler
  incrCounter $ counter cfg
  case parseRealms rj of 
    Nothing -> pure ()
    Just x -> addReqsToQ cfg $ S.fromList $ map (ReqAuc cfg ) $ filterRealmsByLocale (filterLocale cfg) $ filterSameRealms x
    
filterItems :: TrackingItems -> ([Auction] -> [Auction])
filterItems ti  = filter (\i -> itemId i `elem` ti)

filterSameRealms :: [Realm] -> [Realm]
filterSameRealms [] = []
filterSameRealms (x:xs) = x : t
  where t = filterSameRealms $ filter (\y -> slug x `notElem` connectedRealms y ) xs

filterRealmsByLocale :: [Locale] -> [Realm] -> [Realm]
filterRealmsByLocale _ [] = []
filterRealmsByLocale [] rs = rs
filterRealmsByLocale ls rs = filter (\y -> locale y `elem` ls) rs

takeAuctionInfo :: Config -> Realm -> IO ()
takeAuctionInfo cfg r = do
  req <- C.parseRequest $  "https://" <> show (region cfg) <> ".api.battle.net/wow/auction/data/" <> slug r <> "?locale=" <> show (langLocale cfg) <> "&apikey=" <> apiKey cfg
  let res = runResourceT $ do
        response <- C.httpLbs  (setRequestIgnoreStatus req) $ manager cfg
        pure $ C.responseBody response
  aj <- res `catch` allHttpExHandler
  incrCounter $ counter cfg
  case parseAucFile aj of
    Nothing -> pure ()
    Just x ->  atomically $ writeTChan (dlChan cfg) (DLAucJson x r)


harvestAuctionJson :: Config -> TrackingItems -> AucFile -> Realm -> IO ()
harvestAuctionJson cfg ti a r = do
  let t = millisToUTC $ lastModified a
      s = slug r
      connP = connPool cfg
  putStrLn $ rname r <> " @ " <> show t
  req <- C.parseRequest $ url a
  aj<-runResourceT $ do 
    response <- C.httpLbs (setRequestIgnoreStatus req) $ manager cfg
    pure $ C.responseBody response
  case parseAuctions aj of
    Nothing -> pure ()
    Just x -> do
      let l = M.toList $ M.map seqStatsToWBoxed $ collect $ filterItems ti x
      i <- writeBoxInDB t s l connP
      print i
      case compare i 0 of
        GT -> do
          print =<< updLastModified t s connP
          changeUpdTime (updatedAt cfg) s t
        _ -> pure ()


addReqToQ :: Config -> ReqParams -> IO ()
addReqToQ cfg reqParam = do
  rq' <- takeMVar (reqQueue cfg)
  putMVar (reqQueue cfg) $ rq' S.|> reqParam

addReqsToQ :: Config -> S.Seq ReqParams  -> IO ()
addReqsToQ cfg reqParams = do
  rq' <- takeMVar (reqQueue cfg)
  putMVar (reqQueue cfg) $ rq' S.>< reqParams


incrCounter :: MVar Int -> IO ()
incrCounter counter = do
  c <- takeMVar counter
  putMVar counter $ c + 1

millisToUTC :: Integer -> UTCTime
millisToUTC t = posixSecondsToUTCTime $ fromInteger t / 1000

runRequest :: ReqParams  -> IO()
runRequest rp = case rp of
  ReqAuc cfg r -> takeAuctionInfo cfg r
  ReqRealms cfg -> updateRealms cfg


runJob :: Config -> IO ()
runJob cfg = do
  c' <- takeMVar (counter cfg)
  rq' <- takeMVar (reqQueue cfg)
  let rqlen = S.length rq'
  case compare c' rqlen of
    LT -> do
      putMVar (counter cfg) 0
      let (r,t) = S.splitAt c' rq'
      putMVar (reqQueue cfg) t
      mapConcurrently_ runRequest r
    _ -> do
      putMVar (counter cfg) (c' - rqlen)
      putMVar (reqQueue cfg) S.empty
      mapConcurrently_ runRequest rq'

isActual :: MVar (M.Map Slug UTCTime) -> Slug -> UTCTime  -> IO Bool
isActual m s t = do
  m' <- readMVar m
  let v = M.lookup s m'
  case v of
    Nothing -> pure False
    Just x -> pure $ x >= t

changeUpdTime :: MVar (M.Map Slug UTCTime) -> Slug -> UTCTime  -> IO ()
changeUpdTime u s t = do
  u' <- takeMVar u
  putMVar u $ M.insert s t u'


updAucJson ::  Config -> IO () 
updAucJson cfg = do
  DLAucJson a r <- atomically $ readTChan (dlChan cfg)
  let t = millisToUTC $ lastModified a 
      s = slug r
  b <- isActual (updatedAt cfg) s t
  unless b $ do
    ti <- trackingItems
    harvestAuctionJson cfg ti a r

loadLastModified :: Config -> IO ()
loadLastModified cfg = do
  xs <- readLastModified (connPool cfg)
  putMVar (updatedAt cfg) $ M.fromList xs

myfun :: IO ()
myfun = do
  cfg <- readCfg "./config.cfg"
  withResource (connPool cfg) initMigrations
  loadLastModified cfg
  forkIO $ forever $ updAucJson cfg
  forkIO $ forever $ do 
    addReqToQ cfg (ReqRealms cfg)
    threadDelay $ 120 * oneSecond
  forever $ do
    forkIO $ runJob cfg
    threadDelay oneSecond
    pure ()