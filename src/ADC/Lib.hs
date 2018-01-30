{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE TransformListComp #-}
--{-# LANGUAGE OverloadedRecordFields #-}

module ADC.Lib (loadLastModified
           ,updAucJson
           ,addReqToQ
           ,runJob
)


where
import ADC.Types.Types
import ADC.Types.Locale
import ADC.DB
import ADC.Config
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
import Control.Monad (unless)
import Control.Concurrent.MVar
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.Async
import Data.Pool
import Control.Exception
import Data.Int (Int64)
import Data.Functor (($>))



parseToBox :: [Int] -> WBox
parseToBox [ic,minW,botW,p25,p50,p75,topW,maxW] = WBox {..}

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

aucFilesParser :: Value -> Parser [AucFile]
aucFilesParser = withObject "aucFilesParser" $ \o -> o .: "files"

parseAucFile :: B.ByteString -> Maybe AucFile
parseAucFile x = fmap head $ parseMaybe aucFilesParser =<< decode x

auctionsParser :: Value -> Parser [Auction]
auctionsParser = withObject "auctionsParser" $ \o -> o .: "auctions"

parseAuctions :: B.ByteString -> Maybe [Auction]
parseAuctions x = parseMaybe auctionsParser =<< decode x

realmsParser :: Value -> Parser [Realm]
realmsParser = withObject "realmsParser" $ \o -> o .: "realms"

parseRealms :: B.ByteString -> Maybe [Realm]
parseRealms x = parseMaybe realmsParser =<< decode x


aucToIStats :: Auction -> IStats
aucToIStats a = IStats bid' buyout'
  where
    q = quantity a
    bid' = S.replicate q $ quot (bid a) q
    buyout' | buyout a > 0 = S.replicate q $ quot (buyout a) q
            | otherwise = S.empty


--foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b

collect :: [Auction] -> M.Map Int IStats
collect = foldl' (\b a -> M.insertWith mappend (itemId a) (aucToIStats a) b ) M.empty

allHttpExHandler :: C.HttpException -> IO B.ByteString
allHttpExHandler e = print e $> B.empty

sendRequest :: C.Manager -> C.Request -> IO B.ByteString
sendRequest m r = runResourceT (C.responseBody <$> C.httpLbs (setRequestIgnoreStatus r) m)
  `catch` allHttpExHandler

updateRealms :: Config -> IO ()
updateRealms cfg = do
  req <- C.parseRequest $ "https://" <> show (region cfg)
      <> ".api.battle.net/wow/realm/status?locale=" <> show (langLocale cfg)
      <> "&apikey=" <> apiKey cfg
  rj <- sendRequest (manager cfg) req
  incrCounter $ counter cfg
  case parseRealms rj of 
    Nothing -> pure ()
    Just x -> addReqsToQ cfg $ S.fromList $ map (ReqAuc cfg )
            $ filterRealmsByLocale (filterLocale cfg) $ filterSameRealms x
    
filterItems :: TrackingItems -> [Auction] -> [Auction]
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
  req <- C.parseRequest $  "https://" <> show (region cfg)
      <> ".api.battle.net/wow/auction/data/" <> slug r <> "?locale="
      <> show (langLocale cfg) <> "&apikey=" <> apiKey cfg
  aj <- sendRequest (manager cfg) req
  incrCounter $ counter cfg
  case parseAucFile aj of
    Nothing -> pure ()
    Just x ->  atomically $ writeTChan (dlChan cfg) (DLAucJson x r)


harvestAuctionJson :: Config -> TrackingItems -> AucFile -> Realm -> IO Int64
harvestAuctionJson cfg ti a r = do
  let t = millisToUTC $ lastModified a
      s = slug r
      connP = connPool cfg
  putStrLn $ rname r <> " @ " <> show t
  req <- C.parseRequest $ url a
  aj<- sendRequest (manager cfg) req
  case parseAuctions aj of
    Nothing -> pure 0
    Just x -> do
      let l = M.toList $ M.map seqStatsToWBoxed $ collect $ filterItems ti x
      writeBoxInDB t s l connP


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
    ti <- trackingItems $ connPool cfg
    i <- harvestAuctionJson cfg ti a r
    print i
    case compare i 0 of
      GT -> do
        print =<< updLastModified t s (connPool cfg)
        changeUpdTime (updatedAt cfg) s t
      _ -> pure ()

loadLastModified :: Config -> IO ()
loadLastModified cfg = do
  xs <- readLastModified (connPool cfg)
  putMVar (updatedAt cfg) $ M.fromList xs