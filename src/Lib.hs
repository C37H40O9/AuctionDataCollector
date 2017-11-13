{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
--{-# LANGUAGE TransformListComp #-}
--{-# LANGUAGE OverloadedRecordFields #-}

module Lib
     where
import Types.Types
import Types.Locale
import DB
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
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Applicative (liftA2)
import Control.Monad (join, forever, when, unless)
import Control.Concurrent.MVar
import Data.Time.Clock.POSIX
import Data.Time.Clock
import System.Locale
import Control.Monad.Trans.Class (lift)
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Foreign.StablePtr
import qualified Control.Monad.Parallel as P
import Control.Concurrent.Async
import Data.Configurator
import Database.PostgreSQL.Simple
import Data.Pool
import Data.Maybe (fromJust)




trackingItems :: IO TrackingItems
trackingItems = do
    res <- itemsDec
    case res of
        Left err -> return []
        Right i -> return $ map iid (items i)


parseToBox :: [Int] -> WBox
parseToBox [i, a, b, c, d, e] = WBox i a b c d e


seqToBox :: S.Seq Int -> Maybe WBox
seqToBox s | S.null s = Nothing
           | otherwise = Just $ parseToBox  $ l : ws
                where
                    l = S.length s
                    maxIndex = l - 1
                    s' = S.sort s
                    ix :: [Int]
                    ix = quot . (* maxIndex) <$> [9,25,50,75,91] <*> pure 100
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
aucToIStats a = IStats { bid'    = S.replicate (quantity a) (quot  (bid a)    (quantity a))
                       , buyout' = if buyout a > 0 then S.replicate (quantity a) (quot  (buyout a) (quantity a))  else S.empty
                       }

statsConcat :: IStats -> IStats -> IStats
statsConcat  s1 s2 = IStats bi bu
    where bi = bid'    s1 S.>< bid' s2
          bu = buyout' s1 S.>< buyout' s2
          

--foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b

collect ::  [Auction] -> M.Map Int IStats
collect  = foldl' (\b a -> M.insertWith statsConcat (itemId a) (aucToIStats a) b ) M.empty 


takeRealms :: Config -> IO ()
takeRealms cfg = do
    req <- C.parseRequest $  "https://" <> show (region cfg) <> ".api.battle.net/wow/realm/status?locale=" <> show (langLocale cfg) <> "&apikey=" <> apiKey cfg
    bs<-runResourceT $ do               
               response <- C.httpLbs (setRequestIgnoreStatus req) $ manager cfg
               return $  C.responseBody response
    incrCounter $ counter cfg
    let rr =  parseRealms bs
    case rr of 
        Nothing -> return ()
        Just x -> addReqsToQ cfg $ S.fromList $ map (ReqAuc cfg ) $ filterRealmsByLocale (filterLocale cfg) $ filterSameRealms x
    


filterSameRealms :: [Realm] -> [Realm]
filterSameRealms [] = []
filterSameRealms (x:xs) = x : t
    where t = filterSameRealms $ filter (\y -> slug x `notElem` connectedRealms y ) xs

filterRealmsByLocale :: [Locale] -> [Realm] -> [Realm]
filterRealmsByLocale _ [] = []
filterRealmsByLocale ls rs = filter (\y -> locale y `elem` ls) rs

takeAuctionInfo :: Config -> Realm -> IO ()
takeAuctionInfo cfg r = do
    req <- C.parseRequest $  "https://" <> show (region cfg) <> ".api.battle.net/wow/auction/data/" <> slug r <> "?locale=" <> show (langLocale cfg) <> "&apikey=" <> apiKey cfg
    aj<-runResourceT $ do            
            response <- C.httpLbs  (setRequestIgnoreStatus req) $ manager cfg
            return $ C.responseBody response
    incrCounter $ counter cfg
    let af = parseAucFile aj
    case af of
        Nothing -> return ()
        Just x ->  atomically $ writeTChan (dlChan cfg) (DLAucJson x r)
    


harvestAuctionJson :: Config -> TrackingItems -> AucFile -> Realm ->  IO ()
harvestAuctionJson cfg ti a r = do
    req <- C.parseRequest $ url a
    let t = millisToUTC $ lastModified a
    putStrLn $ rname r <> " @ " <> show t
    
    aj<-runResourceT $ do 
            response <- C.httpLbs (setRequestIgnoreStatus req) $ manager cfg
            return $ C.responseBody response
    let as = parseAuctions aj
    case as of
        Nothing -> return ()
        Just x -> do
            i <-  M.traverseWithKey (\k v -> writeBoxInTBid t (slug r) k (fromJust $ bbid v) (connPool cfg) ) $  M.map seqStatsToWBoxed $ collect $ filter (\y -> itemId y `elem` ti) x
            print i


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
    ReqAuc cfg r    -> takeAuctionInfo cfg r
    ReqRealms cfg   -> takeRealms cfg


runJob :: Config -> IO ()
runJob cfg = do
    c' <- takeMVar (counter cfg)
    rq' <- takeMVar (reqQueue cfg)
    let rqlen = S.length rq'
    if rqlen >= c'
        then do
            putMVar (counter cfg) 0
            let (r,t) = S.splitAt c' rq'
            putMVar (reqQueue cfg) t
            mapConcurrently_ runRequest r 
        else do
            putMVar (counter cfg) (c' - rqlen)
            putMVar (reqQueue cfg) S.empty
            mapConcurrently_ runRequest rq' 

oneSecond = 1000000 :: Int

isActual :: MVar (M.Map Slug UTCTime) -> Slug -> UTCTime  -> IO Bool
isActual m s t = do
    m' <- readMVar m
    let v = M.lookup s m'
    case v of
        Nothing -> return False
        Just x -> return $ x >= t

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
        changeUpdTime (updatedAt cfg) s t




myfun :: IO ()
myfun = do
    conf <- load [Required "./config.cfg"]
    let dbconf = subconfig "database" conf
    let apiconf = subconfig "api" conf
    connectUser <- require dbconf "username"
    connectPassword <- require dbconf "password"
    connectDatabase <- require dbconf "database"
    connectHost <- require dbconf "host"
    connectPort <- require dbconf "port"
    apiKey <- require apiconf "key" :: IO ApiKey
    apiLimit <- require apiconf "limit"
    region <- read <$> require apiconf "region" :: IO Region
    langLocale <- read <$> require apiconf "langLocale" :: IO Locale
    filterLocale <- fmap read <$> require apiconf "filterLocale" :: IO [Locale]
    reqQueue <- newMVar S.empty :: IO (MVar (S.Seq ReqParams))
    dlChan <- atomically newTChan :: IO (TChan DLParams)
    counter <- newMVar apiLimit :: IO (MVar Int)
    updatedAt <- newMVar M.empty :: IO (MVar (M.Map Slug UTCTime))
    manager <- C.newManager C.tlsManagerSettings
    let connInfo = ConnectInfo { connectHost
                               , connectPort
                               , connectUser
                               , connectPassword
                               , connectDatabase
                               }
    connPool <- createPool (connect connInfo) close 1 10 20
    let cfg = Config { apiKey
                     , region
                     , langLocale
                     , filterLocale
                     , counter
                     , reqQueue
                     , manager
                     , dlChan
                     , updatedAt
                     , connPool
                     }
    withResource connPool initMigrations
    --conn <- connect connInfo
    --initMigrations conn
    forkIO $ forever $ updAucJson cfg
    forkIO $ forever $ do 
        addReqToQ cfg (ReqRealms cfg)
        threadDelay $ 120 * oneSecond
    forever $ do
                forkIO $ runJob cfg
                threadDelay oneSecond
                return ()