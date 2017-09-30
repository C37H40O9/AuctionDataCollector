{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE TransformListComp #-}
--{-# LANGUAGE OverloadedRecordFields #-}

module Lib
     where
import Types
import DB
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
--import GHC.Exts (sortWith, groupWith, the)
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
import Data.Time.Format
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




trackingItems :: IO TrackingItems
trackingItems = do
    res <- itemsDec
    case res of
        Left err -> return []
        Right i -> return $ map iid (items i)


parseToBox :: [Int] -> WBox
parseToBox [i, a, b, c, d, e, f, g] = WBox i a b c d e f g


seqToBox :: S.Seq Int -> Maybe WBox
seqToBox s = if S.null s then Nothing
            else Just $ parseToBox  $ l : ws
                   where 
                        l = S.length s - 1
                        s' = S.sort s
                        ix :: [Int]
                        ix = quot . (*l) <$> [0,9,25,50,75,91,100] <*> pure 100
                        ws = S.index s' <$> ix
                   
seqStatsToWBoxed :: IStats -> WBoxedStats
seqStatsToWBoxed s = BoxedStats (seqToBox $ bid' s) (seqToBox $ buyout' s)

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


takeRealms :: ApiKey -> MVar Int -> MVar (S.Seq ReqParams) -> C.Manager -> TChan DLParams -> IO ()
takeRealms k c rq m ch = do
    req <- C.parseRequest $  "https://eu.api.battle.net/wow/realm/status?locale=en_GB&apikey=" <> k
    bs<-runResourceT $ do               
               response <- C.httpLbs (setRequestIgnoreStatus req) m
               return $  C.responseBody response
    incrCounter c
    let rr =  parseRealms bs
    case rr of 
        Nothing -> return ()
        Just x -> addReqsToQ rq $ S.fromList $ map (ReqAuc k c rq m ch ) $ filterRealmsByLocale [RU_RU] $ filterSameRealms x
    


filterSameRealms :: [Realm] -> [Realm]
filterSameRealms [] = []
filterSameRealms (x:xs) = x : t
    where t = filterSameRealms $ filter (\y -> slug x `notElem` connectedRealms y ) xs

filterRealmsByLocale :: [Locale] -> [Realm] -> [Realm]
filterRealmsByLocale _ [] = []
filterRealmsByLocale ls rs = filter (\y -> locale y `elem` ls) rs

takeAuctionInfo :: ApiKey -> MVar Int -> MVar (S.Seq ReqParams ) -> C.Manager -> TChan DLParams -> Realm -> IO () -- request realm auction info from bnet api
takeAuctionInfo k c rq m ch r = do
    req <- C.parseRequest $  "https://eu.api.battle.net/wow/auction/data/" <> slug r <> "?locale=en_GB&apikey=" <> k
    aj<-runResourceT $ do            
            response <- C.httpLbs  (setRequestIgnoreStatus req) m
            return $ C.responseBody response
    incrCounter c
    let af = parseAucFile aj
    case af of
        Nothing -> return ()
        Just x ->  atomically $ writeTChan ch (DLAucJson x r)
    


harvestAuctionJson :: C.Manager -> TrackingItems -> AucFile -> Realm ->  IO ()

harvestAuctionJson m ti a r = do
    req <- C.parseRequest $ url a
    putStrLn $ slug r <> " @ " <> show (millisToUTC $ lastModified a)
    
    aj<-runResourceT $ do 
            response <- C.httpLbs (setRequestIgnoreStatus req) m
            return $ C.responseBody response
    let as = parseAuctions aj
    case as of
        Nothing -> return ()
        Just x -> print $  M.map seqStatsToWBoxed $ collect $ filter (\x -> itemId x `elem` ti) x
    




addReqToQ :: MVar (S.Seq ReqParams ) -> ReqParams  -> IO ()
addReqToQ rq reqParam = do
    rq' <- takeMVar rq
    putMVar rq $ rq' S.|> reqParam

addReqsToQ :: MVar (S.Seq ReqParams ) -> S.Seq ReqParams  -> IO ()
addReqsToQ rq reqParams = do
    rq' <- takeMVar rq
    putMVar rq $ rq' S.>< reqParams


incrCounter :: MVar Int -> IO ()
incrCounter counter = do
    c <- takeMVar counter
    putMVar counter $ c + 1

millisToUTC :: Integer -> UTCTime
millisToUTC t = posixSecondsToUTCTime $ fromInteger t / 1000

runRequest :: ReqParams  -> IO()
runRequest rp = case rp of
    ReqAuc k c rq m ch r    -> takeAuctionInfo k c rq m ch r
    ReqRealms k c rq m ch   -> takeRealms k c rq m ch


runJob :: MVar Int -> MVar (S.Seq ReqParams ) -> IO ()
runJob c rq = do
    c' <- takeMVar c
    rq' <- takeMVar rq
    let rqlen = S.length rq'
    if rqlen >= c'
        then do
            putMVar c 0
            let (r,t) = S.splitAt c' rq'
            putMVar rq t
            mapConcurrently_ runRequest r --mapM_ (forkIO . runRequest) r 
        else do
            putMVar c (c' - rqlen)
            putMVar rq S.empty
            mapConcurrently_ runRequest rq' --mapM_ (forkIO . runRequest) rq'

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


updAucJson :: C.Manager -> TChan DLParams -> MVar (M.Map Slug UTCTime) -> IO ()
updAucJson m ch u =  do
    DLAucJson a r <- atomically $ readTChan ch
    let t = millisToUTC $ lastModified a 
        s = slug r
    b <- isActual u s t
    unless b $ do
        ti <- trackingItems
        harvestAuctionJson m ti a r
        changeUpdTime u s t




myfun :: IO ()
myfun = do
    conf <- load [Required "./config.cfg"]
    let subconf = subconfig "database" conf
    dbuser <- require subconf "username"
    dbpass <- require subconf "password"
    dbname <- require subconf "database"
    dbhost <- require subconf "host"
    dbport <- require subconf "port"
    apikey <- require conf "api.key" :: IO ApiKey
    region <- (\s -> read s :: Region) <$> require conf "api.region"
    langLocale <- (\s -> read s :: Locale) <$> require conf "api.langLocale"
    filterLocale <- (\s -> read s :: [Locale]) <$> require conf "api.filterLocale"
    let connInfo = ConnectInfo { connectHost = dbhost
                               , connectPort = dbport
                               , connectUser = dbuser
                               , connectPassword = dbpass
                               , connectDatabase = dbname
                               }
    conn <- connect connInfo
    initMigrations conn
    reqQueue <- newMVar S.empty :: IO (MVar (S.Seq ReqParams ))
    downloadChan <- atomically newTChan :: IO (TChan DLParams )
    counter <- newMVar 99 :: IO (MVar Int)
    updatedAt <- newMVar M.empty :: IO (MVar (M.Map Slug UTCTime))
    manager <- C.newManager C.tlsManagerSettings
    forkIO $ forever $ updAucJson manager downloadChan updatedAt
    forkIO $ forever $ do 
        addReqToQ reqQueue (ReqRealms apikey counter reqQueue manager downloadChan)
        threadDelay $ 120 * oneSecond
    forever $ do
                forkIO $ runJob counter reqQueue
                threadDelay oneSecond
                return ()