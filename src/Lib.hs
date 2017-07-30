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




someFunc :: IO [Int]
someFunc = do
    res <- itemsDec
    case res of
        Left err -> return []
        Right i -> return $ map iid (items i)

inames :: IO [(Int,String)]
inames = do
    res <- itemsDec
    case res of
        Left err -> return []
        Right i -> return $ map (liftA2 (,) iid  name ) (items i)

{-
someFunc2 :: IO ()
someFunc2 = do
    res <- aucsDec
    inam <- inames 
    case res of
        Left err -> putStrLn err
        Right a -> do
            ids <- someFunc
            let aucList =  filter (\x -> itemId x `elem` ids) (auctions a)
            let m = collect aucList
            mapM_ print $ M.assocs $ M.map (statToBox . buyout') m
            
            let aucList = map aucToTuple $ filter (\x -> itemId x `elem` ids) (auctions a)
            let avg = [(the iid, sum bid, sum buyout, sum quantity)
                      | (iid, bid, buyout, quantity) <- aucList
                      , then group by iid using groupWith
                      , then sortWith by sum quantity]            
            mapM_ (putStrLn . (\(l,i1,i2,i3)-> l <> " " <> show i1 <> " " <> show i2 <> " " <> show i3)) [(n,b,byu,c) | (i,n) <- inam , (ii,b,byu,c) <- avg, i==ii]
            -}


parseToBox :: [Int] -> Maybe WBox
parseToBox [i, a, b, c, d, e, f, g] = Just (WBox i a b c d e f g)
parseToBox _ = Nothing

statToBox :: S.Seq Int -> Maybe WBox
statToBox s = parseToBox  $ l : ws 
                   where 
                        l = S.length s - 1
                        s' = S.sort s
                        ix :: [Int]
                        ix = quot . (*l) <$> [0,9,25,50,75,91,100] <*> pure 100
                        ws = S.index s' <$> ix
                   
                        

itemsFile :: FilePath
itemsFile = "items.json"

getItemsJSON :: IO B.ByteString
getItemsJSON = B.readFile itemsFile

itemsDec ::  IO (Either String ItemS)
itemsDec = fmap eitherDecode' getItemsJSON


apikey :: String
apikey = "vrh7sn2zntq4vu7wntkxmd64jwq2ahny"


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

{- TODO 
doReq :: ReqParams C.Manager Realm -> IO ()
doReq r = case r of 
    ReqRealms m -> takeRealms m
    ReqAuc m r -> takeAuctionInfo m r

-}
takeRealms :: MVar Int -> MVar (S.Seq (ReqParams c rq m r a ch)) -> C.Manager -> TChan(DLParams a r) -> IO ()
takeRealms c rq m ch = do    
    req <- C.parseRequest $  "https://eu.api.battle.net/wow/realm/status?locale=en_GB&apikey=" <> apikey    
    bs<-runResourceT $ do               
               response <- C.httpLbs (setRequestIgnoreStatus req) m                                       
               return $  C.responseBody response
    incrCounter c
    let rr =  parseRealms bs    
    case rr of 
        Nothing -> return ()
        Just x -> addReqsToQ rq $ S.fromList $ map (ReqAuc c rq m ch ) $ filterRealms x
    


filterRealms :: [Realm] -> [Realm]
filterRealms [] = []
filterRealms (x:xs) = x : t
    where t = filterRealms $ filter (\y -> slug x `notElem` connectedRealms y ) xs


takeAuctionInfo :: MVar Int -> MVar (S.Seq (ReqParams c rq m r a u)) -> C.Manager -> TChan (DLParams a r)  -> Realm -> IO () -- request realm auction info from bnet api
takeAuctionInfo c rq m ch r = do 
    req <- C.parseRequest $  "https://eu.api.battle.net/wow/auction/data/" <> slug r <> "?locale=en_GB&apikey=" <> apikey
    aj<-runResourceT $ do            
            response <- C.httpLbs  (setRequestIgnoreStatus req) m
            return $ C.responseBody response  
    incrCounter c  
    let af = parseAucFile aj
    case af of
        Nothing -> return ()
        Just x ->  atomically $ writeTChan ch (DLAucJson x r)
    

-- TODO must work with channel
harvestAuctionJson :: C.Manager -> AucFile -> Realm ->  IO ()--IO (Maybe (M.Map Int IStats))
harvestAuctionJson m a r = do
    req <- C.parseRequest $ url a
    putStrLn $ slug r <> " @ " <> show (millisToUTC $ lastModified a)
    {-
    aj<-runResourceT $ do 
            response <- C.httpLbs (setRequestIgnoreStatus req) m
            return $ C.responseBody response
    let as = parseAuctions aj
    case as of
        Nothing -> return ()
        Just x -> print $  collect x -}
    




addReqToQ :: MVar (S.Seq (ReqParams c rq m r a u)) -> ReqParams c rq m r a u -> IO ()
addReqToQ rq reqParam = do 
    rq' <- takeMVar rq 
    putMVar rq $ rq' S.|> reqParam 

addReqsToQ :: MVar (S.Seq (ReqParams c rq m r a u)) -> S.Seq(ReqParams c rq m r a u) -> IO ()
addReqsToQ rq reqParams = do
    rq' <- takeMVar rq
    putMVar rq $ rq' S.>< reqParams


incrCounter :: MVar Int -> IO ()
incrCounter counter = do 
    c <- takeMVar counter
    putMVar counter $ c + 1

millisToUTC :: Integer -> UTCTime
millisToUTC t = posixSecondsToUTCTime $ fromInteger t / 1000

runRequest :: ReqParams c rq m r a u -> IO()
runRequest rp = case rp of 
    ReqAuc c rq m ch r    -> takeAuctionInfo c rq m ch r
    ReqRealms c rq m ch   -> takeRealms c rq m ch
    --ReqAucJson c rq m a r -> harvestAuctionJson c rq m a r


runJob :: MVar Int -> MVar (S.Seq (ReqParams c rq m r a ch)) -> IO ()
runJob c rq = do 
    c' <- takeMVar c 
    rq' <- takeMVar rq
    let rqlen = S.length rq'
    if rqlen >= c' 
        then do 
            putMVar c 0
            let (r,t) = S.splitAt c' rq'
            putMVar rq t
            mapM_ (\x -> forkIO $ runRequest x) r 
        else do 
            putMVar c (c' - rqlen)            
            putMVar rq S.empty
            mapM_ (\x -> forkIO $ runRequest x) rq'

oneSecond = 1000000 :: Int

isActual :: MVar (M.Map String UTCTime) -> String -> UTCTime  -> IO Bool
isActual m s t = do
    m' <- readMVar m
    let v = M.lookup s m'
    case v of
        Nothing -> return False
        Just x -> return $ x >= t

changeUpdTime :: MVar (M.Map String UTCTime) -> String -> UTCTime  -> IO ()
changeUpdTime u s t = do
    u' <- takeMVar u
    putMVar u $ M.insert s t u'


updAucJson :: C.Manager -> TChan (DLParams a r) -> MVar (M.Map String UTCTime) -> IO ()
updAucJson m ch u =  do
    DLAucJson a r <- atomically $ readTChan ch
    let t = millisToUTC $ lastModified a 
        s = slug r 
    b <- isActual u s t 
    unless b $ do 
        harvestAuctionJson m a r
        changeUpdTime u s t
    
        



myfun :: IO ()
myfun = do
    reqQueue <- newMVar S.empty :: IO (MVar (S.Seq (ReqParams c rq m r a ch)))
    downloadChan <- atomically newTChan :: IO (TChan (DLParams a r))
    counter <- newMVar 99 :: IO (MVar Int)
    updatedAt <- newMVar M.empty :: IO (MVar (M.Map String UTCTime))
    newStablePtr reqQueue
    newStablePtr counter
    newStablePtr updatedAt
    manager <- C.newManager C.tlsManagerSettings
    forkIO $ forever $ updAucJson manager downloadChan updatedAt
    forkIO $ forever $ do 
        addReqToQ reqQueue (ReqRealms counter reqQueue manager downloadChan)
        threadDelay $ 120 * oneSecond
    forever $ do 
                --c <- readMVar counter
                --print c 
                --q <- readMVar reqQueue
                --print $ S.length q                
                forkIO $ runJob counter reqQueue                
                threadDelay oneSecond                
                return ()

