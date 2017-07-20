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
import Control.Monad.IO.Class
import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Concurrent.MVar
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Control.Monad.Trans.Class (lift)




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
            {-
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

realmsFile :: FilePath
realmsFile = "realms.json"

getRealmsJSON :: IO B.ByteString
getRealmsJSON = B.readFile realmsFile

aucsFile :: FilePath
aucsFile = "auctions.json"

getAucsJSON :: IO B.ByteString
getAucsJSON = B.readFile aucsFile

aucsDec :: IO (Either String AuctionS)
aucsDec = fmap eitherDecode' getAucsJSON

aucToTuple :: Auction -> (Int,Int,Int, Int)
aucToTuple a = (itemId a,bid a, buyout a, quantity a)


apikey :: String
apikey = "vrh7sn2zntq4vu7wntkxmd64jwq2ahny"


aucFilesParser :: Value -> Parser [AucFile]
aucFilesParser = withObject "aucFilesParser" $ \o -> o .:? "files" .!= [AucFile {url = "default", lastModified = 12345}]

parseAucFiles :: B.ByteString -> Maybe AucFile
parseAucFiles x = fmap head $ parseMaybe aucFilesParser =<< decode x

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
                   , buyout' = S.replicate (quantity a) (quot  (buyout a) (quantity a))                
                   }

statsConcat :: IStats -> IStats -> IStats
statsConcat  s1 s2 = IStats bi bu 
    where bi = bid' s1 S.><  bid' s2 
          bu = buyout' s1 S.>< buyout' s2
          

--foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b

collect ::  [Auction] -> M.Map Int IStats
collect  = foldl'  (\b a -> M.insertWith statsConcat (itemId a) (aucToIStats a) b ) M.empty 

{- TODO 
doReq :: ReqParams C.Manager Realm -> IO ()
doReq r = case r of 
    ReqRealms m -> takeRealms m
    ReqAuc m r -> takeAuctionInfo m r

-}
takeRealms :: MVar Int -> MVar (S.Seq (ReqParams c rq m r a)) -> C.Manager -> IO (Maybe [Realm])
takeRealms c rq m = do
    req <- C.parseRequest $  "https://eu.api.battle.net/wow/realm/status?locale=en_GB&apikey=" <> apikey
    runResourceT $ do
           response <- C.httpLbs (setRequestIgnoreStatus req) m  
           incrCounter c         
    return $  fmap (mapM_ addReqToQ).map (ReqAuc c rq) .filterRealms  $ parseRealms $ C.responseBody response


filterRealms :: [Realm] -> [Realm]
filterRealms [] = []
filterRealms (x:xs) = x : t
    where t = filterRealms $ filter (\y -> slug x `notElem` connectedRealms y ) xs


takeAuctionInfo :: MVar Int -> MVar (S.Seq (ReqParams c rq m r a)) -> C.Manager  -> Realm -> IO () -- request realm auction info from bnet api
takeAuctionInfo m r = do 
    req <- C.parseRequest $  "https://eu.api.battle.net/wow/auction/data/" <> slug r <> "?locale=en_GB&apikey=" <> apikey
    runResourceT $ do 
        response <- C.httpLbs  (setRequestIgnoreStatus req) m 
        af <- lift parseAucFiles $ C.responseBody response
        addReqToQ (ReqAucJson m af)


harvestAuctionJson :: MVar Int -> MVar (S.Seq (ReqParams c rq m r a)) -> C.Manager -> AucFile -> IO (Maybe (M.Map Int IStats))
harvestAuctionJson c m a = do 
    req <- C.parseRequest $ url a
    runResourceT $ do 
        response <- C.httpLbs (setRequestIgnoreStatus req) m         
        return $  fmap collect $ parseAuctions $ C.responseBody response
        


--reqQueue :: MVar (S.Seq (IO ()))
--reqQueue = newMVar S.empty

--counter :: MVar Int
--initCounter = newMVar 100

addReqToQ :: ReqParams c rq m r a -> MVar (S.Seq (ReqParams c rq m r a)) -> IO ()
addReqToQ reqParam rq = do 
    rq' <- takeMVar rq 
    putMVar rq $ rq' S.|> reqParam 

incrCounter :: MVar Int -> IO ()
incrCounter counter = do 
    c <- takeMVar counter
    putMVar counter $ c + 1

millisToUTC :: Integer -> UTCTime
millisToUTC t = posixSecondsToUTCTime $ fromInteger t / 1000

runRequest :: ReqParams c rq m r a -> IO()
runRequest rp = case rp of 
    ReqAuc c rq m r     -> takeAuctionInfo c rq m r
    ReqRealms c rq m    -> takeRealms c rq m
    ReqAucJson c rq m a ->  harvestAuctionJson c rq m a


myfun :: IO ()
myfun = do
    reqQueue <- newMVar S.empty :: IO (MVar (S.Seq (ReqParams (MVar Int) C.Manager Realm AucFile )))
    counter <- newMVar 100 :: IO (MVar Int)
    manager <- C.newManager C.tlsManagerSettings
    res <- takeRealms manager
    case res of 
        Nothing -> putStrLn "No realms"
        Just x -> do 
            --print  x
            af <- sequence  [takeAuctionInfo manager z | z <- x]
            --print af
            case af of 
                [] -> putStrLn "No auc files"
                ls@(af:afs) -> mapM_ (mapM_ print)  $ map (millisToUTC . lastModified )  <$>  sequence ls
            