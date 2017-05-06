{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TransformListComp #-}
--{-# LANGUAGE OverloadedRecordFields #-}

module Lib
     where

import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import GHC.Exts (sortWith, groupWith, the)
import Data.Monoid ((<>))
import Data.List (foldl')
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings, setRequestIgnoreStatus)
import qualified Network.HTTP.Conduit as C
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Either (rights)




someFunc :: IO [Int]
someFunc = do
    res <- itemsDec
    case res of
        Left err -> return []
        Right i -> return $ map Lib.id (items i)

inames :: IO [(Int,String)]
inames = do
    res <- itemsDec
    case res of
        Left err -> return []
        Right i -> return $ map (\x -> (Lib.id x, name x)) (items i)


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
                   
                        
                -- Whiskers Box
data WBox = WBox { ic   :: Int  -- items count
                 , minW :: Int
                 , botW :: Int
                 , p25  :: Int
                 , p50  :: Int
                 , p75  :: Int
                 , topW :: Int
                 , maxW :: Int } deriving Show
                 

printItem :: Item -> IO ()
printItem = putStrLn . (\x -> "name: " <> name x <> " id: " <> show (Lib.id x))

printAuc :: Auction -> IO ()
printAuc = putStrLn . (\x -> "itemId: " <> show (itemId x) <> " bid: " <> show (bid x) <> " buyout: " <> show (buyout x) <> " quantity: " <> show (quantity x))

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

--realmsDec :: Maybe [ConnectedRealms]
--realmsDec =  parseMaybe realms 

aucsFile :: FilePath
aucsFile = "auctions.json"

getAucsJSON :: IO B.ByteString
getAucsJSON = B.readFile aucsFile

aucsDec :: IO (Either String AuctionS)
aucsDec = fmap eitherDecode' getAucsJSON

aucToTuple :: Auction -> (Int,Int,Int, Int)
aucToTuple a = (itemId a,bid a, buyout a, quantity a)

data Item = Item { name :: String
                 , id :: Int
                 } deriving (Eq, Show)

data Auction = Auction { bid :: Int
                       , buyout :: Int
                       , quantity :: Int
                       , itemId :: Int
                       } deriving (Eq, Show)

data IStats = IStats { bid' :: S.Seq Int
                     , buyout' :: S.Seq Int
                     --, quantity' :: Int
                     } deriving (Eq, Show)

aToS :: Auction -> IStats
aToS a = IStats { bid'    = S.replicate (quantity a) (quot  (bid a)    (quantity a))
                , buyout' = S.replicate (quantity a) (quot  (buyout a) (quantity a))
                --, quantity' = quantity a
                }

mcon :: IStats -> IStats -> IStats
mcon  f s = IStats bi bu --q
    where bi = bid' f S.><  bid' s 
          bu = buyout' f S.>< buyout' s
          --q = quantity' f + quantity' s

--foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b

collect ::  [Auction] -> M.Map Int IStats
collect  = foldl'  (\b a -> M.insertWith mcon (itemId a) (aToS a) b ) M.empty 

newtype AuctionS = AuctionS {auctions :: [Auction]} deriving (Eq, Show)

newtype ItemS = ItemS {items :: [Item]} deriving (Eq, Show)

newtype ConnectedRealms =  ConnectedRealms {connectedRealms :: [String]} deriving (Eq, Show)




--realms :: Value -> Parser [ConnectedRealms]
--realms = withObject "realms" $ \o -> o .: "realms"

-- 

instance FromJSON ConnectedRealms where 
    parseJSON = withObject "connected_realms" $ \o -> do
        connectedRealms <- o .: "connected_realms"
        return ConnectedRealms{..}



instance FromJSON AuctionS where
    parseJSON = withObject "auctions" $ \o -> do
        auctions <- o .: "auctions"
        return AuctionS{..}

instance FromJSON ItemS where
    parseJSON = withObject "items" $ \o -> do
        items <- o .: "items"
        return ItemS{..}

instance FromJSON Auction where
    parseJSON = withObject "aucs" $ \o -> do
        bid <- o .: "bid"
        buyout <- o .: "buyout"
        quantity <- o .: "quantity"
        itemId <- o .: "item"
        return Auction{..}

instance FromJSON Item where
    parseJSON = withObject "items" $ \o -> do
        name <- o .: "name"
        id <- o .: "id"
        return Item{..}

apikey :: Text
apikey = "vrh7sn2zntq4vu7wntkxmd64jwq2ahny"

data Realm = Realm
             { rname :: Text
             , slug :: Text
             , connected_realms :: [Text]} deriving (Eq, Show)

newtype Realms = Realms {realms ::[Realm]} deriving (Eq, Show) 

instance FromJSON Realm where
    parseJSON = withObject "realm" $ \o -> do
           --realmsO <- o .: "realms"
           rname <- o .: "name"
           slug <- o .: "slug"
           connected_realms <- o .: "connected_realms"
           return Realm{..}

instance FromJSON  Realms where
    parseJSON = withObject "r" $ \o -> do
        realms <- o .: "realms"
        return Realms{..}



takeRealms :: Manager -> IO (Either String Realms)
takeRealms m = do
    req <- C.parseRequest $ T.unpack $ "https://eu.api.battle.net/wow/realm/status?locale=en_GB&apikey=" <> apikey
    runResourceT $ do
           response <- C.httpLbs (setRequestIgnoreStatus req) m
           --print response
           return $ eitherDecode' $ C.responseBody response

filterRealms :: [Realm] -> [Realm]
filterRealms [] = []
filterRealms (x:xs) = x : t
    where t = filterRealms $ filter (\y -> slug x `notElem` connected_realms y ) xs

myfun :: IO ()
myfun = do
    manager <- C.newManager C.tlsManagerSettings
    res <- takeRealms manager
    case res of
        Left e -> print e
        Right r -> mapM_ print $ filterRealms $ realms r




