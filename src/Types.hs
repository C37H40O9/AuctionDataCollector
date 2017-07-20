{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types
        where
import qualified Data.Sequence as S
import GHC.Generics
import Data.Aeson
import qualified Network.HTTP.Conduit as C
import Control.Concurrent.MVar

                -- Type for whiskers box diagram
data WBox = WBox { ic   :: Int  -- items count
                 , minW :: Int
                 , botW :: Int
                 , p25  :: Int
                 , p50  :: Int
                 , p75  :: Int
                 , topW :: Int
                 , maxW :: Int } deriving Show

data Item = Item { name :: String
                 , iid :: Int
                 } deriving (Eq, Show)

data Auction = Auction { bid :: Int
                       , buyout :: Int
                       , quantity :: Int
                       , itemId :: Int
                       } deriving (Eq, Show)

data IStats = IStats { bid' :: S.Seq Int
                     , buyout' :: S.Seq Int                     
                     } deriving (Eq, Show)

newtype AuctionS = AuctionS {auctions :: [Auction]} deriving (Eq, Show)

newtype ItemS = ItemS {items :: [Item]} deriving (Eq, Show)

type Region = String

data Realm = Realm
             { rname :: String
             , slug :: String
             , connectedRealms :: [String]} deriving (Eq, Show)

newtype Realms = Realms {realms ::[Realm]} deriving (Eq, Show) 

data AucFile = AucFile { url          :: String
                       , lastModified :: Integer} deriving (Eq, Show, Generic)

-- counter requestQueue manager realm aucfile 
data ReqParams c rq m r a = ReqAuc     (MVar Int) (MVar (S.Seq (ReqParams c rq m r a ))) C.Manager Realm 
                          | ReqRealms  (MVar Int) (MVar (S.Seq (ReqParams c rq m r a ))) C.Manager 
                          | ReqAucJson (MVar Int) (MVar (S.Seq (ReqParams c rq m r a ))) C.Manager AucFile

instance FromJSON AucFile {-where 
    parseJSON = withObject "file" $ \o -> do 
        url <- o .: "url"
        lastModified <- o .: "lastModified"
        return AucFile{..}
-}
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
        iid <- o .: "id"
        return Item{..}


instance FromJSON Realm where
    parseJSON = withObject "realm" $ \o -> do          
           rname <- o .: "name"
           slug <- o .: "slug"
           connectedRealms <- o .: "connected_realms"
           return Realm{..}
