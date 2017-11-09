{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Types ( WBox(..)
                   , Item(..)
                   , Auction(..)
                   , IStats(..)
                   , WBoxedStats(..)
                   , ItemS(..)
                   , ApiKey
                   , TrackingItems
                   , Profession(..)
                   , Slug
                   , Region(..)
                   , Realm(..)
                   , AucFile(..)
                   , Config(..)
                   , ReqParams(..)
                   , DLParams(..)
                   )
        where
import Types.Locale
import qualified Data.Sequence as S
import Data.Aeson
import qualified Network.HTTP.Conduit as C
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Data.Time.Clock
import qualified Data.Map.Strict as M
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

                -- Type for whiskers box diagram
data WBox = WBox { ic   :: Int  -- items count
                 , minW :: Int
                 , botW :: Int
                 , p25  :: Int
                 , p50  :: Int
                 , p75  :: Int
                 , topW :: Int
                 , maxW :: Int } deriving (Eq, Show)

instance ToRow WBox where
    toRow b = map toField $ [ic , minW , botW , p25 , p50 , p75 , topW , maxW ] <*> pure b

instance ToField WBox where
    toField b = Many $ map toField $ [ic , minW , botW , p25 , p50 , p75 , topW , maxW ] <*> pure b

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

data WBoxedStats = BoxedStats { bbid :: Maybe WBox
                             , bbuyout :: Maybe WBox
                             } deriving (Eq, Show)

newtype AuctionS = AuctionS {auctions :: [Auction]} deriving (Eq, Show)

newtype ItemS = ItemS {items :: [Item]} deriving (Eq, Show)


type ApiKey = String

type TrackingItems = [Int]

data Profession = Alchemy
                | Engineering
                | Leatherworking
                | Blacksmith
                | Enchanting
                | Inscription
                | Tailoring
                | Skinning
                | Jewelcrafting
                | Herbalism
                | Mining
                | World deriving (Eq, Show)

type Slug = String

data Region = EU | KR | TW | US deriving (Eq, Ord, Show, Read)


data Realm = Realm
             { rname :: String
             , slug :: Slug
             , locale :: Locale
             , connectedRealms :: [Slug]} deriving (Eq, Show)



data AucFile = AucFile { url          :: String
                       , lastModified :: Integer} deriving (Eq, Show)

data Config = Config { apiKey :: ApiKey
                     , region :: Region
                     , langLocale :: Locale
                     , filterLocale :: [Locale]
                     , counter :: MVar Int
                     , reqQueue :: MVar (S.Seq ReqParams)
                     , manager :: C.Manager
                     , dlChan :: TChan DLParams
                     , updatedAt :: MVar (M.Map Slug UTCTime)
                     }
-- counter requestQueue manager realm aucfile 
data ReqParams = ReqAuc     Config Realm 
               | ReqRealms  Config
               -- | ReqAucJson (MVar Int) (MVar (S.Seq ReqParams  )) C.Manager AucFile Realm

data DLParams = DLAucJson AucFile Realm

instance FromJSON AucFile where
    parseJSON = withObject "file" $ \o -> do 
        url <- o .: "url"
        lastModified <- o .: "lastModified"
        return AucFile{..}

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
           locale <- o .: "locale"
           connectedRealms <- o .: "connected_realms"
           return Realm{..}