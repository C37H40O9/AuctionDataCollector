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
import Control.Concurrent.STM.TChan
import Data.Time.Clock
import qualified Data.Map.Strict as M
import Prelude hiding (Applicative(..), print)
import Data.Maybe (fromJust)
import Text.Syntax
import Text.Syntax.Parser.Naive
import Text.Syntax.Printer.Naive
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.Text.Lazy as TL
import qualified Control.Applicative as CA                      ( empty, pure )
import qualified Data.Aeson.Types  as AT                        ( Parser )
import Data.Text                                 ( pack )

                -- Type for whiskers box diagram
data WBox = WBox { ic   :: Int  -- items count
                 , minW :: Int
                 , botW :: Int
                 , p25  :: Int
                 , p50  :: Int
                 , p75  :: Int
                 , topW :: Int
                 , maxW :: Int } deriving (Eq, Show)

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

data Locale = DE_DE
            | EN_GB
            | ES_ES
            | FR_FR
            | IT_IT
            | PT_BR
            | RU_RU deriving (Eq, Ord)


pLocale :: Syntax f => f Locale
pLocale =  pure DE_DE <* text "de_DE"
       <|> pure EN_GB <* text "en_GB"
       <|> pure ES_ES <* text "es_ES"
       <|> pure FR_FR <* text "fr_FR"
       <|> pure IT_IT <* text "it_IT"
       <|> pure PT_BR <* text "pt_BR"
       <|> pure RU_RU <* text "ru_RU"
    
runParser (Parser p) = p

instance Read Locale where readsPrec _ = runParser pLocale

instance Show Locale where show = fromJust . print pLocale

instance FromJSON Locale where
    parseJSON (String t) =  fromString (TL.unpack (TL.fromStrict t))
        where fromString :: String -> AT.Parser Locale
              fromString s = CA.pure (read s :: Locale)
    parseJSON _ = CA.empty

data Realm = Realm
             { rname :: String
             , slug :: Slug
             , locale :: Locale
             , connectedRealms :: [Slug]} deriving (Eq, Show)



data AucFile = AucFile { url          :: String
                       , lastModified :: Integer} deriving (Eq, Show)

-- counter requestQueue manager realm aucfile 
data ReqParams = ReqAuc     ApiKey (MVar Int) (MVar (S.Seq ReqParams  )) C.Manager  (TChan DLParams ) Realm 
               | ReqRealms  ApiKey (MVar Int) (MVar (S.Seq ReqParams  )) C.Manager (TChan DLParams )
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
