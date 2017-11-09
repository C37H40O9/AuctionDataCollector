{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Locale ( Locale(..) )
    where

import Prelude hiding (Applicative(..), print)
import Text.Syntax
import Text.Syntax.Parser.Naive
import Text.Syntax.Printer.Naive
import qualified Data.Aeson.Types  as AT                        ( Parser )
import Data.Aeson
import qualified Data.Text.Lazy as TL
import qualified Control.Applicative as CA                      ( empty, pure )
import Data.Maybe (fromJust)

data Locale = DE_DE
            | EN_GB
            | ES_ES
            | FR_FR
            | IT_IT
            | PT_PT
            | RU_RU
            | KO_KR
            | ZH_TW
            | EN_US
            | PT_BR
            | ES_MX deriving (Eq, Ord)


pLocale :: Syntax f => f Locale
pLocale =  pure DE_DE <* text "de_DE"
   <|> pure EN_GB <* text "en_GB"
   <|> pure ES_ES <* text "es_ES"
   <|> pure FR_FR <* text "fr_FR"
   <|> pure IT_IT <* text "it_IT"
   <|> pure PT_PT <* text "pt_PT"
   <|> pure RU_RU <* text "ru_RU"
   <|> pure KO_KR <* text "ko_KR"
   <|> pure ZH_TW <* text "zh_TW"
   <|> pure EN_US <* text "en_US"
   <|> pure PT_BR <* text "pt_BR"
   <|> pure ES_MX <* text "es_MX"

runParser :: Parser t -> String -> [(t, String)]
runParser (Parser p) = p

instance Read Locale where readsPrec _ = runParser pLocale

instance Show Locale where show = fromJust . print pLocale

instance FromJSON Locale where
    parseJSON (String t) =  fromString (TL.unpack (TL.fromStrict t))
        where fromString :: String -> AT.Parser Locale
              fromString s = CA.pure (read s :: Locale)
    parseJSON _ = CA.empty