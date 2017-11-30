{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Config (readCfg)
    where

import Types.Types
import Types.Locale
import Data.Configurator
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M
import qualified Network.HTTP.Conduit as C
import Control.Concurrent.MVar
import Data.Time.Clock
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Database.PostgreSQL.Simple
import Data.Pool

readCfg :: FilePath -> IO Config
readCfg fp = do
    conf <- load [Required fp ] --"./config.cfg"]
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
    pure cfg

