module Main where

import Lib
import Config
import DB
import Types.Types
import Data.Pool
import Control.Concurrent
import Control.Monad (forever)


main :: IO ()
main = do
  cfg <- readCfg "./config.cfg"
  withResource (connPool cfg) initMigrations
  loadLastModified cfg
  forkIO $ forever $ updAucJson cfg
  forkIO $ forever $ do 
    addReqToQ cfg (ReqRealms cfg)
    threadDelay $ 120 * oneSecond
  forever $ do
    forkIO $ runJob cfg
    threadDelay oneSecond