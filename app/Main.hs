module Main where

import ADC.Lib
import ADC.Config
import ADC.DB
import ADC.Types.Types
import Data.Pool
import Control.Concurrent
import Control.Monad (forever)
import ADC.Options


main :: IO ()
main = do
  opts <- options
  cfg <- readCfg $ config opts
  withResource (connPool cfg) initMigrations
  loadLastModified cfg
  forkIO $ forever $ updAucJson cfg
  forkIO $ forever $ do 
    addReqToQ cfg (ReqRealms cfg)
    threadDelay $ 120 * oneSecond
  forever $ do
    forkIO $ runJob cfg
    threadDelay oneSecond