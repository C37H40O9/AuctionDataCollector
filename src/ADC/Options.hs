module ADC.Options (options
                  ,ADCOptions(..))
where

import Options.Applicative
import Data.Semigroup ((<>))

data ADCOptions = ADCOptions
  {config :: String
  ,daemon :: Bool}

adcoptions :: Parser ADCOptions
adcoptions = ADCOptions
          <$> strOption
              ( long "config"
             <> short 'c'
             <> help "Configuration file"
             <> showDefault
             <> value "./config.cfg" )
          <*> switch
              ( long "daemon"
             <> short 'd'
             <> help "Run as daemon" )

options = execParser opts
  where
    opts = info (adcoptions <**> helper)
      (fullDesc
     <>progDesc "Collect prices from wow auctions"
     <>header "Auction Data Collector")