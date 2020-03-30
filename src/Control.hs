module Control
  ( die
  )
where

import           Control.Monad.Extra            ( (<=<) )
import           System.Exit                    ( exitFailure )

die :: String -> IO a
die = const exitFailure <=< putStrLn
