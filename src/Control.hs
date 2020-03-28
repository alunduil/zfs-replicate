module Control
  ( die
  )
where

import           Control.Monad.Extra            ( (<=<) )
import           Prelude                        ( IO
                                                , String
                                                , const
                                                , putStrLn
                                                )
import           System.Exit                    ( exitFailure )

die :: String -> IO a
die = const exitFailure <=< putStrLn
