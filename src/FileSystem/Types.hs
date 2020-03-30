{-# LANGUAGE DeriveGeneric #-}

module FileSystem.Types where

import           Data.List                      ( stripPrefix )
import           Data.Maybe                     ( fromMaybe )
import           GHC.Generics                   ( Generic )
import           Prelude                        ( (/=)
                                                , ($)
                                                , Bool(False)
                                                , Eq
                                                , Ord
                                                , Show
                                                , String
                                                , takeWhile
                                                )

data FileSystem = FileSystem
                { dataset :: String
                , name :: String
                , readonly :: Bool
                } deriving (Eq, Generic, Ord, Show)

fromName :: String -> FileSystem
fromName n = FileSystem { dataset = takeWhile (/= '/') n', name = n', readonly = False }
  where n' = fromMaybe n $ stripPrefix "/" n
