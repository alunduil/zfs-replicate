{-# LANGUAGE DeriveGeneric #-}

module FileSystem.Types where

import           GHC.Generics                   ( Generic )
import           Prelude                        ( (/=)
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
fromName n = FileSystem { dataset = takeWhile (/= '/') n, name = n, readonly = False }
