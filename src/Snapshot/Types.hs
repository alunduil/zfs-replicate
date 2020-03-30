{-# LANGUAGE DeriveGeneric #-}

module Snapshot.Types where

import           Data.List                      ( isSuffixOf )
import qualified FileSystem                    as FS
                                                ( FileSystem(name) )
import           GHC.Generics                   ( Generic )

data Snapshot = Snapshot
              { fileSystem :: FS.FileSystem
              , name :: String
              , previous :: Maybe Snapshot
              , timestamp :: Int
              } deriving (Generic, Show)

instance Eq Snapshot where
  x == y = isSuffix && name x == name y && timestamp x == timestamp y
   where
    isSuffix = yName `isSuffixOf` xName || xName `isSuffixOf` yName
    xName    = FS.name $ fileSystem x
    yName    = FS.name $ fileSystem y
