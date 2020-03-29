module Snapshot
  ( list
  , ListOptions(..)
  , listOptions
  , group
  )
where

import           Data.Map.Lazy                  ( Map
                                                , fromListWith
                                                )
import qualified FileSystem                    as FS
                                                ( FileSystem )
import           Snapshot.List
import           Snapshot.Types

group :: [Snapshot] -> Map FS.FileSystem [Snapshot]
group ss = fromListWith (++) [ (Snapshot.Types.fileSystem s, [s]) | s <- ss ]
