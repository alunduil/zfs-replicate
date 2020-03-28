{-# LANGUAGE RecordWildCards #-}

module Snapshot.Internal where

import           Control.Arrow                  ( first
                                                , second
                                                )
import qualified FileSystem                    as FS
                                                ( fromName )
import           Prelude                        ( (/=)
                                                , (==)
                                                , ($)
                                                , Maybe(..)
                                                , String
                                                , head
                                                , lines
                                                , not
                                                , null
                                                , otherwise
                                                , read
                                                , span
                                                , tail
                                                , zipWith
                                                )
import           Snapshot.Types

snapshots :: String -> [Snapshot]
snapshots ""    = []
snapshots input = head ss : zipWith addPrevious (tail ss) ss
  where ss = [ snapshot line | line <- lines input, not (null line) ]

snapshot :: String -> Snapshot
snapshot line = Snapshot { .. }
 where
  previous                 = Nothing
  (prefix    , timestamp ) = second read $ span (/= '\t') line
  (fileSystem, '@' : name) = first FS.fromName $ span (/= '@') prefix

addPrevious :: Snapshot -> Snapshot -> Snapshot
addPrevious s p | fileSystem s == fileSystem p = s { previous = Just p }
                | otherwise                    = s { previous = Nothing }
