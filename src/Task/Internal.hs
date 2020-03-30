{-# LANGUAGE OverloadedStrings #-}

module Task.Internal where

import           Data.Maybe                     ( fromMaybe )
import           Data.List                      ( intersect
                                                , stripPrefix
                                                )
import qualified FileSystem                    as FS
                                                ( FileSystem(name, readonly)
                                                , fromName
                                                )

dropFromName :: FS.FileSystem -> FS.FileSystem -> FS.FileSystem
dropFromName remote fs = (FS.fromName name') { FS.readonly = FS.readonly fs }
 where
  name' = fromMaybe name $ stripPrefix (FS.name remote ++ "/") name
  name  = FS.name fs

venn :: (Eq a) => [a] -> [a] -> ([a], [a], [a])
venn lefts rights = (ls, ms, rs)
 where
  ls = filter (`notElem` ms) lefts
  ms = rights `intersect` lefts
  rs = filter (`notElem` ms) rights
