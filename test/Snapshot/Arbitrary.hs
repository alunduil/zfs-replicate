{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snapshot.Arbitrary
  ()
where

import           Data.Char                      ( isSpace )
import qualified FileSystem                    as FS
                                                ( FileSystem(name)
                                                , fromName
                                                )
import           Prelude                        ( (.)
                                                , (/=)
                                                , (<$>)
                                                , ($)
                                                , Maybe(Nothing)
                                                , not
                                                , return
                                                )
import           Snapshot.Types                 ( Snapshot(..) )
import           Test.QuickCheck                ( Arbitrary(arbitrary, shrink)
                                                , arbitraryPrintableChar
                                                , listOf
                                                , suchThat
                                                )

instance Arbitrary Snapshot where
  arbitrary = do
    fileSystem <- FS.fromName <$> arbitraryName
    name       <- arbitraryName
    let previous = Nothing
    timestamp <- arbitrary
    return Snapshot { .. }
    where arbitraryName = listOf $ arbitraryPrintableChar `suchThat` (not . isSpace) `suchThat` (/= '@')

  shrink Snapshot {..} =
    [ Snapshot (FS.fromName fileSystem') name' previous' timestamp'
    | (fileSystem', name', previous', timestamp') <- shrink (FS.name fileSystem, name, previous, timestamp)
    ]
