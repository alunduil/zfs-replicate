{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snapshot.Arbitrary
  ()
where

import           Arbitrary.Name                 ( arbitraryName )
import qualified FileSystem                    as FS
                                                ( fromName )
import           FileSystem.Arbitrary           ( )
import           Snapshot.Types                 ( Snapshot(..) )
import           Test.QuickCheck                ( Arbitrary(arbitrary, shrink)
                                                , genericShrink
                                                )

instance Arbitrary Snapshot where
  arbitrary = do
    fileSystem <- FS.fromName <$> arbitraryName
    name       <- arbitraryName
    let previous = Nothing
    timestamp <- arbitrary
    return Snapshot { .. }

  shrink = genericShrink
