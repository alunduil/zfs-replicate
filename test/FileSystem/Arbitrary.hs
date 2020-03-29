{-# OPTIONS_GHC -fno-warn-orphans #-}

module FileSystem.Arbitrary
  ()
where

import           Arbitrary.Name                 ( arbitraryName )
import           FileSystem.Types
import           Test.QuickCheck                ( Arbitrary(arbitrary, shrink)
                                                , genericShrink
                                                , suchThat
                                                )

instance Arbitrary FileSystem where
  arbitrary = do
    ro <- arbitrary
    fs <- fromName <$> (arbitraryName `suchThat` (not . null))
    return $ fs { readonly = ro }

  shrink = genericShrink
