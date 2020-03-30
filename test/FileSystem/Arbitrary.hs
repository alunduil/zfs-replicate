{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module FileSystem.Arbitrary
  ()
where

import           Arbitrary.Name                 ( NameString(NameString)
                                                , arbitraryName
                                                )
import           Data.List                      ( isPrefixOf )
import           FileSystem.Types
import           Test.QuickCheck                ( Arbitrary(arbitrary, shrink)
                                                , suchThat
                                                )

instance Arbitrary FileSystem where
  arbitrary = do
    ro <- arbitrary
    fs <- fromName <$> arbitraryName `suchThat` (not . ("/" `isPrefixOf`))
    return $ fs { readonly = ro }

  shrink FileSystem {..} =
    [ (fromName name') { readonly = readonly' }
    | (NameString name', readonly') <- shrink (NameString name, readonly)
    , not $ null name'
    ]
