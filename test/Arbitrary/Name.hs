module Arbitrary.Name
  ( NameString(..)
  , arbitraryName
  )
where

import           Data.Char                      ( isSpace )
import           Test.QuickCheck                ( Arbitrary(arbitrary, shrink)
                                                , Gen
                                                , arbitraryPrintableChar
                                                , listOf
                                                , suchThat
                                                )

newtype NameString = NameString { getNameString :: String }

instance Arbitrary NameString where
  arbitrary = NameString <$> arbitraryName
  shrink (NameString s) = NameString <$> filter ('\n' `notElem`) (shrink s)

arbitraryName :: Gen String
arbitraryName = listOf (arbitraryPrintableChar `suchThat` (not . isSpace) `suchThat` (/= '@'))
