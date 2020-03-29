module Arbitrary.Name
  ( arbitraryName
  )
where

import           Data.Char                      ( isSpace )
import           Test.QuickCheck                ( Gen
                                                , arbitraryPrintableChar
                                                , listOf
                                                , suchThat
                                                )

arbitraryName :: Gen String
arbitraryName = listOf $ arbitraryPrintableChar `suchThat` (not . isSpace) `suchThat` (/= '@')
