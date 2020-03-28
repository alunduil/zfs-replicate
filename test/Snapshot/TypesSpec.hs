module Snapshot.TypesSpec
  ( main
  , spec
  )
where

import qualified FileSystem                    as FS
                                                ( fromName )
import           Prelude                        ( ($)
                                                , IO
                                                , Maybe(..)
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , hspec
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

import           Snapshot.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ describe "Eq" $ it "ignore previous in Eq" $ do
  let s = Snapshot { fileSystem = FS.fromName "", name = "", previous = Nothing, timestamp = 0 }
  s `shouldBe` s { previous = Just s }
