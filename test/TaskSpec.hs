module TaskSpec
  ( main
  , spec
  )
where

import           Control.Monad                  ( liftM2 )
import           Data.Map.Strict                ( elems
                                                , empty
                                                , size
                                                )
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )
import qualified FileSystem                    as FS
                                                ( fromName )
import           Snapshot.Arbitrary             ( )
import           Task.Types
import           Test.Hspec                     ( Spec
                                                , context
                                                , describe
                                                , hspec
                                                , it
                                                , parallel
                                                , shouldBe
                                                )
import           Test.Hspec.QuickCheck          ( prop )

import           Task

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ describe "fromSnapshots" $ do
  it "fromSnapshots (FS.fromName \"\") empty empty False == []"
    $ let result = fromSnapshots (FS.fromName "") empty empty False in result `shouldBe` []
  context "empty remote snapshots" $ do
    prop "create count matches filesystem count" $ \ss ->
      let p      = ((== Create) . action) .&&. (isNothing . snapshot)
          result = fromSnapshots (FS.fromName "") ss empty False
      in  length (filter p result) `shouldBe` size ss
    prop "send count matches sum of snapshot counts" $ \ss ->
      let p      = ((== Send) . action) .&&. (isJust . snapshot)
          result = fromSnapshots (FS.fromName "") ss empty False
      in  length (filter p result) `shouldBe` length (concat $ elems ss)
  context "empty local snapshots" $ do
    prop "destroy count matches filesystem count plus snapshot counts" $ \ss ->
      let p      = (== Destroy) . action
          result = fromSnapshots (FS.fromName "") empty ss False
      in  length (filter p result) `shouldBe` size ss + length (concat $ elems ss)
    prop "only destroy actions" $ \ss -> all ((== Destroy) . action) $ fromSnapshots (FS.fromName "") empty ss False

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) = liftM2 (&&)
