{-# LANGUAGE TypeApplications #-}

module Task.InternalSpec
  ( main
  , spec
  )
where

import           Data.List                      ( nub )
import qualified FileSystem                    as FS
                                                ( fromName )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , hspec
                                                , it
                                                , parallel
                                                , shouldBe
                                                )
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck                ( Positive(Positive) )

import           Task.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "venn" $ do
    prop "venn A B | B > A" $ \a b ->
      let a' = filter (`notElem` b') $ nub a
          b' = nub b
      in  venn @Int a' (a' ++ b') `shouldBe` ([], a', filter (`notElem` a') b')
    prop "venn A B | B < A" $ \a b ->
      let a' = nub a
          b' = filter (`notElem` a') $ nub b
      in  venn @Int (a' ++ b') b' `shouldBe` (filter (`notElem` b') a', b', [])
    prop "venn A B | A - B == A, B - A == ø" $ \(Positive n) ->
      let ls = filter even [0 .. n]
          rs = filter odd [0 .. n]
      in  venn @Int ls rs `shouldBe` (ls, [], rs)
  describe "dropFromName" $ do
    it "doesn't strip suffixes"
      $ let remote = FS.fromName ""
            fs     = FS.fromName "a/"
        in  dropFromName remote fs `shouldBe` fs
    it "drops matching dataset"
      $ let remote = FS.fromName "a/b" in dropFromName remote (FS.fromName "a/b/c") `shouldBe` FS.fromName "c"
    it "idempotent"
      $ let remote = FS.fromName "a/b"
        in  dropFromName remote (dropFromName remote (FS.fromName "a/b/c")) `shouldBe` FS.fromName "c"
