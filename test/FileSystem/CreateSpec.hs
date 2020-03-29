{-# LANGUAGE RecordWildCards #-}

module FileSystem.CreateSpec
  ( main
  , spec
  )
where

import           FileSystem.Arbitrary           ( )
import           FileSystem.Types
import           System.FilePath                ( joinPath )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , hspec
                                                , it
                                                , parallel
                                                , shouldBe
                                                )
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck                ( Positive(Positive) )

import           FileSystem.Create

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "ancestors" $ do
    it "ancestors \"\" == []" $ ancestors "" `shouldBe` []
    it "ancestors \"a\" == [\"a\"]" $ ancestors "a/a" `shouldBe` ["a", "a/a"]
    it "ancestors \"a/a\" == [\"a\", \"a/a\"]" $ ancestors "a/a" `shouldBe` ["a", "a/a"]
    prop "length (ancestors $ replicate n \"a\") == n"
      $ \(Positive n) -> length (ancestors $ joinPath $ replicate n "a") == n
  describe "filesystems" $ prop "id == filesystems . unlines . output" $ \fss ->
    filesystems (unlines (output <$> fss)) `shouldBe` fss
  describe "filesystem" $ prop "id == filesystem . output" $ \fs -> filesystem (output fs) `shouldBe` fs

output :: FileSystem -> String
output FileSystem {..} = name ++ "\t" ++ if readonly then "on" else "off"
