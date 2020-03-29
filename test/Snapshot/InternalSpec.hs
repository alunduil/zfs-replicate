{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snapshot.InternalSpec
  ( main
  , spec
  )
where

import qualified FileSystem                    as FS
                                                ( FileSystem(name)
                                                , fromName
                                                )
import           Snapshot.Arbitrary             ( )
import           Snapshot.Types                 ( Snapshot(..) )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , hspec
                                                , it
                                                , parallel
                                                , shouldBe
                                                )
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck                ( NonEmptyList(NonEmpty) )


import           Snapshot.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "snapshots" $ do
    prop "length (snapshots . unlines . output) ss == length ss"
      $ \ss -> length (snapshots $ unlines (output <$> ss)) `shouldBe` length ss
    prop "id == snapshots . unlines . output" $ \ss -> snapshots (unlines (output <$> ss)) `shouldBe` ss
    prop "max depth <= 2"
      $ \(NonEmpty (ss :: [Snapshot])) -> maximum (depth <$> snapshots (unlines (output <$> ss))) <= 2
  describe "snapshot" $ do
    it "snapshot \"@\\t0\" == Snapshot (FS.fromName \"\") \"\" Nothing 0" $ snapshot "@\t0" `shouldBe` Snapshot
      { fileSystem = FS.fromName ""
      , name       = ""
      , previous   = Nothing
      , timestamp  = 0
      }

    prop "id == snapshot . output" $ \s -> s == snapshot (output s)

depth :: Snapshot -> Int
depth Snapshot { previous = Nothing }  = 1
depth Snapshot { previous = (Just p) } = 1 + depth p

output :: Snapshot -> String
output Snapshot {..} = FS.name fileSystem ++ "@" ++ name ++ "\t" ++ show timestamp
