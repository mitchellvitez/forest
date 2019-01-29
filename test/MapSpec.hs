module MapSpec (spec) where

import Test.Hspec
import Map
import BinaryTree (BinaryTree(Empty), size)
import Prelude hiding (lookup)

spec :: Spec
spec = do
  let testMap = bind 3 4 $ bind 5 6 $ bind 8 9 $ bind 0 1 empty
  describe "empty" $ do
    it "can create empty map" $ do
      (empty :: Map Int Int) `shouldBe` Empty

  describe "bind" $ do
    it "binds keys and values" $ do
      bind 0 1 empty `shouldNotBe` empty

    it "binds multiple keys and values" $ do
      size testMap `shouldBe` 4
  
  describe "lookup" $ do
    it "finds a key that is there" $ do
      lookup 8 testMap `shouldBe` Just 9

    it "doesn't find a key that isn't there" $ do
      lookup 7 testMap `shouldBe` Nothing
