module BinaryTreeSpec (spec) where

import Test.Hspec
import BinaryTree

spec :: Spec
spec = do
  let testTree = insert 1 $ insert 2 $ insert 8 $ insert 6 $ insert 5 $ insert 7 $ insert 9 $ insert 4 Empty 

  describe "insert" $ do
    it "empty" $ do
      insert 1 Empty `shouldBe` (BinaryTree 1 Empty Empty)

    it "many elements" $ do
      (insert 2 $ insert 3 $ insert 1 Empty) `shouldBe`
        (BinaryTree 1 Empty (BinaryTree 3 (BinaryTree 2 Empty Empty) Empty))

  describe "member" $ do
    it "empty" $ do
      member 0 Empty `shouldBe` False

    it "one element" $ do
      member 0 (BinaryTree 0 Empty Empty) `shouldBe` True

    it "many elements" $ do
      member 5 testTree `shouldBe` True

  describe "findMin" $ do
    it "many elements" $ do
      findMin testTree `shouldBe` 1

  describe "findMax" $ do
    it "many elements" $ do
      findMax testTree `shouldBe` 9

  describe "size" $ do
    it "empty" $ do
      size (Empty :: BinaryTree Int) `shouldBe` 0

    it "many elements" $ do
      size testTree `shouldBe` 8
