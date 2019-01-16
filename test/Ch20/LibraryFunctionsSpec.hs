module Ch20.LibraryFunctionsSpec where

import           Ch20.LibraryFunctions
import           Test.Hspec

spec :: Spec
spec = do
    describe "sum" $ do
        it "adds [Num]" $
            sum' [1, 2] `shouldBe` 3
        it "adds Just Nums" $
            sum' (Just 3) `shouldBe` 3
        it "adds Nothing" $
            sum' Nothing `shouldBe` 0
    describe "product" $ do
        it "multiplies [Num]" $
            product' [3, 2] `shouldBe` 6
        it "multiplies Just Nums" $
            product' (Just 3) `shouldBe` 3
        it "multiplies Nothing" $
            product' Nothing `shouldBe` 1
    describe "elem" $ do
        it "found in []" $
            elem' 2 [3, 2] `shouldBe` True
        it "not found in []" $
            elem' 1 [3, 2] `shouldBe` False
        it "found in Just" $
            elem' 3 (Just 3) `shouldBe` True
        it "not found in Just" $
            elem' 3 (Just 2) `shouldBe` False
        it "multiplies Nothing" $
            elem' 1 Nothing `shouldBe` False
    describe "minimun'" $ do
        it "found minimum" $
            minimun' ([2, 1, 3] :: [Integer]) `shouldBe` Just 1
        it "empty list is Nothing" $
            minimun' ([] :: [Integer]) `shouldBe` Nothing
        it "Just Num is Just Num" $
            minimun' (Just 1) `shouldBe` Just 1
        it "Nothing is Nothing" $
            minimun' (Nothing :: Maybe Integer) `shouldBe` Nothing
    describe "length'" $ do
        it "length of [Int]" $
            length' [1, 2, 3] `shouldBe` 3
        it "length of [] is 0" $
            length' [] `shouldBe` 0
        it "length of Just is 1" $
            length' (Just 3) `shouldBe` 1
        it "length of Nothing is 0" $
            length' Nothing `shouldBe` 0
