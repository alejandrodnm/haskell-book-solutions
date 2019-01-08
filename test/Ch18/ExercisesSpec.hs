module Ch18.ExercisesSpec where

import           Ch17.List
import           Ch17.ListSpec
import           Ch18.Exercises
import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp (..), eq)
import           Test.QuickCheck.Classes

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance (Arbitrary b, Arbitrary a) => Arbitrary (Either' b a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Left' a, Right' b]

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance (Eq a, Eq b) => EqProp (Either' b a) where
    (=-=) = eq

instance EqProp (Nope a) where
    (=-=) = eq

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "Nope a" $ do
        it "Functor" $
            hspec $ testBatch $ functor (undefined :: Nope (String, Int, Int))
        it "Applicative" $
            hspec $ testBatch $ applicative (undefined :: Nope (String, Int, Int))
        it "Monad" $
            hspec $ testBatch $ monad (undefined :: Nope (String, Int, Int))
    describe "Either' b a" $ do
        it "Functor" $
            hspec $ testBatch $ functor (undefined :: Either' String (String, Int, Int))
        it "Applicative" $
            hspec $ testBatch $ applicative (undefined :: Either' String (String, Int, Int))
        it "Monad" $
            hspec $ testBatch $ monad (undefined :: Either' String (String, Int, Int))
    describe "Identity a" $ do
        it "Functor" $
            hspec $ testBatch $ functor (undefined :: Identity (String, Int, Int))
        it "Applicative" $
            hspec $ testBatch $ applicative (undefined :: Identity (String, Int, Int))
        it "Monad" $
            hspec $ testBatch $ monad (undefined :: Identity (String, Int, Int))
    describe "List a" $
        it "Monad" $
            hspec $ testBatch $ monad (undefined :: List (String, Int, Int))
    describe "j" $ do
        it "j [[1, 2], [], [3]] == [1, 2, 3]" $
            j [[1, 2], [], [3]] `shouldBe` [1, 2, 3]
        it "j (Just (Just 1)) == Just 1" $
            j (Just (Just 1)) `shouldBe` Just 1
        it "j Nothing" $
            j Nothing `shouldBe` (Nothing :: Maybe Int)
        it "j (Just Nothing) == Nothing" $
            j (Just Nothing) `shouldBe` (Nothing :: Maybe Int)
