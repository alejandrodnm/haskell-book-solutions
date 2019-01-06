module Ch17.ExercisesSpec where

import           Ch16.Functor
import           Ch16.FunctorSpec
import           Ch17.Exercises
import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp (..), eq)
import           Test.QuickCheck.Classes

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "Pair a" $
        it "Applicative" $
            hspec $ testBatch $ applicative (undefined :: Pair (String, Int, Int))
    describe "Two a b" $
        it "Applicative" $
            hspec $ testBatch $ applicative (undefined :: Two String (String, Int, Int))
    describe "Three' a b" $
        it "Applicative" $
            hspec $ testBatch $ applicative (undefined :: Two String (String, Int, Int))
