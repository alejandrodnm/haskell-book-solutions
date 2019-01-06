module Ch17.ValidationSpec where

import           Ch17.Validation
import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp (..), eq)
import           Test.QuickCheck.Classes

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        elements [Ch17.Validation.Failure e, Ch17.Validation.Success a]

instance (Eq a, Eq e) => EqProp (Validation e a) where
    (=-=) = eq

spec :: Spec
spec =
    describe "Validation e a" $ do
        it "Functor" $
            hspec $ testBatch $ functor (undefined :: Validation String (String, Int, Int))
        it "Applicative" $
            hspec $ testBatch $ applicative (undefined :: Validation String (String, Int, Int))
