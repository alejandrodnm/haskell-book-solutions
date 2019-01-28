module Ch21.ExercisesSpec where

import           Ch17.Identity
import           Ch21.Exercises
import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp (..), eq)
import           Test.QuickCheck.Classes

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

spec :: Spec
spec =
    it "Traversable Identity" $
        hspec $ testBatch $ traversable (undefined :: Identity (Int, Int, String))
