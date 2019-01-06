module Ch17.ListSpec where

import           Ch17.List
import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp (..), eq)
import           Test.QuickCheck.Classes

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = sized list

list :: (Arbitrary a) => Int -> Gen (List a)
list 0 = return Nil
list n | n > 0 = do
    a <- arbitrary
    l <- list (n `div` 4)
    return $ Cons a l

instance Eq a => EqProp (List a) where
    (=-=) = eq

spec :: Spec
spec =
    describe "List a" $ do
        it "Monoid" $
            hspec $ testBatch $ monoid (undefined :: List String)
        it "Functor" $
            hspec $ testBatch $ functor (undefined :: List (String, Int, Int))
        it "Applicative" $
            hspec $ testBatch $ applicative (undefined :: List (String, Int, Int))
