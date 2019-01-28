{-# LANGUAGE FlexibleContexts #-}
module Ch21.SkiFreeSpec where

import           Ch21.SkiFree

import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

instance (Functor n , Arbitrary (n a) , Arbitrary a) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where (=-=) = eq

spec :: Spec
spec =
    describe "Ski Free - S n a" $ do
    it "Functor" $
        hspec $ testBatch $ functor (undefined :: S [] (Int, Int, [Int]))
    it "Traversable" $
        hspec $ testBatch $ traversable (undefined :: S [] (Int, Int, [Int]))
