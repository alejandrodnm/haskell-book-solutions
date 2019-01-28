{-# LANGUAGE FlexibleContexts #-}
module Ch21.TreeSpec where

import           Ch21.Tree

import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- probably not the best distribution for a tree.
-- Should probably have it have a certain depth similar to []
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = do
        a  <- arbitrary
        t  <- arbitrary
        t' <- arbitrary
        frequency [(1, return Empty),
                   (3, return $ Leaf a),
                   (3, return $ Node t a t')]
instance (Eq a) => EqProp (Tree a) where (=-=) = eq

spec :: Spec
spec =
    it "Tree" $
        hspec $ testBatch $ traversable (undefined :: Tree (Int, Int, [Int]))
