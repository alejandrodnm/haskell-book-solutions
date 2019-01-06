module Ch17.ZipListSpec where

import           Ch17.ListSpec
import           Ch17.ZipList
import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Checkers (EqProp (..), eq)
import           Test.QuickCheck.Classes

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
      where
        xs' =
            let (ZipList' l) = xs
             in take' 1000 l
        ys' =
            let (ZipList' l) = ys
             in take' 1000 l

spec :: Spec
spec =
    describe "ZipList a" $ do
        it "Functor" $
            hspec $ testBatch $ functor (undefined :: ZipList' (String, Int, Int))
        it "Applicative" $
            hspec $ testBatch $ applicative (undefined :: ZipList' (String, Int, Int))
