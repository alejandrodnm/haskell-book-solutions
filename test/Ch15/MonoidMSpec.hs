module Ch15.MonoidMSpec (spec) where

import           Ch15.MonoidM
import           Ch15.Optional
import           Test.Hspec
import           Test.QuickCheck

genOnly :: Arbitrary a => Gen (Optional a)
genOnly = Only <$> arbitrary

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency
    [ (1, return Nada)
    , (10, genOnly)
    ]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    return First' { getFirst' = a }

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


spec :: Spec
spec =
  describe "Monoid Custom Maybe" $ do
    it "Assoc String" $
      property (monoidAssoc :: FirstMappend)
    it "Left identity" $
      property (monoidLeftIdentity :: FstId)
    it "Right identity" $
      property (monoidRightIdentity :: FstId)
