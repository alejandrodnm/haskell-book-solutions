module Ch15.MonoidSpec where

import           Ch15.Monoid
import           Control.Applicative
import           Data.Monoid
import           Test.Hspec
import           Test.QuickCheck

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
  arbitrary = return Trivial

type IdentityAssoc =
  Identity String -> Identity String -> Identity String -> Bool

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type TwoAssoc =
  Two String (Identity String) ->
    Two String (Identity String) ->
      Two String (Identity String) -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

type OrAssoc =
  Or String (Identity String) ->
    Or String (Identity String) ->
      Or String (Identity String) -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

type ValidationAssoc
   = Validation String String
  -> Validation String String
  -> Validation String String
  -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Ch15.Monoid.Failure a,  Ch15.Monoid.Success b]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type CombineAssoc = Fun Int (Sum Int) -> Fun Int (Sum Int) -> Fun Int (Sum Int) -> Bool

semigroupFunAssoc :: (Eq b, Semigroup m)
                  => (Fun a b -> m)
                  -> (m -> a -> b)
                  -> a
                  -> Fun a b
                  -> Fun a b
                  -> Fun a b
                  -> Bool
semigroupFunAssoc wrap eval point a b c =
  eval (a' <> (b' <> c')) point == eval ((a' <> b') <> c') point
    where
      a' = wrap a
      b' = wrap b
      c' = wrap c

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

spec :: Spec
spec =
  describe "Semi groups" $ do
    it "Trivial" $
      property (semigroupAssoc :: TrivAssoc)
    it "Identity" $
      property (semigroupAssoc :: IdentityAssoc)
    it "Two" $
      property (semigroupAssoc :: TwoAssoc)
    it "Or" $
      property (semigroupAssoc :: OrAssoc)
    it "Combine" $
      property (semigroupFunAssoc (Combine . applyFun) unCombine :: Int -> CombineAssoc)
    it "Validation" $
      property (semigroupAssoc :: ValidationAssoc)
