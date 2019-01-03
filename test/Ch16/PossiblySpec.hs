module Ch16.PossiblySpec where

import           Ch16.FunctorLaws
import           Ch16.Possibly
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary a => Arbitrary (Possibly a) where
    arbitrary = do
        a <- arbitrary
        elements [ Yeppers a, LolNope ]

type PossiblyCompose
    = Fun String Integer
   -> Fun Integer Integer
   -> Possibly String
   -> Bool

spec :: Spec
spec =
    describe "Functor instances" $
        context "Possibly a" $ do
            it "Identity" $
                property (functorIdentity :: Possibly String -> Bool)
            it "Compose" $
                property (functorCompose' :: PossiblyCompose)
