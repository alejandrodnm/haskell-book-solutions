module Ch20.ExercisesSpec where

import           Ch20.Exercises
import           Data.Monoid    (Sum)
import           Test.Hspec

spec :: Spec
spec = do
    describe "Constant a b" $
        it "Foldable" $
            foldMap (+1) (Constant 2 :: Constant String (Sum Integer))
                `shouldBe` 3
