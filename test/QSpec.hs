module QSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

half :: Fractional a => a -> a
half x = x / 2

spec :: Spec
spec =
  describe "half" $
    it "is inverse of halfIdentity " $ property $
      \x -> ((*2) . half) x == (x :: Double)
