module Ch15.Optional where

import           Data.Monoid

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
  Nada <> Nada         = Nada
  Nada <> (Only x)     = Only x
  (Only x) <> Nada     = Only x
  (Only x) <> (Only y) = Only (x <> y)
