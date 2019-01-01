module Ch15.MonoidM where

import           Ch15.Optional

newtype First' a =
    First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
    mempty = First' { getFirst' = Nada }

instance Semigroup (First' a) where
    (<>) (First' Nada) (First' Nada)         = First' Nada
    (<>) (First' (Only x)) (First' Nada)     = First' (Only x)
    (<>) (First' Nada) (First' (Only x))     = First' (Only x)
    (<>) (First' (Only x)) (First' (Only y)) = First' (Only x)
