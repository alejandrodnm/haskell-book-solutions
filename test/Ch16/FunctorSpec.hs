{-# LANGUAGE FlexibleInstances #-}

module Ch16.FunctorSpec where

import           Ch16.Functor
import           Ch16.FunctorLaws
import           Control.Applicative
import           Test.Hspec
import           Test.QuickCheck


instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = liftA2 Pair arbitrary arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = liftA2 Two arbitrary arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
    arbitrary = K <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K a b) where
    arbitrary = Flip <$> arbitrary

instance (Arbitrary a) => Arbitrary (LiftItOut Identity a) where
    arbitrary = LiftItOut . Identity <$> arbitrary

instance (Arbitrary a) => Arbitrary (Parappa Identity [] a) where
    arbitrary = do
        a <- arbitrary
        return (DaWrappa (Identity a) [a])

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = sized list

list :: (Arbitrary a) => Int -> Gen (List a)
list 0 = return Nil
list n | n > 0 = do
    a <- arbitrary
    l <- list (n `div` 4)
    return $ Cons a l

type IdentityCompose
    = Fun String Integer
   -> Fun Integer Integer
   -> Identity String
   -> Bool

type PairCompose
    = Fun String Integer
   -> Fun Integer Integer
   -> Pair String
   -> Bool

type TwoCompose
    = Fun String Integer
   -> Fun Integer Integer
   -> Two Integer String
   -> Bool

type ThreeCompose
    = Fun String Integer
   -> Fun Integer Integer
   -> Three' Integer String
   -> Bool

type KCompose
    = Fun String Integer
   -> Fun Integer Integer
   -> K String String
   -> Bool

type FlipCompose
    = Fun String Integer
   -> Fun Integer Integer
   -> Flip K String String
   -> Bool

type LiftItOutCompose
    = Fun String Integer
   -> Fun Integer Integer
   -> LiftItOut Identity String
   -> Bool

type ParappaCompose
    = Fun String Integer
   -> Fun Integer Integer
   -> Parappa Identity [] String
   -> Bool

type ListCompose
    = Fun String Integer
   -> Fun Integer Integer
   -> List String
   -> Bool

spec :: Spec
spec = do
    describe "Heavy Lifting" $ do
        it "(+1) $ read \"[1]\" :: [Int]" $
            ((+1) <$> read "[1]" :: [Int]) == [2]
        it "(++ \"lol\") (Just [\"Hi,\", \"Hello\"])" $
            (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"]) ==
                Just ["Hi,lol","Hellolol"]
        it "(*2) (\\x -> x - 2)" $
            let f = fmap (*2) (\x -> x - 2)
             in f 1 == -2
        it "((return '1' ++) . show) (\\x -> [x, 1..3])" $
            let d = (return '1' ++) . show <$> (\x -> [x, 1..3])
             in d 0 == "1[0,1,2,3]"
        it "e" $ do
            let ioi = readIO "1" :: IO Integer
                changed = read . ("123"++) . show <$> ioi :: IO Integer
            v <- (*3) <$> changed
            v `shouldBe` 3693
    describe "Functor instances" $ do
        context "Indentity a" $ do
            it "Identity" $
                property (functorIdentity :: Identity String -> Bool)
            it "Compose" $
                property (functorCompose' :: IdentityCompose)
        context "Pair a a" $ do
            it "Identity" $
                property (functorIdentity :: Pair String -> Bool)
            it "Compose" $
                property (functorCompose' :: PairCompose)
        context "Two a b" $ do
            it "Identity" $ property (functorIdentity :: Two Integer String -> Bool)
            it "Compose" $
                property (functorCompose' :: TwoCompose)
        context "Three' a b" $ do
            it "Identity" $
                property (functorIdentity :: Three' Integer String -> Bool)
            it "Compose" $
                property (functorCompose' :: ThreeCompose)
        context "K a" $ do
            it "Identity" $
                property (functorIdentity :: K String Int -> Bool)
            it "Compose" $
                property (functorCompose' :: KCompose)
        context "Flip f a b" $ do
            it "Identity" $
                property (functorIdentity :: Flip K String String -> Bool)
            it "Compose" $
                property (functorCompose' :: FlipCompose)
        context "LiftItOut f a" $ do
            it "Identity" $
                property (functorIdentity :: LiftItOut Identity String -> Bool)
            it "Compose" $
                property (functorCompose' :: LiftItOutCompose)
        context "Parappa f g a" $ do
            it "Identity" $
                property (functorIdentity :: Parappa Identity [] String -> Bool)
            it "Compose" $
                property (functorCompose' :: ParappaCompose)
        context "List a" $ do
            it "Identity" $
                property (functorIdentity :: List Integer -> Bool)
            it "Compose" $
                property (functorCompose' :: ListCompose)
