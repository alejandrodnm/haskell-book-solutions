module Ch23.KeepOnRolling where

import           System.Random

data Die
    = DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
    case n of
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        -- Use 'error'
        -- _extremely_ sparingly.
        x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN = go 0 0
    where
        go :: Int -> Int -> Int -> StdGen -> Int
        go sum count n gen
          | sum >= n = count
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) n nextGen


rollsCountLogged :: StdGen -> (Int, [Die])
rollsCountLogged = go 0 (0, [])
    where
        go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
        go sum count gen
          | sum >= 20 = count
          | otherwise =
              let (die, nextGen) = randomR (1, 6) gen
                  (countI, countL) = count
                in go (sum + die) (countI + 1, countL ++ [intToDie die]) nextGen

