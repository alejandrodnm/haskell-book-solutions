module Ch28.VectorBenchmark where

import           Criterion.Main
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV

boxedHead :: [Int] -> Int
boxedHead l =
    let
        v = V.fromList l
    in
        V.head v

boxedIndex :: [Int] -> Int
boxedIndex l =
    let
        v = V.fromList l
    in
        v V.! 99

unboxedHead :: [Int] -> Int
unboxedHead l =
    let
        v = UV.fromList l
    in
        UV.head v

unboxedIndex :: [Int] -> Int
unboxedIndex l =
    let
        v = UV.fromList l
    in
        v UV.! 99

list :: [Int]
list = [1..100000]

main :: IO ()
main = defaultMain
    [ bench "boxed vector head" $ whnf boxedHead list
    , bench "unboxed vector head" $ whnf unboxedHead list
    , bench "boxed vector index" $ whnf boxedIndex list
    , bench "unboxed vector index" $ whnf unboxedIndex list
    ]
