module Ch26.Morra where

import           Control.Monad             (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Char                 (toLower)
import           System.Random             (randomRIO)

type Game = StateT Score IO
data Score = Score
    { human :: Int
    , pc    :: Int
    } deriving Show

data Player = Human | PC deriving Show
type RoundWinner = Player
data GameResult = GameWinner Player | Tie

type HumanSelection = Int
type PcSelection = Int

gameResults :: Score -> GameResult
gameResults s | human s > pc s = GameWinner Human
              | human s < pc s = GameWinner PC
              | human s == pc s = Tie

humanSelect :: IO HumanSelection
humanSelect = do
    putStrLn "-- Select a number between 1 and 2"
    putStr "P1: "
    s <- getLine
    case s of
        "1" -> return 1
        "2" -> return 2
        _ -> do
            putStrLn "-- Invalid choice, try again"
            humanSelect

pcSelect :: IO PcSelection
pcSelect = do
    pcS <- randomRIO (1, 2)
    putStrLn $ "PC: " ++ show pcS
    return pcS

roundWinner :: HumanSelection -> PcSelection -> RoundWinner
roundWinner hs pcS =
    if (hs + pcS) `mod` 2 == 0 then
        PC
    else
        Human

newRound :: IO RoundWinner
newRound = do
    hs <- humanSelect
    pcS <- pcSelect
    let w = roundWinner hs pcS
    putStrLn $ "-- This round winner is " ++ show w
    return w


rematch :: IO Bool
rematch = do
    putStrLn "-- Play another round y/n"
    r <- getLine
    case fmap toLower r of
        "y" -> return True
        "n" -> return False
        _ -> do
            putStrLn "-- Invalid choice, try again"
            rematch

gameLoop :: Game ()
gameLoop = do
    liftIO $ putStrLn "Round starting"
    s <- get
    winner <- liftIO newRound
    let s' = case winner of
          Human -> s { human = human s + 1 }
          PC    -> s { pc = pc s + 1 }
    put s'
    r <- liftIO rematch
    when r gameLoop

main :: IO ()
main = do
    putStrLn "-- Starting game"
    putStrLn "-- Player is odds, computer is evens"
    results <- execStateT gameLoop $ Score 0 0
    let msg = case gameResults results of
                GameWinner Human -> "The winner is the player1"
                GameWinner PC    -> "The winner is the PC, better luck next time"
                Tie   -> "It's a tie"
    putStrLn msg
