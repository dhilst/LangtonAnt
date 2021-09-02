module Main where

import Control.Concurrent
import Control.Monad
import Data.Array
import Data.Char
import Data.Function
import Debug.Trace
import System.Console.ANSI (clearScreen)
import qualified System.Process as SP

data Direction
  = DUp
  | DDown
  | DLeft
  | DRight
  deriving (Show)

data Color
  = Black
  | White
  deriving (Show)

type Pos = (Int, Int)

type Universe = Array Pos Color

data Ant =
  Ant
    { pos :: Pos
    , dir :: Direction
    }
  deriving (Show)

h = 50

w = 100

start = (h `div` 2, w `div` 2)

mkArray :: (Int, Int) -> Array (Int, Int) Color
mkArray (maxx, maxy) =
  array
    ((0, 0), (maxy - 1, maxx - 1))
    [((y, x), Black) | y <- [0 .. maxy - 1], x <- [0 .. maxx - 1]]

mkUniverse :: Universe
mkUniverse = mkArray (w, h)

mkAnt :: Ant
mkAnt =
  let (y, x) = start
   in Ant {pos = (y, x), dir = DUp}

-- sleep seconds
delay :: IO ()
delay =
  let n = 3
   in threadDelay (10000 * n)

-- clear terminal
clear :: IO ()
clear = do
  _ <- SP.system "reset"
  return ()

colorToChar :: Color -> Char
colorToChar Black = '.'
colorToChar White = '@'

printUniverse :: Universe -> IO ()
printUniverse m = do
  forM_ (assocs m) $ \((y, x), cell) -> do
    putChar . colorToChar $ cell
    putChar '\n' & when (x == w - 1)

turnLeft :: Direction -> Direction
turnLeft DUp = DLeft
turnLeft DDown = DRight
turnLeft DLeft = DDown
turnLeft DRight = DUp

turnRight :: Direction -> Direction
turnRight DUp = DRight
turnRight DDown = DLeft
turnRight DLeft = DUp
turnRight DRight = DDown

turnAnt :: Color -> Direction -> Direction
turnAnt White dir = turnRight dir
turnAnt Black dir = turnLeft dir

flipColor :: Color -> Color
flipColor Black = White
flipColor White = Black

updateCell :: Pos -> Color -> Universe -> Universe
updateCell (y, x) color universe = universe // [((y, x), color)]

flipCell :: Pos -> Universe -> Universe
flipCell (y, x) universe =
  let c = universe ! (y, x)
   in updateCell (y, x) (flipColor c) universe

moveForward :: Direction -> (Int, Int) -> (Int, Int)
moveForward DLeft (y, x) = (y, (x - 1) `mod` w)
moveForward DRight (y, x) = (y, (x + 1) `mod` w)
moveForward DUp (y, x) = ((y - 1) `mod` h, x)
moveForward DDown (y, x) = ((y + 1) `mod` h, x)

moveAnt :: Color -> Ant -> Ant
moveAnt currentColor Ant {pos = (y, x), dir = dir} =
  let newDir = turnAnt currentColor dir
      (x', y') = moveForward newDir (y, x)
   in Ant {pos = (x', y'), dir = newDir}

getCurrentCellColor :: Ant -> Universe -> Color
getCurrentCellColor Ant {pos = (y, x)} universe = universe ! (y, x)

stepSystem :: (Ant, Universe) -> (Ant, Universe)
stepSystem (ant@Ant {pos = pos}, universe) =
  let currentCellColor = getCurrentCellColor ant universe
      newAnt = moveAnt currentCellColor ant
   in (newAnt, flipCell pos universe)

runSystem :: (Ant, Universe) -> Int -> IO ()
runSystem system@(ant, universe) step = do
  clearScreen
  printUniverse universe
  print $ "Step: " ++ show step
  delay
  runSystem (stepSystem system) (step + 1)

main :: IO ()
main = runSystem (mkAnt, mkUniverse) 0
