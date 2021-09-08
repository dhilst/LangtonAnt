{-# LANGUAGE NamedFieldPuns #-}

module LangtonAnt
  ( langtonAnt
  ) where

import Data.Array
import Graphics.Vty

import Common

data Direction
  = DUp
  | DDown
  | DLeft
  | DRight
  deriving (Show)

data Ant =
  Ant
    { pos :: Pos
    , dir :: Direction
    }
  deriving (Show)

mkAnt :: UniSiz -> Ant
mkAnt siz = Ant {pos = middle siz, dir = DUp}

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

turnAnt :: SquareColor -> Direction -> Direction
turnAnt White dir = turnRight dir
turnAnt Black dir = turnLeft dir

moveForward :: UniSiz -> Direction -> (Int, Int) -> (Int, Int)
moveForward (_, w) DLeft (y, x) = (y, (x - 1) `mod` w)
moveForward (_, w) DRight (y, x) = (y, (x + 1) `mod` w)
moveForward (h, _) DUp (y, x) = ((y - 1) `mod` h, x)
moveForward (h, _) DDown (y, x) = ((y + 1) `mod` h, x)

moveAnt :: UniSiz -> SquareColor -> Ant -> Ant
moveAnt siz currentColor Ant {pos = (y, x), dir = dir} =
  let newDir = turnAnt currentColor dir
      (x', y') = moveForward siz newDir (y, x)
   in Ant {pos = (x', y'), dir = newDir}

getCurrentCellColor :: Ant -> Universe -> SquareColor
getCurrentCellColor Ant {pos = (y, x)} universe = universe ! (y, x)

stepSystem :: (Ant, Universe) -> (Ant, Universe)
stepSystem (ant@Ant {pos = pos}, universe) =
  let currentCellColor = getCurrentCellColor ant universe
      newAnt = moveAnt (universeSize universe) currentCellColor ant
   in (newAnt, flipCell pos universe)

langtonAnt :: Options -> IO ()
langtonAnt opts = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
  let Options {height, width} = opts
      universe = mkUniverse (height, width)
      siz = universeSize universe
   in runSystem (vtyRender vty) (mkAnt siz, universe) 0

runSystem :: (Int -> Universe -> IO ()) -> (Ant, Universe) -> Int -> IO ()
runSystem render system@(_, universe) step = do
  render step universe
  delay
  runSystem render (stepSystem system) (step + 1)
