{-# LANGUAGE NamedFieldPuns #-}

module GameOfLife where

import Control.Concurrent (threadDelay)
import Data.Array ((!), array, assocs, bounds)
import Graphics.Vty
import System.Random (randomRIO)

import Common

-- Qualquer célula viva com menos de dois vizinhos vivos morre de solidão.
-- Qualquer célula viva com mais de três vizinhos vivos morre de superpopulação.
-- Qualquer célula morta com exatamente três vizinhos vivos se torna uma célula viva.
-- Qualquer célula viva com dois ou três vizinhos vivos continua no mesmo estado para a próxima geração.
--
-- live + <2 live = dead
-- live + >3 live = dead
-- dead + 3 live = live
-- live 2 | 3 live = live
data CState
  = Live
  | Dead

colorToState :: SquareColor -> CState
colorToState Black = Dead
colorToState White = Live

stateToColor :: CState -> SquareColor
stateToColor Live = White
stateToColor Dead = Black

cellState :: CState -> Int -> CState
cellState Live n
  | n < 2 = Dead
  | n > 3 = Dead
  | otherwise = Live
cellState Dead 3 = Live
cellState _ _ = Dead

getNeighbour :: Pos -> Universe -> CState
getNeighbour pos@(y, x) universe =
  let (h, w) = universeSize universe
   in if 0 <= y && y < h && 0 <= x && x < w
        then colorToState (universe ! pos)
        else Dead

-- What about dependent list here, with 8 spaces?
getNeighbours :: Pos -> Universe -> [CState]
getNeighbours (y, x) universe =
  [ getNeighbour (y - 1, x - 1) universe
  , getNeighbour (y - 1, x) universe
  , getNeighbour (y - 1, x + 1) universe
  , getNeighbour (y, x - 1) universe
  , getNeighbour (y, x + 1) universe
  , getNeighbour (y + 1, x - 1) universe
  , getNeighbour (y + 1, x) universe
  , getNeighbour (y + 1, x + 1) universe
  ]

getNewCellState :: Universe -> (Pos, SquareColor) -> (Pos, SquareColor)
getNewCellState universe (pos, curState) =
  let neighboursAlive = neighboursLive $ getNeighbours pos universe
   in (pos, stateToColor $ cellState (colorToState curState) neighboursAlive)

neighboursLive :: [CState] -> Int
neighboursLive states = sum $ map live states
  where
    live :: CState -> Int
    live Dead = 0
    live Live = 1

stepSystem :: Universe -> Universe
stepSystem universe =
  let assoc' = assocs universe
      newUniverse = fmap (getNewCellState universe) assoc'
      bounds' = bounds universe
   in array bounds' newUniverse

randomizeSystem :: Int -> Universe -> IO Universe
randomizeSystem num universe = do
  mapM randomizeCell universe
  where
    randomizeCell :: SquareColor -> IO SquareColor
    randomizeCell color = do
      r <- randomRIO (1, 100)
      return $
        if r < num
          then flipSquareColor color
          else color

runGameOfLife :: (Int -> Universe -> IO ()) -> Int -> Universe -> IO ()
runGameOfLife render step universe = do
  let newUniverse = stepSystem universe
  render step newUniverse
  threadDelay 100000
  runGameOfLife render (step + 1) newUniverse

gameOfLife :: Options -> IO ()
gameOfLife opts = do
  vty <- standardIOConfig >>= mkVty
  let Options {height, width} = opts
  let universe = mkUniverse (height, width)
  randomUniverse <- randomizeSystem 25 universe
  runGameOfLife (vtyRender vty) 0 randomUniverse
