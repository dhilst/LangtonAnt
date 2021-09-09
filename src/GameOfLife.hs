module GameOfLife where

import Data.Array (array, assocs, bounds)

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

type Bounds = (Int, Int)

getNeighbour :: Pos -> Universe -> CState
getNeighbour = undefined

getNeighbours :: Pos -> Universe -> [CState]
getNeighbours = undefined

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
