module Main where

import Control.Concurrent
import Control.Monad
import Data.Array
import Data.Char
import Data.Function
import Data.Maybe
import Data.Semigroup ((<>))
import Debug.Trace
import Graphics.Vty
import Options.Applicative
import System.Console.ANSI (clearScreen)
import System.Exit (exitSuccess)
import System.IO
import qualified System.Process as SP

data Options =
  Options
    { width :: Int
    , height :: Int
    }
  deriving (Show)

data Direction
  = DUp
  | DDown
  | DLeft
  | DRight
  deriving (Show)

data SquareColor
  = Black
  | White
  deriving (Show)

type Pos = (Int, Int)

type Universe = Array Pos SquareColor

data Ant =
  Ant
    { pos :: Pos
    , dir :: Direction
    }
  deriving (Show)

h = 25

w = 50

start = (h `div` 2, w `div` 2)

options :: Parser Options
options =
  Options <$>
  option
    auto
    (long "width" <> help "width of the terminal" <> value 100 <> metavar "INT") <*>
  option
    auto
    (long "height" <> help "width of the terminal" <> value 100 <> metavar "INT")

mkArray :: (Int, Int) -> Array (Int, Int) SquareColor
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
   in threadDelay (100 * n)

-- clear terminal
clear :: IO ()
clear = do
  _ <- SP.system "reset"
  return ()

colorToChar :: SquareColor -> Char
colorToChar Black = ' '
colorToChar White = '\x25AE'

printUniverse :: Universe -> IO ()
printUniverse m = do
  forM_ (assocs m) $ \((y, x), cell) -> do
    putChar . colorToChar $ cell
    putChar '\n' & when (x == w - 1)

part :: Int -> [a] -> [[a]]
part _ [] = []
part w xs =
  let (y, ys) = splitAt w xs
   in y : part w ys

renderUniverse :: Universe -> Image
renderUniverse universe =
  let linesColors = string defAttr <$> part w (colorToChar <$> elems universe)
   in vertCat linesColors

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

flipSquareColor :: SquareColor -> SquareColor
flipSquareColor Black = White
flipSquareColor White = Black

updateCell :: Pos -> SquareColor -> Universe -> Universe
updateCell (y, x) color universe = universe // [((y, x), color)]

flipCell :: Pos -> Universe -> Universe
flipCell (y, x) universe =
  let c = universe ! (y, x)
   in updateCell (y, x) (flipSquareColor c) universe

moveForward :: Direction -> (Int, Int) -> (Int, Int)
moveForward DLeft (y, x) = (y, (x - 1) `mod` w)
moveForward DRight (y, x) = (y, (x + 1) `mod` w)
moveForward DUp (y, x) = ((y - 1) `mod` h, x)
moveForward DDown (y, x) = ((y + 1) `mod` h, x)

moveAnt :: SquareColor -> Ant -> Ant
moveAnt currentColor Ant {pos = (y, x), dir = dir} =
  let newDir = turnAnt currentColor dir
      (x', y') = moveForward newDir (y, x)
   in Ant {pos = (x', y'), dir = newDir}

getCurrentCellColor :: Ant -> Universe -> SquareColor
getCurrentCellColor Ant {pos = (y, x)} universe = universe ! (y, x)

stepSystem :: (Ant, Universe) -> (Ant, Universe)
stepSystem (ant@Ant {pos = pos}, universe) =
  let currentCellColor = getCurrentCellColor ant universe
      newAnt = moveAnt currentCellColor ant
   in (newAnt, flipCell pos universe)

asciiRender :: Universe -> IO ()
asciiRender universe = do
  clearScreen
  printUniverse universe
  --print $ "Step: " ++ show step
  hFlush stdout
  delay

asciiInit :: IO ()
asciiInit = do
  hSetBuffering stdout $ BlockBuffering (Just (w * h))

vtyRender :: Vty -> Int -> Universe -> IO ()
vtyRender vty step universe = do
  input <- nextEventNonblocking vty
  quit vty & when (input & isExit)
  let universeImage = renderUniverse universe
      output =
        universeImage <->
        string defAttr ("Pres ESC to exit - Steps: " ++ show step)
   in update vty (picForImage output)
  where
    isExit (Just (EvKey KEsc _)) = True
    isExit _ = False
    quit vty = do
      shutdown vty
      exitSuccess

runSystem :: (Universe -> IO ()) -> (Ant, Universe) -> Int -> IO ()
runSystem render system@(ant, universe) step = do
  render universe
  runSystem render (stepSystem system) (step + 1)

main :: IO ()
main = do
  opts <- execParser opts
  print opts
  cfg <- standardIOConfig
  vty <- mkVty cfg
  runSystem (vtyRender vty 0) (mkAnt, mkUniverse) 0
  where
    opts =
      info
        (options <**> helper)
        (fullDesc <>
         progDesc "Langton Ant" <> header "A langton ant implementation")
