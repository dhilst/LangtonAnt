{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
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

start Options {width, height} = (height `div` 2, width `div` 2)

options :: Parser Options
options =
  Options <$>
  option
    auto
    (long "width" <> help "width of the terminal" <> value 100 <> metavar "INT") <*>
  option
    auto
    (long "height" <> help "width of the terminal" <> value 50 <> metavar "INT")

mkArray :: (Int, Int) -> Array (Int, Int) SquareColor
mkArray (maxx, maxy) =
  array
    ((0, 0), (maxy - 1, maxx - 1))
    [((y, x), Black) | y <- [0 .. maxy - 1], x <- [0 .. maxx - 1]]

mkUniverse :: Options -> Universe
mkUniverse Options {width = w, height = h} = mkArray (w, h)

mkAnt :: Options -> Ant
mkAnt opts =
  let (y, x) = start opts
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

renderUniverse :: Options -> Universe -> Image
renderUniverse Options {width} universe =
  let linesColors =
        string defAttr <$> part width (colorToChar <$> elems universe)
   in vertCat linesColors
  where
    part _ [] = []
    part w xs =
      let (y, ys) = splitAt w xs
       in y : part w ys

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

moveForward :: Options -> Direction -> (Int, Int) -> (Int, Int)
moveForward Options {width = w, height = h} DLeft (y, x) = (y, (x - 1) `mod` w)
moveForward Options {width = w, height = h} DRight (y, x) = (y, (x + 1) `mod` w)
moveForward Options {width = w, height = h} DUp (y, x) = ((y - 1) `mod` h, x)
moveForward Options {width = w, height = h} DDown (y, x) = ((y + 1) `mod` h, x)

moveAnt :: Options -> SquareColor -> Ant -> Ant
moveAnt opts currentColor Ant {pos = (y, x), dir = dir} =
  let newDir = turnAnt currentColor dir
      (x', y') = moveForward opts newDir (y, x)
   in Ant {pos = (x', y'), dir = newDir}

getCurrentCellColor :: Ant -> Universe -> SquareColor
getCurrentCellColor Ant {pos = (y, x)} universe = universe ! (y, x)

stepSystem :: Options -> (Ant, Universe) -> (Ant, Universe)
stepSystem opts (ant@Ant {pos = pos}, universe) =
  let currentCellColor = getCurrentCellColor ant universe
      newAnt = moveAnt opts currentCellColor ant
   in (newAnt, flipCell pos universe)

vtyRender :: Vty -> Int -> Universe -> ReaderT Options IO ()
vtyRender vty step universe = do
  opts <- ask
  input <- liftIO $ nextEventNonblocking vty
  liftIO $ quit vty & when (input & isExit)
  let universeImage = renderUniverse opts universe
      output =
        universeImage <->
        string defAttr ("Pres ESC to exit - Steps: " ++ show step)
   in liftIO $ update vty (picForImage output)
  where
    isExit (Just (EvKey KEsc _)) = True
    isExit _ = False
    quit vty = do
      shutdown vty
      exitSuccess

runSystem ::
     (Int -> Universe -> ReaderT Options IO ())
  -> (Ant, Universe)
  -> Int
  -> ReaderT Options IO ()
runSystem render system@(ant, universe) step = do
  opts <- ask
  render step universe
  runSystem render (stepSystem opts system) (step + 1)

main :: IO ()
main = do
  opts' <- execParser opts
  cfg <- standardIOConfig
  vty <- mkVty cfg
  runReaderT
    (runSystem (vtyRender vty) (mkAnt opts', mkUniverse opts') 0)
    opts'
  where
    opts =
      info
        (options <**> helper)
        (fullDesc <>
         progDesc "Langton Ant" <> header "A langton ant implementation")
