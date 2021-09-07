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

type Universe = Array Pos SquareColor

type Height = Int

type Width = Int

type Pos = (Height, Width)

type UniSiz = (Height, Width)

data Ant =
  Ant
    { pos :: Pos
    , dir :: Direction
    }
  deriving (Show)

middle (height, width) = (height `div` 2, width `div` 2)

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
mkArray (maxy, maxx) =
  array
    ((0, 0), (maxy - 1, maxx - 1))
    [((y, x), Black) | y <- [0 .. maxy - 1], x <- [0 .. maxx - 1]]

mkUniverse :: (Int, Int) -> Universe
mkUniverse (h, w) = mkArray (h, w)

universeSize :: Universe -> (Int, Int)
universeSize = snd . bounds

mkAnt :: UniSiz -> Ant
mkAnt siz = Ant {pos = middle siz, dir = DUp}

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

moveForward :: UniSiz -> Direction -> (Int, Int) -> (Int, Int)
moveForward (h, w) DLeft (y, x) = (y, (x - 1) `mod` w)
moveForward (h, w) DRight (y, x) = (y, (x + 1) `mod` w)
moveForward (h, w) DUp (y, x) = ((y - 1) `mod` h, x)
moveForward (h, w) DDown (y, x) = ((y + 1) `mod` h, x)

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
  runSystem render (stepSystem system) (step + 1)

main :: IO ()
main = do
  opts' <- execParser opts
  cfg <- standardIOConfig
  vty <- mkVty cfg
  let Options {height, width} = opts'
      universe = mkUniverse (height, width)
      siz = universeSize universe
   in runReaderT (runSystem (vtyRender vty) (mkAnt siz, universe) 0) opts'
  where
    opts =
      info
        (options <**> helper)
        (fullDesc <>
         progDesc "Langton Ant" <> header "A langton ant implementation")
