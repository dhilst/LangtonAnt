module Common
  ( Pos
  , UniSiz
  , SquareColor(..)
  , Universe
  , Options(..)
  , GameType(..)
  , options
  , delay
  , mkUniverse
  , universeSize
  , flipCell
  , flipSquareColor
  , vtyRender
  , middle
  ) where

import Control.Concurrent
import Control.Monad
import Data.Array
import Data.Function
import Graphics.Vty
import Options.Applicative
import System.Exit (exitSuccess)

data Options =
  Options
    { width :: Int
    , height :: Int
    , game :: GameType
    }
  deriving (Show)

data GameType
  = LangtonAnt
  | GameOfLife
  deriving (Show, Read)

data SquareColor
  = Black
  | White
  deriving (Show)

type Universe = Array Pos SquareColor

type Height = Int

type Width = Int

type Pos = (Height, Width)

type UniSiz = (Height, Width)

middle :: UniSiz -> Pos
middle (h, w) = (h `div` 2, w `div` 2)

options :: Parser Options
options =
  Options <$>
  option
    auto
    (long "width" <> help "width of the terminal" <> value 100 <> metavar "INT") <*>
  option
    auto
    (long "height" <> help "width of the terminal" <> value 50 <> metavar "INT") <*>
  option
    auto
    (long "game" <>
     help "what game to run" <>
     value LangtonAnt <> metavar "LangtonAnt|GameOfLife")

mkArray :: (Int, Int) -> Array (Int, Int) SquareColor
mkArray (maxy, maxx) =
  array
    ((0, 0), (maxy - 1, maxx - 1))
    [((y, x), Black) | y <- [0 .. maxy - 1], x <- [0 .. maxx - 1]]

mkUniverse :: (Int, Int) -> Universe
mkUniverse (h, w) = mkArray (h, w)

universeSize :: Universe -> (Int, Int)
universeSize universe =
  let (h, w) = snd . bounds $ universe
   in (h + 1, w + 1)

-- sleep seconds
delay :: IO ()
delay =
  let n = 3
   in threadDelay (1000 * n)

colorToChar :: SquareColor -> Char
colorToChar Black = ' '
colorToChar White = '\x25AE'

renderUniverse :: Universe -> Image
renderUniverse universe =
  let (_, w) = universeSize universe
      linesColors = string defAttr <$> part w (colorToChar <$> elems universe)
   in vertCat linesColors
  where
    part _ [] = []
    part w xs =
      let (y, ys) = splitAt w xs
       in y : part w ys

updateCell :: Pos -> SquareColor -> Universe -> Universe
updateCell (y, x) color universe = universe // [((y, x), color)]

flipSquareColor :: SquareColor -> SquareColor
flipSquareColor Black = White
flipSquareColor White = Black

flipCell :: Pos -> Universe -> Universe
flipCell (y, x) universe =
  let c = universe ! (y, x)
   in updateCell (y, x) (flipSquareColor c) universe

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
    quit vty' = do
      shutdown vty'
      exitSuccess
