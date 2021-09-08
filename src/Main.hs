{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Options.Applicative

import Common
import LangtonAnt

gameOfLife :: Options -> IO ()
gameOfLife _ = error "not implemented"

main :: IO ()
main = do
  opts' <- execParser opts
  case game opts' of
    LangtonAnt -> langtonAnt opts'
    GameOfLife -> gameOfLife opts'
  where
    opts =
      info
        (options <**> helper)
        (fullDesc <>
         progDesc "Langton Ant" <> header "A langton ant implementation")
