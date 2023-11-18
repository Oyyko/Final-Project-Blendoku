module Main (main) where

import Lib
import GameUI (playGame)

main :: IO ()
main = do
    playGame
    pure ()
