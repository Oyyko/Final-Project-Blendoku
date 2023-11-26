module Main (main) where

import Lib
import GameUI (playGame)
import PickGameType (pickGameType)

main :: IO ()
main = do
    gameType <- pickGameType
    playGame gameType
    pure ()
