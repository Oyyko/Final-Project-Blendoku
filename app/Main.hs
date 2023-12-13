module Main (main) where

import GameUI (playGame)
import PickGameType (pickGameType)
import InputPlayerName (inputPlayerName)

main :: IO ()
main = do
    playerName <- inputPlayerName
    gameType <- pickGameType (head playerName)
    playGame gameType (head playerName)
    pure ()
