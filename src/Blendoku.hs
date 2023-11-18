{-# LANGUAGE TemplateHaskell #-}
module Blendoku 
(
    initGame
    , Game(..)
    , level
)
where

import Control.Lens (makeLenses, (^.))

data Game = Game
  {
    _level        :: Int
  } deriving (Eq, Show)

makeLenses ''Game


initGame :: IO Game
initGame = do
  pure $ Game
    { 
        _level        = 0
    }