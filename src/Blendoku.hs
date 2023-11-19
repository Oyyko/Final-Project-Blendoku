{-# LANGUAGE TemplateHaskell #-}
module Blendoku 
(
    initGame
    , Game(..)
    , Cell(..)
    , Board
    , ColorVector
    , Coord
    , level
)
where

import Control.Lens (makeLenses, (^.))
import Data.Map (Map)
import Linear.V2 (V2(..))

type ColorVector = (Int, Int, Int)
data Cell = Cell
  { 
    _color :: ColorVector
  , _filled :: Bool
  } deriving (Eq, Show)

type Board = Map Cell Coord

type Coord = V2 Int

data Game = Game
  {
    _level        :: Int
--   , _candidates   :: [Cell]
  } deriving (Eq, Show)

makeLenses ''Game


initGame :: IO Game
initGame = do
  pure $ Game
    { 
        _level        = 0
    }