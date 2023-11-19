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
    , board
    , color
    , colorToName
    , colorToNameGray
)
where

import Control.Lens (makeLenses, (^.))
import Data.Map (Map)
import Data.Map as M
import Linear.V2 (V2(..))
import Data.String (String)

type ColorVector = (Int, Int, Int) -- (R, G, B)
data Cell = Cell
  { 
    -- _color :: ColorVector
    _color :: Int
  , _filled :: Bool
  } deriving (Eq, Show, Ord)

makeLenses ''Cell

type Board = Map Cell Coord

type Coord = V2 Int

data Game = Game
  {
    _level        :: Int
  , _board        :: Board
  } deriving (Eq, Show)

makeLenses ''Game

-- TODO: implementation for RGBcolor rather than gray
colorToName :: ColorVector -> String
colorToName (r, g, b) = "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

colorToNameGray :: Int -> String
colorToNameGray x = "gray " ++ show x

initGame :: IO Game
initGame = do
  pure $ Game
    { 
        _level        = 0
      , _board        = M.fromList [(testCell1, V2 5 5), (testCell2, V2 7 9)]
    }

testCell1, testCell2 :: Cell
testCell1 = Cell
    {
        _color = 128
      , _filled = False
    }
testCell2 = Cell
    {
        _color = 197
      , _filled = False
    }