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
import Data.Map as M (fromList, toList)
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
      , _board        = generateBoard 10
    }

generateBoard :: Int -> Board
generateBoard n = M.fromList $ zip 
  (generateGradientCells 0 255 n) (generateCoords n)

generateGradientCells :: Int -> Int -> Int -> [Cell]
generateGradientCells start end n = map (`Cell` False) (generateGradient start end n)
  where generateGradient start end n = map (\x -> x * (end `div` n)) [1..n]

generateCoords :: Int -> [Coord]
generateCoords n = [V2 x 1  | x <- [1..1+n]]
