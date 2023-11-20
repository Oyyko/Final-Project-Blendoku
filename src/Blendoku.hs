{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Blendoku
(
    initGame
    , Game(..)
    , Cell(..)
    , Board
    , ColorVector
    , Coord
    , Direction(..)
    , BlendokuGame 
    , level
    , board
    , color
    , cursor
    , chosen
    , colorToName
    , colorToNameGray
    , shift
    , execBlendokuGame
    , evalBlendokuGame
    , candidateRows, candidateCols
)
where

import Control.Lens hiding (preview, op, zoom, (<|), chosen)
import Data.Map (Map)
import Data.Map as M (fromList, toList, (!), insert)
import Linear.V2 (V2(..))
import Data.String (String)
import Control.Monad.Trans.State (StateT(..), gets, evalStateT, execStateT, modify, execStateT)
import Control.Applicative (Applicative(pure))
import Data.Bool (Bool(False))

data Direction = L | R | U | D
  deriving (Eq, Show)

type ColorVector = (Int, Int, Int) -- (R, G, B)
data Cell = Cell
  {
    -- _color :: ColorVector
    _color :: Int
  , _chosen :: Bool
  } deriving (Eq, Show, Ord)

makeLenses ''Cell

type Board = Map Coord Cell

type Coord = V2 Int

data Game = Game
  {
    _level        :: Int
  , _board        :: Board
  , _cursor       :: Coord
  } deriving (Eq, Show)

makeLenses ''Game

type BlendokuGameT = StateT Game
type BlendokuGame a = forall m. (Monad m) => BlendokuGameT m a

evalBlendokuGame :: BlendokuGame a -> Game -> a
evalBlendokuGame m = runIdentity . evalStateT m

execBlendokuGame :: BlendokuGame a -> Game -> Game
execBlendokuGame m = runIdentity . execStateT m

shift :: Direction -> BlendokuGame ()
shift dir = do
  cursor <- gets _cursor
  board <- gets _board
  let cell1 = board M.! cursor
      cell1' = cell1 & chosen .~ False
      cursor' = updateCursor cursor dir
      cell2 = board M.! cursor'
      cell2' = cell2 & chosen .~ True
      board' = M.insert cursor cell1' (M.insert cursor' cell2' board)
  modify $ \g -> g { _board = board', _cursor = cursor' }

updateCursor :: Coord -> Direction -> Coord
updateCursor (V2 x y) dir = case dir of
  L -> V2 ((x - 1 + candidateCols) `mod` candidateCols) y
  R -> V2 ((x + 1) `mod` candidateCols) y
  U -> V2 x ((y + 1) `mod` candidateRows)
  D -> V2 x ((y - 1 + candidateRows) `mod` candidateRows)

-- TODO: implementation for RGBcolor rather than gray
colorToName :: ColorVector -> String
colorToName (r, g, b) = "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

colorToNameGray :: Int -> String
colorToNameGray x = "gray " ++ show x

initGame :: IO Game
initGame = do
  pure Game
    {
        _level        = 0
      , _board        = modifyFirstCell (generateBoard 10)
      , _cursor       = V2 1 1
      -- ,  _board        = sampleBoard
    }


sampleBoard :: Board
sampleBoard = M.fromList $ zip 
  [V2 1 1, V2 2 1, V2 3 1, V2 4 1, V2 5 1, V2 6 1, V2 7 1, V2 8 1, V2 9 1, V2 10 1]
  [Cell 0 True, Cell 1 False, Cell 2 False, Cell 3 False, Cell 4 False, Cell 5 False, Cell 6 False, Cell 7 False, Cell 8 False, Cell 9 False] 

generateBoard :: Int -> Board
generateBoard n = M.fromList $ zip 
   (generateCoords n) (generateGradientCells 0 255 n)

modifyFirstCell :: Board -> Board
modifyFirstCell board = M.insert (V2 1 1) (Cell 0 True) board

generateGradientCells :: Int -> Int -> Int -> [Cell]
generateGradientCells start end n = map (`Cell` False) (generateGradient start end n)
  where generateGradient start end n = map (\x -> x * (end `div` n)) [1..n]

generateCoords :: Int -> [Coord]
generateCoords n = [V2 x 1  | x <- [1..1+n]]

candidateRows, candidateCols :: Int
candidateRows = 1
candidateCols = 10