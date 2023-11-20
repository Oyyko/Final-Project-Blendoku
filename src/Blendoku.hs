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
    , cursorPos, chosenPos
    , chosen, hovered
    , colorToName
    , colorToNameGray
    , shift
    , toggleSelection
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
  , _hovered :: Bool
  , _chosen :: Bool
  } deriving (Eq, Show, Ord)

makeLenses ''Cell

type Board = Map Coord Cell

type Coord = V2 Int

data Game = Game
  {
    _level        :: Int
  , _board        :: Board
  , _cursorPos     :: Coord
  , _chosenPos     :: Coord
  } deriving (Eq, Show)

makeLenses ''Game

type BlendokuGameT = StateT Game
type BlendokuGame a = forall m. (Monad m) => BlendokuGameT m a

evalBlendokuGame :: BlendokuGame a -> Game -> a
evalBlendokuGame m = runIdentity . evalStateT m

execBlendokuGame :: BlendokuGame a -> Game -> Game
execBlendokuGame m = runIdentity . execStateT m

toggleSelection :: BlendokuGame ()
toggleSelection = do
  chosenPos <- gets _chosenPos
  cursorPos <- gets _cursorPos
  board <- gets _board
  let isNotChosen = (chosenPos == V2 0 0)
      isSamePos = (chosenPos == cursorPos)
  if isNotChosen then do
    let cell = board M.! cursorPos
        cell' = cell & chosen %~ not
        board' = M.insert cursorPos cell' board
    modify $ \g -> g { _chosenPos = cursorPos, _board = board' }
  else
    if isSamePos then do
      let cell = board M.! cursorPos
          cell' = cell & chosen %~ not
          board' = M.insert cursorPos cell' board
      modify $ \g -> g { _chosenPos = V2 0 0, _board = board' }
    else do
      let chosenCell = board M.! chosenPos
          chosenCell' = chosenCell & chosen %~ not
          cell = board M.! cursorPos
          cell' = cell & chosen %~ not
          board' = M.insert cursorPos cell' (M.insert chosenPos chosenCell' board)
      modify $ \g -> g { _board = board', _chosenPos = cursorPos }

shift :: Direction -> BlendokuGame ()
shift dir = do
  cursor <- gets _cursorPos
  board <- gets _board
  let cell1 = board M.! cursor
      cell1' = cell1 & hovered .~ False
      cursor' = updateCursor cursor dir
      cell2 = board M.! cursor'
      cell2' = cell2 & hovered .~ True
      board' = M.insert cursor cell1' (M.insert cursor' cell2' board)
  modify $ \g -> g { _board = board', _cursorPos = cursor'}

updateCursor :: Coord -> Direction -> Coord
updateCursor (V2 x y) dir = case dir of
  L -> V2 (x - 1) y
  R -> V2 (x + 1) y
  U -> V2 x (y + 1)
  D -> V2 x (y - 1)

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
      , _cursorPos       = V2 1 1
      -- V2 0, 0 means no chosen grid
      , _chosenPos       = V2 0 0  
    }


generateBoard :: Int -> Board
generateBoard n = M.fromList $ zip 
   (generateCoords n) (generateGradientCells 0 255 n)

modifyFirstCell :: Board -> Board
modifyFirstCell board = M.insert (V2 1 1) (Cell 0 True False) board

generateGradientCells :: Int -> Int -> Int -> [Cell]
generateGradientCells start end n = map (\x -> (x `Cell` False) False) (generateGradient start end n)
  where generateGradient start end n = map (\x -> x * (end `div` n)) [1..n]

generateCoords :: Int -> [Coord]
generateCoords n = [V2 x 1  | x <- [1..1+n]]

candidateRows, candidateCols :: Int
candidateRows = 1
candidateCols = 10