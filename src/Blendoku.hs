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
    , board, gtBoard
    , cursorPos, chosenPos
    , color, chosen, hovered
    , colorToName
    , colorToNameGray
    , shift
    , toggleSelection
    , swapWithChosen
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
import System.Random (Random(..), newStdGen, mkStdGen)
import Data.List (sortOn)

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
  , _gtBoard      :: Board
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
  U -> V2 x (y - 1)
  D -> V2 x (y + 1)

swapWithChosen :: BlendokuGame ()
swapWithChosen = do
  chosenPos <- gets _chosenPos
  cursorPos <- gets _cursorPos
  board <- gets _board
  let isNotChosen = (chosenPos == V2 0 0)
  let isSame = (chosenPos == cursorPos)
  if isNotChosen || isSame then pure () 
  else do
    let cell1 = board M.! chosenPos
        cell1' = cell1 & chosen .~ False & hovered .~ True
        cell2 = board M.! cursorPos
        cell2' = cell2 & chosen .~ True & hovered .~ False
        -- cell2' = cell2 & chosen .~ False & hovered .~ False
        board' = M.insert chosenPos cell2' (M.insert cursorPos cell1' board)
    modify $ \g -> g { _board = board'}
    -- modify $ \g -> g { _board = board', _chosenPos = V2 0 0}

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
      , _board        = modifyFirstCell (generateBoard True candidateRows candidateCols)
      , _gtBoard      = modifyFirstCell (generateBoard False candidateRows candidateCols)
      , _cursorPos       = V2 1 1
      -- V2 0, 0 means no chosen grid
      , _chosenPos       = V2 0 0  
    }


generateBoard :: Bool -> Int -> Int -> Board
generateBoard False row col = M.fromList $ zip 
   (generateCoords row col) (generateGradientCells 0 255 (row * col))
generateBoard True row col = M.fromList $ zip 
   (generateCoords row col) (shuffle (generateGradientCells 0 255 (row * col)))

modifyFirstCell :: Board -> Board
modifyFirstCell board = M.insert (V2 1 1) (Cell (oriCell ^. color) True False) board
  where oriCell = board M.! (V2 1 1)

generateGradientCells :: Int -> Int -> Int -> [Cell]
generateGradientCells start end n = map (\x -> (x `Cell` False) False) (generateGradient start end n)
  where generateGradient start end n = map (\x -> x * (end `div` n)) [1..n]

generateCoords :: Int -> Int -> [Coord]
generateCoords row col = [V2 x y  | x <- [1..col], y <- [1..row]]

generateShuffledCoords :: Int -> Int -> [Coord]
generateShuffledCoords row col =  shuffle [V2 x y  | x <- [1..col], y <- [1..row]]

shuffle :: [a] -> [a]
shuffle xs = map snd (sortOn fst $ zip shuffle2 xs)
  where shuffle2 = take (length xs) $ randomRs (1::Int, maxBound) (mkStdGen 42)

candidateRows, candidateCols :: Int
candidateRows = 2
candidateCols = 10