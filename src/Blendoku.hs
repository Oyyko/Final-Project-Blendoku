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
    , level, hint
    , board, gtBoard
    , cursorPos, chosenPos
    , color, chosen, hovered, locked
    , colorToNameRGB
    , colorToNameGray
    , shift
    , toggleSelection
    , toggleHint
    , toggleLock
    , swapWithChosen
    , execBlendokuGame
    , evalBlendokuGame
    , candidateRows, candidateCols
)
where

import Control.Lens hiding (preview, op, zoom, (<|), chosen)
import Data.Map (Map)
import Data.Map as M (fromList, toList, (!), insert, member)
import Linear.V2 (V2(..))
import Data.String (String)
import Control.Monad.Trans.State (StateT(..), gets, evalStateT, execStateT, modify, execStateT)
import Control.Applicative (Applicative(pure))
import Control.Monad (when)
import System.Random (Random(..), newStdGen, mkStdGen)
import Data.List (sortOn)

data Direction = L | R | U | D
  deriving (Eq, Show)

type ColorVector = (Int, Int, Int) -- (R, G, B)
data Cell = Cell
  {
    _color :: ColorVector
  , _hovered :: Bool
  , _chosen :: Bool
  , _locked :: Bool
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
  , _hint          :: Bool
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
    -- awkward, always need to check locked before modification
    if cell ^. locked then pure ()
    else modify $ \g -> g { _chosenPos = cursorPos, _board = board' }
  else
    if isSamePos then do
      let cell = board M.! cursorPos
          cell' = cell & chosen %~ not
          board' = M.insert cursorPos cell' board
      if cell ^. locked then pure ()
      else modify $ \g -> g { _chosenPos = V2 0 0, _board = board' }
    else do
      let chosenCell = board M.! chosenPos
          chosenCell' = chosenCell & chosen %~ not
          cell = board M.! cursorPos
          cell' = cell & chosen %~ not
          board' = M.insert cursorPos cell' (M.insert chosenPos chosenCell' board)
      if cell ^. locked then pure ()
      else modify $ \g -> g { _board = board', _chosenPos = cursorPos }

toggleHint :: BlendokuGame ()
toggleHint = do
  hint <- gets _hint
  modify $ \g -> g { _hint = not hint }

toggleLock :: BlendokuGame ()
toggleLock = do
  cursorPos <- gets _cursorPos
  board <- gets _board
  let cell = board M.! cursorPos
      cell' = cell & locked %~ not
      board' = M.insert cursorPos cell' board
  modify $ \g -> g { _board = board' }

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
  Control.Monad.when (M.member cursor' board) $ modify $ \g -> g { _board = board', _cursorPos = cursor'}

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
        board' = M.insert chosenPos cell2' (M.insert cursorPos cell1' board)
    if cell1 ^. locked || cell2 ^. locked then pure ()
    else modify $ \g -> g { _board = board'}

-- TODO: implementation for RGBcolor rather than gray
colorToNameRGB :: ColorVector -> String
colorToNameRGB (r, g, b) = "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

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
      , _hint            = False
    }

generateBoard :: Bool -> Int -> Int -> Board
generateBoard True row col = M.fromList $ zip xs ys
   where xs = generateCoords row col
         ys = shuffle (generateGradientCells (30, 60, 90) (150, 180, 210) (row * col) 2)
generateBoard False row col = M.fromList $ zip xs ys
  where xs = generateCoords row col
        ys = generateGradientCells (30, 60, 90) (150, 180, 210) (row * col) 2

modifyFirstCell :: Board -> Board
modifyFirstCell board = M.insert (V2 1 1) (Cell (oriCell ^. color) True False False) board
  where oriCell = board M.! (V2 1 1)

generateGradientCells :: ColorVector -> ColorVector -> Int-> Int  -> [Cell]
generateGradientCells (r1, g1, b1) (r2, g2, b2) n scale = 
  map (\(r', g', b') -> Cell (r' * scale, g' * scale, b' * scale) False False False) 
    (generateGradient (r1 `div` scale, g1 `div` scale, b1 `div` scale) (r2 `div` scale, g2 `div` scale, b2 `div` scale) n)
  where generateGradient (r1, g1, b1) (r2, g2, b2) n = zip3 (generateGradient' r1 r2 n) (generateGradient' g1 g2 n) (generateGradient' b1 b2 n)
        generateGradient' start end n = map (\x -> x * (end - start) `div` n + start) [1..n]

generateCoords :: Int -> Int -> [Coord]
generateCoords row col = [V2 x y  | x <- [1..col], y <- [1..row]]

shuffle :: [a] -> [a]
shuffle xs = map snd (sortOn fst $ zip shuffle2 xs)
  where shuffle2 = take (length xs) $ randomRs (1::Int, maxBound) (mkStdGen 42)

candidateRows, candidateCols :: Int
candidateRows = 2
candidateCols = 10
