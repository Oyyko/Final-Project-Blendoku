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
    , MazeType(..)
    , BlendokuGame
    , level, hint
    , board, gtBoard, boardRows, boardCols
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
)
where

import Control.Lens hiding (preview, op, zoom, (<|), chosen, elements)
import Data.Map (Map)
import Data.Map as M (fromList, toList, (!), insert, member, keys, filter, union, empty, adjust)
import Linear.V2 (V2(..))
import Data.String (String)
import Control.Monad.Trans.State (StateT(..), gets, evalStateT, execStateT, modify, execStateT)
import Control.Applicative (Applicative(pure))
import Control.Monad (when)
import System.Random (Random(..), newStdGen, mkStdGen)
import Data.List (sortOn)
import Test.QuickCheck (choose, elements, generate, Gen)
import Data.IntMap (insert)

data Direction = L | R | U | D
  deriving (Eq, Show)

data MazeType = Line | Rectangle
  deriving (Eq, Show, Enum)

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

type ColorWord = [(Coord, Cell)]
data Game = Game
  {
    _level        :: Int
  , _board        :: Board
  , _gtBoard      :: Board
  , _cursorPos     :: Coord
  , _chosenPos     :: Coord
  , _hint          :: Bool
  , _boardRows          :: Int
  , _boardCols          :: Int
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

colorToNameRGB :: ColorVector -> String
colorToNameRGB (r, g, b) = "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

colorToNameGray :: Int -> String
colorToNameGray x = "gray " ++ show x

initGame :: IO Game
initGame = do
  let gameType = Rectangle
  (rows, cols, board, gtBoard) <- case gameType of
    Line -> initLineBoard
    Rectangle -> initRectangleBoard
  return  Game
    {
        _level        = 0
      , _board          = board
      , _gtBoard        = gtBoard
      , _cursorPos       = V2 1 1
      -- V2 0, 0 means no chosen grid
      , _chosenPos       = V2 0 0
      , _hint            = False
      , _boardRows       = rows
      , _boardCols       = cols
    }

initLineBoard :: IO (Int, Int, Board, Board)
initLineBoard = do
  let rows = 1
      cols = 9
      emptyBoard = generateEmptyBoard rows cols
      coord = V2 1 1
      dir = R
  c1 <- generate (elements keyColorList)
  c2 <- generate (elements keyColorList)
  xs <- generateGraidentCoords coord cols dir
  ys <- generateGradientCells c1 c2 cols 4
  let gtBoard = insertColorWord (zip xs ys) emptyBoard
      lockedList = [V2 1 1, V2 cols 1]
  gtBoard <- return (foldr (M.adjust (\cell -> cell & locked .~ True)) gtBoard lockedList)
  board <- return (addCursor (shuffleBoard  gtBoard))
  return (rows, cols, board, gtBoard)

initRectangleBoard :: IO (Int, Int, Board, Board)
initRectangleBoard = do
  let rows = 3
      cols = 10
      emptyBoard = generateEmptyBoard rows cols
  c1 <- generate (elements keyColorList)
  c2 <- generate (elements keyColorList)
  c3 <- generate (elements keyColorList)
  c4 <- generate (elements keyColorList)
  xs12 <- generateGraidentCoords (V2 1 1) cols R
  ys12 <- generateGradientCells c1 c2 cols 4
  let word12 = zip xs12 ys12
  gtBoard <- return (insertColorWord word12 emptyBoard)
  xs34 <- generateGraidentCoords (V2 1 3) cols R
  ys34 <- generateGradientCells c3 c4 cols 4
  let word34 = zip xs34 ys34
  gtBoard <- return (insertColorWord word34 gtBoard)
  xs13 <- generateGraidentCoords (V2 1 1) rows D
  ys13 <- generateGradientCells c1 c3 rows 4
  let word13 = zip xs13 ys13
  gtBoard <- return (insertColorWord word13 gtBoard)
  xs24 <- generateGraidentCoords (V2 10 1) rows D
  ys24 <- generateGradientCells c2 c4 rows 4
  let word24 = zip xs24 ys24
  gtBoard <- return (insertColorWord word24 gtBoard)
  let lockedList = [V2 1 1, (V2 cols 1), (V2 1 rows), (V2 cols rows)]
  gtBoard <- return (foldr (M.adjust (\cell -> cell & locked .~ True)) gtBoard lockedList)
  let board = addCursor (shuffleBoard gtBoard)
  return (rows, cols, board, gtBoard)


shuffleBoard :: Board -> Board
shuffleBoard board = foldr M.union M.empty [shuffledRemain, lockedItems, blackItems]
  where
    lockedItems = M.filter (\cell -> cell ^. locked) board
    blackItems = M.filter (\cell -> cell ^. color == (0, 0, 0)) board
    remainItems = M.filter (\cell -> not (cell ^. locked) && cell ^. color /= (0, 0, 0)) board
    (xs, ys) = unzip (M.toList remainItems)
    shuffledRemain = M.fromList (zip xs (shuffle ys))

generateEmptyBoard :: Int -> Int -> Board
generateEmptyBoard row col = M.fromList $ zip xs ys
  where xs = generateCoords row col
        ys = replicate (row * col) (Cell (0, 0, 0) False False False)

updateBoardN :: Int -> Board -> Coord -> IO Board
updateBoardN n board coord = do
  if n == 1
    then updateBoard board coord
    else do
      board' <- updateBoard board coord
      nonBlackCells <- return (M.filter (\cell -> cell ^. color /= (0, 0, 0)) board')
      coord' <- generate (elements (M.keys nonBlackCells))
      updateBoardN (n-1) board' coord'

updateBoard :: Board -> Coord -> IO Board
updateBoard board coord = do
  word' <- generateNextColorWord board coord
  if validateWord word' board
    then return (insertColorWord word' board)
    else updateBoard board coord

generateNextColorWord :: Board -> Coord -> IO ColorWord
generateNextColorWord board coord = do
  n <- generate (choose (3::Int, 7))
  dir <- generate (elements [L, R, U, D])
  c1 <- generate (elements keyColorList)
  c2 <- generate (elements keyColorList)
  xs <- generateGraidentCoords coord n dir
  ys <- generateGradientCells c1 c2 n 4
  return (zip xs ys)

generateGraidentCoords :: Coord -> Int -> Direction -> IO [Coord]
generateGraidentCoords coord n dir = do
  let (V2 x y) = coord
  case dir of
    L -> return [V2 x' y | x' <- [x - n .. x]]
    R -> return [V2 x' y | x' <- [x .. x + n]]
    U -> return [V2 x y' | y' <- [y - n .. y]]
    D -> return [V2 x y' | y' <- [y .. y + n]]

-- we want to keep the first and the last cell the same while generating the gradient
-- generateGradient' takes the start and end color and the number of cells in between
generateGradientCells :: ColorVector -> ColorVector -> Int-> Int  -> IO [Cell]
generateGradientCells (r1, g1, b1) (r2, g2, b2) n scale = do
  let first = (r1, g1, b1)
      last = (r2, g2, b2)
      scaledFirst = (r1 `div` scale, g1 `div` scale, b1 `div` scale)
      scaledLast = (r2 `div` scale, g2 `div` scale, b2 `div` scale) 
      gradient = map (\(r, g, b) ->  (r * scale, g * scale, b * scale)) (generateGradient scaledFirst scaledLast (n-2))
  return (map (\c -> Cell c False False False) ([first] ++ gradient ++ [last]))
  where
        generateGradient (r1, g1, b1) (r2, g2, b2) n = zip3 (generateGradient' r1 r2 n) (generateGradient' g1 g2 n) (generateGradient' b1 b2 n)
        generateGradient' start end n = map (\x -> x * (end - start) `div` (n+1) + start) [1..n]


-- generateGradientCells :: ColorVector -> ColorVector -> Int-> Int  -> IO [Cell]
-- generateGradientCells (r1, g1, b1) (r2, g2, b2) n scale = do
--   return (map (\(r', g', b') -> Cell (r' * scale, g' * scale, b' * scale) False False False)
--     (generateGradient (r1 `div` scale, g1 `div` scale, b1 `div` scale) (r2 `div` scale, g2 `div` scale, b2 `div` scale) n))
--   where
--         generateGradient (r1, g1, b1) (r2, g2, b2) n = zip3 (generateGradient' r1 r2 n) (generateGradient' g1 g2 n) (generateGradient' b1 b2 n)
--         generateGradient' start end n = map (\x -> x * (end - start) `div` n + start) [1..n]


validateWord :: ColorWord -> Board -> Bool
validateWord word board =
  all (\(k, v) -> validateSingle k v board) word && validateLast word board && validateFirst word board

validateFirst :: ColorWord -> Board -> Bool
validateFirst word board =
  not (M.member negCoord board) || (board M.! negCoord ^. color == (0, 0, 0))
  where firstCoord@(V2 x0 y0) = fst (head word)
        secondCoord@(V2 x1 y1) = fst (word !! 1)
        dx = x1 - x0
        dy = y1 - y0
        negCoord = V2 (x0 - dx) (y0 - dy)


validateLast :: ColorWord -> Board -> Bool
validateLast word board =
  all (\coord -> not (M.member coord board) || (board M.! coord ^. color == (0, 0, 0))) neighbors
  where lastCoord = fst (last word)
        neighbors = getFourNeighbors lastCoord
        getFourNeighbors (V2 x y) = [V2 x' y' | x' <- [x - 1, x + 1], y' <- [y - 1, y + 1]]


validateSingle :: Coord -> Cell -> Board -> Bool
validateSingle coord cell board =
  M.member coord board &&
  ((board M.! coord ^. color == cell ^. color) || (board M.! coord ^. color == (0, 0, 0)))


insertColorWord :: ColorWord -> Board -> Board
insertColorWord word board = foldl (\b (k, v) -> M.insert k v b) board word

addCursor :: Board -> Board
addCursor = M.adjust (\cell -> cell & hovered .~ True) (V2 1 1)

generateCoords :: Int -> Int -> [Coord]
generateCoords row col = [V2 x y  | x <- [1..col], y <- [1..row]]

shuffle :: [a] -> [a]
shuffle xs = map snd (sortOn fst $ zip shuffle2 xs)
  where shuffle2 = take (length xs) $ randomRs (1::Int, maxBound) (mkStdGen 42)

keyColorList :: [ColorVector]
keyColorList = shuffle [ (r, g, b) | r <- [0, 32..255], g <- [0, 32..255], b <- [0, 32..255]]