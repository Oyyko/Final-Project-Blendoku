{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
    , level, hint, maxLevel
    , board, gtBoard, boardRows, boardCols, remainTime, isChallenge
    , cursorPos, chosenPos
    , color, chosen, hovered, locked
    , colorToNameRGB
    , colorToNameGray
    , shift
    , toggleSelection
    , toggleHint
    , toggleLock
    , timeTickPerGame
    , swapWithChosen
    , execBlendokuGame
    , evalBlendokuGame
)
where

import Control.Lens hiding (preview, op, zoom, (<|), chosen, elements)
import Data.Map (Map)
import Data.Map as M (fromList, toList, (!), insert, member, keys, filter, union, empty, adjust, null, size)
import Linear.V2 (V2(..))
import Data.String (String)
import Control.Monad.Trans.State (StateT(..), gets, evalStateT, execStateT, modify, execStateT)
import Control.Applicative (Applicative(pure))
import Control.Monad (when)
import Data.List (sortOn)
import Test.QuickCheck (choose, elements, generate, Gen)
import Data.IntMap (insert)
import System.Random (mkStdGen, randomRs, mkStdGen)

data Direction = L | R | U | D
  deriving (Eq, Show)

data MazeType = Line | Rectangle | TShape | HShape | RandomShape | Challenge
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

-- the positive direction of x is right and the positive direction of y is down
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
  , _remainTime ::Int
  , _isChallenge :: Bool
  , _maxLevel :: Int
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

timeTickPerGame :: BlendokuGame ()
timeTickPerGame = do
  remainTime <- gets _remainTime
  -- isChallenge <- gets _isChallenge

  -- if remainTime == 0 then pure ()
  --   when isChallenge halt                -- game over, implement halt in GameUI.hs, not here because Blendoku.hs only cares about logic inside one ui^.game
  if remainTime > 0 then modify $ \g -> g { _remainTime = remainTime - 1 } else pure()

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


-- initialize ui^.game's state, parameters etc.
initGame :: Bool -> Int -> IO Game
initGame isChallenge lvl = do
  gameType <- case lvl of
    0 -> return Line
    1 -> return Rectangle
    2 -> return TShape
    3 -> return HShape
    4 -> return RandomShape
  (rows, cols, board, gtBoard) <- case gameType of
    Line -> initLineBoard
    Rectangle -> initRectangleBoard
    TShape -> initTShapeBoard
    HShape -> initHShapeBoard
    RandomShape -> initRandomShapeBoard
  return  Game
    {
        _level        = lvl 
      , _board          = board
      , _gtBoard        = gtBoard
      , _cursorPos       = V2 1 1
      -- V2 0, 0 means no chosen grid
      , _chosenPos       = V2 0 0
      , _hint            = False
      , _boardRows       = rows
      , _boardCols       = cols
      , _remainTime     = 60*5*1000      -- 5 minutes      
      , _isChallenge    = isChallenge
      , _maxLevel       = 4
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
  let xs = computeGradientCoords coord cols dir
      ys = computeGradientCells c1 c2 cols 4
      gtBoard = insertColorWord (zip xs ys) emptyBoard
      lockedList = [V2 1 1, V2 cols 1]
  gtBoard <- return (foldr (M.adjust (\cell -> cell & locked .~ True)) gtBoard lockedList)
  board <- return (addCursor (shuffleBoard  gtBoard))
  return (rows, cols, board, gtBoard)

initRectangleBoard :: IO (Int, Int, Board, Board)
initRectangleBoard = do
  let rows = 3
      cols = 9
      emptyBoard = generateEmptyBoard rows cols
  c1 <- generate (elements keyColorList)
  c2 <- generate (elements keyColorList)
  c3 <- generate (elements keyColorList)
  c4 <- generate (elements keyColorList)
  let xs12 = computeGradientCoords (V2 1 1) cols R
      ys12 = computeGradientCells c1 c2 cols 4
      word12 = zip xs12 ys12
  gtBoard <- return (insertColorWord word12 emptyBoard)
  let xs34 = computeGradientCoords (V2 1 rows) cols R
      ys34 = computeGradientCells c3 c4 cols 4
      word34 = zip xs34 ys34
  gtBoard <- return (insertColorWord word34 gtBoard)
  let xs13 = computeGradientCoords (V2 1 1) rows D
      ys13 = computeGradientCells c1 c3 rows 4
      word13 = zip xs13 ys13
  gtBoard <- return (insertColorWord word13 gtBoard)
  let xs24 = computeGradientCoords (V2 cols 1) rows D
      ys24 = computeGradientCells c2 c4 rows 4
      word24 = zip xs24 ys24
  gtBoard <- return (insertColorWord word24 gtBoard)
  let lockedList = [V2 1 1, V2 cols 1, V2 1 rows, V2 cols rows]
  gtBoard <- return (foldr (M.adjust (\cell -> cell & locked .~ True)) gtBoard lockedList)
  let board = addCursor (shuffleBoard gtBoard)
  return (rows, cols, board, gtBoard)

initTShapeBoard :: IO (Int, Int, Board, Board)
initTShapeBoard = do
  let rows = 5
      cols = 9
      emptyBoard = generateEmptyBoard rows cols
  c1 <- generate (elements keyColorList)
  c2 <- generate (elements keyColorList)
  let xs = computeGradientCoords (V2 1 1) cols R
      ys = computeGradientCells c1 c2 cols 4
      word12 = zip xs ys
  gtBoard <- return (insertColorWord word12 emptyBoard)
  c3 <- generate (elements keyColorList)
  let coordMid = V2 (cols `div` 2) 1
      cMid = gtBoard M.! coordMid ^. color
      xs = computeGradientCoords coordMid rows D
      ys = computeGradientCells cMid c3 rows 4
      wordMid3 = zip xs ys
  gtBoard <- return (insertColorWord wordMid3 gtBoard)
  let lockedList =  [V2 1 1, V2 cols 1, V2 (cols `div` 2) rows]
  gtBoard <- return (foldr (M.adjust (\cell -> cell & locked .~ True)) gtBoard lockedList)
  let board = addCursor (shuffleBoard gtBoard)
  return (rows, cols, board, gtBoard)

initHShapeBoard :: IO (Int, Int, Board, Board)
initHShapeBoard = do
  let rows = 7
      cols = 9
      emptyBoard = generateEmptyBoard rows cols
  c1 <- generate (elements keyColorList)
  c2 <- generate (elements keyColorList)
  let xs = computeGradientCoords (V2 1 1) rows D
      ys = computeGradientCells c1 c2 rows 4
      word12 = zip xs ys
  gtBoard <- return (insertColorWord word12 emptyBoard)
  c3 <- generate (elements keyColorList)
  c4 <- generate (elements keyColorList)
  let xs = computeGradientCoords (V2 cols 1) rows D
      ys = computeGradientCells c3 c4 rows 4
      word34 = zip xs ys
  gtBoard <- return (insertColorWord word34 gtBoard)
  let coordMid12 = V2 1 ((1 + rows) `div` 2)
      coordMid34 = V2 cols ((1 + rows) `div` 2)
      cMid12 = gtBoard M.! coordMid12 ^. color
      cMid34 = gtBoard M.! coordMid34 ^. color
      xs = computeGradientCoords coordMid12 cols R
      ys = computeGradientCells cMid12 cMid34 cols 4
      wordMid = zip xs ys
  gtBoard <- return (insertColorWord wordMid gtBoard)
  let lockedList = [V2 1 1, V2 cols 1, V2 1 rows, V2 cols rows]
  gtBoard <- return (foldr (M.adjust (\cell -> cell & locked .~ True)) gtBoard lockedList)
  let board = addCursor (shuffleBoard gtBoard)
  return (rows, cols, board, gtBoard)

initRandomShapeBoard :: IO (Int, Int, Board, Board)
initRandomShapeBoard = do
  let rows = 9
      cols = 9
      gt = generateEmptyBoard rows cols
      
  gt <- updateBoard gt
  gt <- updateBoard gt
  gt <- updateBoard gt
  gt <- updateBoard gt
  gt <- updateBoard gt
  gt <- updateBoard gt
  gt <- updateBoard gt

  -- let board = gt
  let board = addCursor (shuffleBoard gt)
  return (rows, cols, board, gt)


updateBoard :: Board -> IO Board
updateBoard board = do
    word' <- generateNextValidColorWord board
    -- set the first grid and the last grid to be locked
    let lockedList = [fst (head word'), fst (last word')]
    board <- return (insertColorWord word' board)
    return (foldr (M.adjust (\cell -> cell & locked .~ True)) board lockedList)

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
  where xs = [V2 x y  | x <- [1..col], y <- [1..row]]
        ys = replicate (row * col) (Cell (0, 0, 0) False False False)

computeGradientCoords :: Coord -> Int -> Direction -> [Coord]
computeGradientCoords coord n dir =
  case dir of
    L ->  reverse [V2 x y | x <- [x0 - n .. x0], y <- [y0]]
    R ->  [V2 x y | x <- [x0 .. x0 + n], y <- [y0]]
    U ->  reverse [V2 x y | x <- [x0], y <- [y0 - n .. y0]]
    D ->  [V2 x y | x <- [x0], y <- [y0 .. y0 + n]]
  where (V2 x0 y0) = coord

-- we want to keep the first and the last cell the same while generating the gradient
-- generateGradient' takes the start and end color and the number of cells in between
computeGradientCells :: ColorVector -> ColorVector -> Int-> Int  -> [Cell]
computeGradientCells (r1, g1, b1) (r2, g2, b2) n scale =
  let first = (r1, g1, b1)
      last = (r2, g2, b2)
      scaledFirst = (r1 `div` scale, g1 `div` scale, b1 `div` scale)
      scaledLast = (r2 `div` scale, g2 `div` scale, b2 `div` scale)
      gradient = map (\(r, g, b) ->  (r * scale, g * scale, b * scale)) (generateGradient scaledFirst scaledLast (n-2))
  in map (\c -> Cell c False False False) ([first] ++ gradient ++ [last])
  where
        generateGradient (r1, g1, b1) (r2, g2, b2) n = zip3 (generateGradient' r1 r2 n) (generateGradient' g1 g2 n) (generateGradient' b1 b2 n)
        generateGradient' start end n = map (\x -> x * (end - start) `div` (n+1) + start) [1..n]

insertColorWord :: ColorWord -> Board -> Board
insertColorWord word board = foldl (\b (k, v) -> M.insert k v b) board word

addCursor :: Board -> Board
addCursor = M.adjust (\cell -> cell & hovered .~ True) (V2 1 1)

shuffle :: [a] -> [a]
shuffle xs = map snd (sortOn fst $ zip shuffle2 xs)
  where shuffle2 = take (length xs) $ randomRs (1::Int, maxBound) (mkStdGen 42)

--  remove first element (black) because it is a special color that represents "wall"
keyColorList :: [ColorVector]
keyColorList = shuffle (tail [ (r, g, b) | r <- [0, 32..255], g <- [0, 32..255], b <- [0, 32..255]])

validateWord :: ColorWord -> Board -> Bool
validateWord word board =
  all (\(k, v) -> validateSingle k v board word) word &&
  -- for beforeFirst and afterLast, either they are not on board, or they are on board and are black
  (not (beforeFirstCoord `M.member` board) || (board M.! beforeFirstCoord ^. color == (0, 0, 0))) &&
  (not (afterLastCoord `M.member` board) || (board M.! afterLastCoord ^. color == (0, 0, 0)))
  where 
    firstCoord = fst (head word)
    sndCoord = fst (head (tail word))
    diff = sndCoord - firstCoord
    beforeFirstCoord = firstCoord - diff
    lastCoord = fst (last word)
    lastButTwo = fst (word !! (length word - 2))
    afterLastCoord = lastCoord + (lastCoord - lastButTwo)

validateSingle :: Coord -> Cell -> Board -> ColorWord -> Bool
validateSingle coord cell board word =
  M.member coord board &&
  -- ((board M.! coord ^. color == cell ^. color) || (board M.! coord ^. color == (0, 0, 0))) 
  ((board M.! coord ^. color == cell ^. color) || (board M.! coord ^. color == (0, 0, 0) && neighborsNotIn)) 
  where
    fourNeighbors = [V2 (x+1) y, V2 (x-1) y, V2 x (y-1), V2 x (y+1)]
    V2 x y = coord
    neighborsNotIn = all (\c -> not (M.member c board) || neighborInWord c || (board M.! c ^. color == (0, 0, 0))) fourNeighbors
    neighborInWord c = c `elem` map fst word


generateNextValidColorWord :: Board -> IO ColorWord
generateNextValidColorWord board = do
  let nonEmptyGrids = M.filter (\cell -> cell ^. color /= (0, 0, 0)) board
      candidateGrids = 
        if M.size nonEmptyGrids == 0 
          -- V2 4 4 is the start position
          then [V2 4 4]
          else M.keys nonEmptyGrids
  coord <- generate (elements candidateGrids)
  currentColor <- if coord /= (V2 4 4) then return (board M.! coord ^. color) else generate (elements keyColorList)
  word' <- generateNextColorWord coord currentColor
  -- putStrLn ("try" ++ show word')
  if validateWord word' board
    -- then do {putStrLn (show coord); putStrLn (show word'); putStrLn (show board); return word'}
    then return word'
    else generateNextValidColorWord board

generateNextColorWord ::  Coord -> ColorVector -> IO ColorWord
generateNextColorWord coord color = do
  n <- generate (choose (3::Int, 5))
  dir <- generate (elements [L, R, U, D])
  let c1 = color
  c2 <- generate (elements keyColorList)
  let xs = computeGradientCoords coord n dir
      ys = computeGradientCells c1 c2 n 4
  return (zip xs ys)

initChallengeBoard :: IO (Int, Int, Board, Board)
initChallengeBoard = do
  initLineBoard

-- Challenge mode: advance to next level
-- nextLevel :: Game -> IO Game
-- nextLevel g = do
--   let newLevel = (_level g) + 1
--   (_, _, newBoard, newGtBoard) <- initChallengeBoard newLevel
--   return g { _level = newLevel, _board = newBoard, _gtBoard = newGtBoard }
