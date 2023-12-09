{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


import Blendoku
import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck as QC
import Control.Exception (assert)
import Linear.V2
import Control.Monad (replicateM_)

import Control.Lens ((^.))
import Data.Map as M (fromList, toList, (!), insert, member, keys, union, empty, adjust, null, size, lookup)
-- import Data.String (String)
-- import Control.Monad.Trans.State (StateT(..), gets, evalStateT, execStateT, modify, execStateT)
-- import Control.Applicative (Applicative(pure))
-- import Control.Monad (when)


main :: IO ()
main = do
    defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "Blendoku Test Suite"
    [ 
        wordTest
    ,   boardTest
    ]

wordTest:: TestTree
wordTest = testGroup "ColorWord Tests"
    [
        test_keyColorList
    ,   test_computeGradientCoordinateLength
    ,   test_computeGradientCellLength
    ,   test_generateNextColorWord
    ]

boardTest :: TestTree
boardTest = testGroup "Board Tests"
    [ 
        test_emptyBoardBlack
    ,   test_emptyBoardLength
    ,   test_shuffleBoardKeepLocked
    ,   test_updateBoardDecreaseBlack
    ]

-- check all values (r, g, b) in the list are a multiple of 32
test_keyColorList :: TestTree
test_keyColorList = testCase "Test keyColorList" $ do
    let allMultiplesOf32 = all (\(r, g, b) -> r `mod` 32 == 0 && g `mod` 32 == 0 && b `mod` 32 == 0) keyColorList
    assertBool "Not all values are multiples of 32" allMultiplesOf32

test_computeGradientCoordinateLength::TestTree
test_computeGradientCoordinateLength = testCase "Test computeGradientCoordinateLength" $ do
    replicateM_ 100 $ do
        x <- generate arbitrary :: IO Int
        y <- generate arbitrary :: IO Int
        n <- generate (choose (1, 20))
        dir <- generate (elements [R, L, U, D])
        let coord = V2 x y
            coordLength = length (computeGradientCoords coord n dir )
        assertEqual "computeGradientCoordinateLength failed" n coordLength

test_computeGradientCellLength::TestTree
test_computeGradientCellLength = testCase "Test computeGradientCellLength" $ do
    replicateM_ 100 $ do
        -- n is the total number of cells, must be greater than 2
        n <- generate (choose (3, 20))
        c1 <- generate arbitrary :: IO ColorVector
        c2 <- generate arbitrary :: IO ColorVector
        let 
            scale = 4
            cells = computeGradientCells c1 c2 n scale
            cellLength = length cells
        assertEqual "computeGradientCellLength failed" n cellLength

test_generateNextColorWord :: TestTree
test_generateNextColorWord = testCase "Test generateNextColorWord" $ do
    replicateM_ 100 $ do
        x <- generate arbitrary :: IO Int
        y <- generate arbitrary :: IO Int
        let coord = V2 x y
        colorVector <- generate arbitrary :: IO ColorVector
        colorWord <- generateNextColorWord coord colorVector
        assertEqual "generateNextColorWord failed" coord (fst (head colorWord))

-- given random generated (m, n), check that the number of keys in the board is m * n
prop_emptyBoardLength :: Int -> Int -> Property
prop_emptyBoardLength m n = m > 0 && n > 0 QC.==> 
    length (generateEmptyBoard m n) == m * n

prop_emptyBoardBlack :: Int -> Int -> Property
prop_emptyBoardBlack m n = m > 0 && n > 0 QC.==>
    let board = generateEmptyBoard m n
        blackItems = filter (\(_, v) -> v ^. color == (0, 0, 0)) (toList board)
    in length blackItems == m * n

test_emptyBoardLength :: TestTree
test_emptyBoardLength = testCase "Test emptyBoardLength" $ do 
    result <- quickCheckWithResult stdArgs prop_emptyBoardLength
    assertBool "test emptyBoardLength failed" (isSuccess result)

test_emptyBoardBlack :: TestTree
test_emptyBoardBlack = testCase "Test emptyBoardBlack" $ do
    result <- quickCheckWithResult stdArgs prop_emptyBoardBlack
    assertBool "test emptyBoardBlack failed" (isSuccess result)

test_shuffleBoardKeepLocked :: TestTree
test_shuffleBoardKeepLocked = testCase "Test shuffleBoardKeepLocked" $ do
    let board = generateEmptyBoard 9 9
    board <- updateBoard board
    board <- updateBoard board
    board <- updateBoard board
    let newBoard = shuffleBoard board
    -- assert that the locked items are not changed
        lockedItems = filter (\(_, v) -> v ^. locked) (toList board)
        -- for all locked items, check that the color is the same as the one in shuffled board
        in assertBool "shuffle doesn't keep locked items the same" (all (\(k, v) -> v^. color == (newBoard ! k) ^. color) lockedItems)


test_updateBoardDecreaseBlack :: TestTree
test_updateBoardDecreaseBlack = testCase "Test updateBoardIncreaseNonBlack" $ do
    let board = generateEmptyBoard 9 9
        blackItems = filter (\(_, v) -> v ^. color == (0, 0, 0)) (toList board)
    board <- updateBoard board
    let updatedBlackItems = filter (\(_, v) -> v ^. color == (0, 0, 0)) (toList board)
    assertBool "updateBoard doesn't increase black items" (length updatedBlackItems < length blackItems)




