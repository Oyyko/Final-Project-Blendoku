{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Blendoku 
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    defaultMain testSuite

testSuite :: TestTree
testSuite = testGroup "Blendoku Test Suite"
    [ blendokuTests
    , anotherTestGroup
    ]

blendokuTests :: TestTree
blendokuTests = testGroup "Blendoku Tests"
    [ testcase_1
    ]

testcase_1 :: TestTree
testcase_1 = testCase "Example Test 1" $ do
    assertEqual "Failed test case 1" True True

anotherTestGroup :: TestTree
anotherTestGroup = testGroup "Another Test Group"
    [ testcase_2
    ]

testcase_2 :: TestTree
testcase_2 = testCase "Example Test 2" $ do
    assertEqual "Failed test case 2" True True




