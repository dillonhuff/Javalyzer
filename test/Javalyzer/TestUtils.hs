module Javalyzer.TestUtils(testFunction,
                           testFunctionFiles,
                           testPath) where

import Control.Monad
import Data.List as L
import Test.HUnit

import Javalyzer.FileManipulation

testPath = "/Users/dillon/Haskell/Bugs/Javalyzer/testCases/"

testFunction func cases = runTestTT $ makeTestCases func cases

testFunctionFiles prefix func fileResultPairs = do
  tests <- testCasesInFiles prefix func fileResultPairs
  results <- runTestTT tests
  return results

makeTestCases func cases =
  TestList $ map (\(input, expected) -> testCase func input expected) cases

testCase func input expected =
  TestCase (assertEqual ("Input: " ++ show input) expected (func input))

testCasesInFiles pathPrefix funcToTest fileNameExpectedResultPairs =
  let expected = L.map snd fileNameExpectedResultPairs
      fileNames = L.map (\(x, y) -> pathPrefix ++ x) fileNameExpectedResultPairs
      results = mapM (applyToFileContents funcToTest (\x y -> "")) fileNames in
  liftM (TestList . L.zipWith (\exp act -> TestCase $ assertEqual ("Input: " ++ show exp) exp act) expected) results
