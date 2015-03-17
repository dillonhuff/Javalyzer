module Javalyzer.TestUtils(testFunction) where

import Control.Monad
import Data.List as L
import Test.HUnit

import Javalyzer.FileManipulation

testFunction func cases = runTestTT $ makeTestCases func cases

testFunctionFiles prefix func fileResultPairs =
  (testCasesInFiles prefix func fileResultPairs) >>= runTestTT

makeTestCases func cases =
  TestList $ map (\(input, expected) -> testCase func input expected) cases

testCase func input expected =
  TestCase (assertEqual ("Input: " ++ show input) expected (func input))

{-testCasesInFiles :: (Show a, Show b) =>
                    FilePath ->
                    (a -> b) ->
                    [(FilePath, b)] ->
                    IO String-}
testCasesInFiles pathPrefix funcToTest fileNameExpectedResultPairs =
  let expected = L.map snd fileNameExpectedResultPairs
      fileNames = L.map (\(x, y) -> pathPrefix ++ x) fileNameExpectedResultPairs
      results = mapM (applyToFileContents funcToTest) fileNames in
  liftM (TestList . L.zipWith (\exp act -> TestCase $ assertEqual ("Input: " ++ show exp) exp act) expected) results
--    let testResults = L.zipWith (\(expected, actual) -> TestCase $ assertEqual ("Input: " ++ show expected) expected actual) (L.map snd fileNameExpectedResultPairs) results in
  --    do
