module Javalyzer.Main(main) where

import Data.List as L
import Data.Time.LocalTime

import Javalyzer.Desugared
import Javalyzer.FileManipulation
import Javalyzer.Parser
import Javalyzer.Syntax
import Javalyzer.Utils

--projectDir="/Users/dillon/javaTestProjects/main/apisupport.osgidemo/osgidemo/showbundles/test/unit/src/org/netbeans/demo/osgi/showbundles/"
--projectDir = "/Users/dillon/javaTestProjects/main/"
projectDir = "/Users/dillon/javaTestProjects/main/ant.debugger/src/org/netbeans/modules/ant/debugger/"
--projectDir = "/Users/dillon/javaTestProjects/main/cnd/"

main :: IO ()
main = do
  startTime <- getZonedTime
  source <- allFilesWithExtensions javaExtensions projectDir
  res <- mapM (applyToFileContents desugarCompilationUnit parseLog) source
  endTime <- getZonedTime
  printCoverageSummary startTime endTime res

desugarCompilationUnit str = (parseCompilationUnit str) >>= dsCompilationUnit

printCoverageSummary startTime endTime res =
  let successful = L.filter (\x -> not $ isFail x) res
      failed = L.filter (\x -> isFail x) res
      numFailed = length failed
      numSucceeded = length successful
      total = numFailed + numSucceeded
      fracParsed = (fromIntegral $ numSucceeded) / (fromIntegral $ total)
      pctParsed = fracParsed * 100.0 in
  do
    putStrLn "================= Parse Results ====================="
    putStrLn $ "Start time:\t" ++ (show $ startTime)
    putStrLn $ "End time:\t" ++ (show $ endTime)
    putStrLn $ "Total files:\t" ++ (show $ total)
    putStrLn $ "Successes:\t" ++ (show $ numSucceeded)
    putStrLn $ "Failures:\t" ++ (show $ numFailed)
    putStrLn $ "Parse Cover %:\t" ++ (show pctParsed)
    putStrLn "====================================================="

parseLog :: JError DCompilationUnit -> FilePath -> String
parseLog (JSuccess unit) path = "\n\n\n" ++ path ++
         "\n**************** Parse Succeeded **********************\n\n\n"
parseLog (JFail failMsg) path = "\n" ++ path ++ "\n" ++ failMsg ++ "\n"
