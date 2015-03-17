module Javalyzer.Main(main) where

import Data.List as L

import Javalyzer.FileManipulation
import Javalyzer.Parser
import Javalyzer.Syntax
import Javalyzer.Utils

--projectDir="/Users/dillon/javaTestProjects/main/apisupport.osgidemo/osgidemo/showbundles/test/unit/src/org/netbeans/demo/osgi/showbundles/"
projectDir = "/Users/dillon/javaTestProjects/main/"
--projectDir = "/Users/dillon/javaTestProjects/main/ant.debugger/src/org/netbeans/modules/ant/debugger/"

main :: IO ()
main = do
  source <- allFilesWithExtensions javaExtensions projectDir
  res <- mapM (applyToFileContents parseCompilationUnit parseLog) source
  putStrLn "DONE"

parseLog :: JError JCompilationUnit -> FilePath -> String
parseLog (JSuccess unit) path = "\n\n\n" ++ path ++
         "\n**************** Parse Succeeded **********************\n\n\n"
parseLog (JFail failMsg) path = "\n" ++ path ++ "\n" ++ failMsg ++ "\n"
