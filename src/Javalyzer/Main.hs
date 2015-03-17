module Javalyzer.Main(main) where

import Data.List as L
import Language.Java.Parser

import Javalyzer.FileManipulation

--projectDir = "/Users/dillon/javaTestProjects/main/applemenu/src/com/apple/eawt/"
--projectDir = "/Users/dillon/javaTestProjects/main/"
projectDir = "/Users/dillon/javaTestProjects/main/ant.debugger/src/"
--projectDir = "/Users/dillon/javaTestProjects/main/ant.debugger/src/org/netbeans/modules/ant/debugger/"

main :: IO ()
main = do
  source <- allFilesWithExtensions javaExtensions projectDir
  putStrLn $ "Number of .java files in project: " ++ (show $ length source)
--  res <- mapM (applyToFileContents parseJava) source
--  putStrLn $ show res
--  res <- mapM (applyToFileContents (\x -> if length x > 10000 then 1 else 0)) source
--  putStrLn $ show $ L.foldl (+) 0 res

parseJava javaCode = parser compilationUnit javaCode
