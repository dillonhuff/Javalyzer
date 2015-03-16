module Javalyzer.Main(main) where

{-import Control.Monad (forM, liftM, mapM)
import Data.List as L
import System.Directory
import System.FilePath ((</>))
import System.IO-}

import Javalyzer.FileManipulation

--projectDir = "/Users/dillon/javaTestProjects/main/applemenu/src/com/apple/eawt/"
--projectDir = "/Users/dillon/javaTestProjects/main/"
--projectDir = "/Users/dillon/javaTestProjects/main/ant.debugger/src/"
projectDir = "/Users/dillon/javaTestProjects/main/ant.debugger/src/org/netbeans/modules/ant/debugger/"

main :: IO ()
main = do
  projDirCon <-getRecursiveContents projectDir
  putStrLn $ show $ projDirCon
  source <- allFilesWithExtensions javaExtensions projectDir
--  putStrLn $ show source
  res <- mapM (applyToFileContents (\x -> if length x > 10 then 1 else 0)) source
  putStrLn $ show res
--  res <- liftM L.concat res
--  putStrLn $ show $ res
