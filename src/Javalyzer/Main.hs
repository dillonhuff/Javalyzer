module Javalyzer.Main(main) where

import Javalyzer.FileManipulation

--projectDir = "/Users/dillon/javaTestProjects/main/applemenu/src/com/apple/eawt/"
--projectDir = "/Users/dillon/javaTestProjects/main/"
--projectDir = "/Users/dillon/javaTestProjects/main/ant.debugger/src/"
projectDir = "/Users/dillon/javaTestProjects/main/ant.debugger/src/org/netbeans/modules/ant/debugger/"

main :: IO ()
main = do
  source <- allFilesWithExtensions javaExtensions projectDir
  res <- mapM (applyToFileContents (\x -> if length x > 100000 then 1 else 0)) source
  putStrLn $ show res
