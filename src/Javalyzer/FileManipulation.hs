module Javalyzer.FileManipulation(
  applyToFileContents,
  allFilesWithExtensions,
  javaExtensions,
  cExtensions,
  cppExtensions,
  getRecursiveContents) where

import Control.Monad (forM, liftM, mapM)
import Data.List as L
import System.Directory
import System.FilePath as Path ((</>))
import System.FilePath.Posix as Posix
import System.IO as SIO
import System.IO.Strict as StrictIO

javaExtensions = [".java"]
cExtensions = [".c", ".h"]
cppExtensions = [".cpp", ".h", ".hpp"]

applyToFileContents :: (String -> a) -> FilePath -> IO a
applyToFileContents f path = do
  fileH <- SIO.openFile path ReadMode
  fileContents <- StrictIO.hGetContents fileH
  return $ f fileContents

allFilesWithExtensions :: [String] -> FilePath -> IO [FilePath]
allFilesWithExtensions acceptableExtensions dir = do
  fileNames <- getRecursiveContents dir
  let sourceFileNames = L.filter (hasAcceptableExtension acceptableExtensions) fileNames in
    return sourceFileNames

hasAcceptableExtension :: [String] -> FilePath -> Bool
hasAcceptableExtension acceptableExtensions path = L.elem suffix acceptableExtensions
  where
    suffix = Posix.takeExtension path

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", "..", ".git", ".hg"]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)
