module Javalyzer.ClassTable(buildClassTable,
                            classTable,
                            emptyClassTable) where

import Control.Monad
import Data.List as L

import Javalyzer.Symtab
import Javalyzer.Syntax
import Javalyzer.Utils

data ClassTable = ClassTable (Maybe JPackageDecl) String Symtab
                  deriving (Eq, Ord, Show)

classTable = ClassTable

emptyClassTable pkg n = ClassTable pkg n emptySymtab

 -- Class Table construction
buildClassTable :: JCompilationUnit -> JError ClassTable
buildClassTable jcu = case isSingleClass jcu of
  True -> let cl = head $ classDecls jcu
              n = className cl
              imps = imports jcu
              pkg = package jcu in
          buildSingleClassTable n pkg imps cl
  False -> fail "buildClassTable currently only supports one class per compilation unit"

buildSingleClassTable :: String ->
                         Maybe JPackageDecl ->
                         [JImportDecl] ->
                         JClassDecl ->
                         JError ClassTable
buildSingleClassTable name pkg imps cDecl = do
  classST <- buildClassSymtab cDecl
  return $ classTable pkg name classST

buildClassSymtab :: JClassDecl -> JError Symtab
buildClassSymtab cDecl =
  let memberDecls = L.map getMember $ L.filter (\x -> declType x == MEMBER) $ bodyDecls $ classBody cDecl in
  foldM (\st d -> addDeclToSymtab d st) emptySymtab memberDecls

