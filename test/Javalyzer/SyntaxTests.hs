module Javalyzer.SyntaxTests(allSyntaxTests) where

import Data.List as L
import Data.Set as S

import Javalyzer.Desugared
import Javalyzer.Parser
import Javalyzer.Syntax
import Javalyzer.TestUtils
import Javalyzer.Utils

allSyntaxTests = do
  testFunction dsVarIdent identCases
  testFunction (dsTypeParam typeParams) typeParamCases
  testFunctionFiles (testPath ++ "desugarTests/") desugarStr desugarCompUnitCases
  testFunction (dsBlockStmt typeParams) blockStmtCases

identCases =
  [(jIdent "tame", dVarIdent "tame")]

typeParams = S.fromList $ [dTypeParam "K" [], dTypeParam "L" []]

typeParamCases =
  [(jTypeParam (jIdent "T") [], dTypeParam "T" []),
   (jTypeParam (jIdent "L") [jClassRefType $ jClassType [(jIdent "K", [])]],
    dTypeParam "L" [dClassRefType $ dClassType [(dTypeVar "K", [])]]),
   (jTypeParam (jIdent "U") [jClassRefType $ jClassType [(jIdent "Man", [])]],
    dTypeParam "U" [dClassRefType $ dClassType [(dClassName "Man", [])]])]

desugarStr str = (parseCompilationUnit str) >>= dsCompilationUnit

desugarCompUnitCases =
  L.map (\(x, y) -> (x ++ ".java", JSuccess y))
  [("Empty", dCompilationUnit Nothing [] [] []),
   ("EmptyClass",
    dCompilationUnit Nothing [] [] [dClassDecl "Empty" [] Nothing [] [] []]),
   ("PackageClass",
    dCompilationUnit (Just (dPackage ["my", "pack", "is", "cool"])) [] [] [dClassDecl "Empty" [] Nothing [] [] []]),
   ("ImportClass",
    dCompilationUnit Nothing [dImportDecl False False ["some", "random", "imp"]] [] [dClassDecl "Empty" [] Nothing [] [] []]),
   ("FieldClass",
    dCompilationUnit Nothing [] [] [oneFieldClass]),
   ("MethodClass",
    dCompilationUnit Nothing [] [] [oneMethodClass])]


blockStmtCases =
  L.map (\(x, y) -> (x, JSuccess y))
  [(jLocalVars
    []
    (jRefType $ jClassRefType $ jClassType [(jIdent "Object", [])])
    [jVarDecl (jVarId $ jIdent "o") Nothing],
    [dLocalVarDecl noMods (dRefType $ dClassRefType $ dClassType [(dClassName "Object", [])])
    (dVarIdent "o")]),
   (jLocalVars
    []
    (jRefType $ jClassRefType $ jClassType [(jIdent "String", [])])
    [jVarDecl (jVarId $ jIdent "s") Nothing,
     jVarDecl (jVarId $ jIdent "l") Nothing],
    [dLocalVarDecl
     noMods
     (dRefType $ dClassRefType $ dClassType [(dClassName "String", [])])
     (dVarIdent "s"),
     dLocalVarDecl
     noMods
     (dRefType $ dClassRefType $ dClassType [(dClassName "String", [])])
     (dVarIdent "l")])]

oneFieldClass =
  dClassDecl "FieldClass" [] Nothing [dVarDecl noMods (dPrimType $ dIntT) (dVarIdent "i")] [] []

oneMethodClass =
  dClassDecl "MethodClass" [] Nothing [] [dMethod noMods [] Nothing "emptyMethod" [] [] []] []
