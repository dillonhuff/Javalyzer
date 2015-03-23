module Javalyzer.Java.SyntaxTests(allSyntaxTests) where

import Control.Monad
import Data.List as L
import Data.Set as S

import Javalyzer.Java.Desugared
import Javalyzer.Java.Parser
import Javalyzer.Java.Syntax
import Javalyzer.TestUtils
import Javalyzer.Utils

allSyntaxTests = do
  testFunction dsVarIdent identCases
  testFunction (dsTypeParam typeParams) typeParamCases
  testFunctionFiles (testPath ++ "desugarTests/") desugarStr desugarCompUnitCases
  testFunction (dsBlockStmt typeParams) blockStmtCases
  testFunctionFiles (testPath ++ "desugarTests/") desugarFirstStmt desugarFirstStmtCases
  testFunction dsMods modsCases

desugarFirstStmt str = liftM (head . firstBlock) (desugarStr str)
  
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
  L.map (\(x, y) -> (x ++ ".java", return y))
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
    dCompilationUnit Nothing [] [] [oneMethodClass]),
   ("ConstructorClass",
    dCompilationUnit Nothing [] [] [oneConstructorClass])]



blockStmtCases =
  L.map (\(x, y) -> (x, return y))
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

desugarFirstStmtCases =
  L.map (\(x, y) -> (x ++ ".java", return y))
  [("Return", dReturn Nothing),
   ("ReturnChar", dReturn $ Just $ dLit $ dChar 'a'),
   ("PrimaryFieldAccess", dReturn $ Just $ dPrimaryFieldAccess dThis (dVarIdent "m")),
   ("ExpName", dReturn $ Just $ dExpName $ dName [dVarIdent "nope"])]

oneFieldClass =
  dClassDecl "FieldClass" [] Nothing [dVarDecl noMods (dPrimType $ dIntT) (dVarIdent "i")] [] []

oneMethodClass =
  dClassDecl "MethodClass" [] Nothing [] [dMethod noMods [] Nothing "emptyMethod" [] [] []] []

oneConstructorClass =
  dClassDecl "ConstructorClass" [] Nothing [] [] [dConstructor (mods private realExtendable nonStatic) [] "ConstructorClass" [dVarDecl noMods (dRefType $ dClassRefType $ dClassType [(dClassName "Object", [])]) (dVarIdent "obj")] [] (dConstructorBody Nothing [])]

modsCases =
  L.map (\(x, y) -> (x, return y))
  [([], noMods),
   ([jPrivate], mods private realExtendable nonStatic)]
