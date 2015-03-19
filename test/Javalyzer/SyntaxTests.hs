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
    dCompilationUnit Nothing [] [] [dClassDecl "Empty" [] Nothing [] [] []])]

blockStmtCases =
  []
