module Javalyzer.ParserTests(allParserTests) where

import Javalyzer.Parser
import Javalyzer.Syntax
import Javalyzer.TestUtils

allParserTests = do
  testFunctionFiles (testPath ++ "parserTests/") parseCompilationUnit testCases

emptyClassDecl = jClassTypeDecl [] (jIdent "Empty") [] Nothing [] (jClassBody [])

emptyMeth = jMethodDecl [jPublic] [] Nothing (jIdent "tinyMethod") [] [] $ jBlockMethod $ jBlock [jBlockStmt jReturnVoid]

emptyMethodBody =
  jClassBody [jMemberDecl emptyMeth]
emptyMethodClassDecl =
  jClassTypeDecl [] (jIdent "EmptyMethod") [] (Just $ jClassRefType $ jClassType [(jIdent "Object", [])]) [] emptyMethodBody


testCases =
  [("EmptyClass.java", Right $ jCompUnit Nothing [] [emptyClassDecl]),
   ("EmptyMethod.java", Right $ jCompUnit Nothing [] [emptyMethodClassDecl])]
