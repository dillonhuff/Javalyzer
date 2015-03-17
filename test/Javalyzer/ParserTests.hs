module Javalyzer.ParserTests(allParserTests) where

import Javalyzer.Parser
import Javalyzer.Syntax
import Javalyzer.TestUtils

allParserTests = do
  testFunctionFiles (testPath ++ "parserTests/") parseCompilationUnit testCases

emptyClassDecl = jClassTypeDecl [] (jIdent "Empty") [] Nothing [] (jClassBody [])
testCases = [("EmptyClass.java", Right $ jCompUnit Nothing [] [emptyClassDecl])]
