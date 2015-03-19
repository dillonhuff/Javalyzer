module Javalyzer.ClassTableTests(allClassTableTests) where

import Data.List as L

import Javalyzer.ClassTable
import Javalyzer.Parser
import Javalyzer.Symtab
import Javalyzer.Syntax
import Javalyzer.TestUtils
import Javalyzer.Utils

classTableFromStr str = (parseCompilationUnit str) >>= buildClassTable

allClassTableTests = do
  testFunctionFiles (testPath ++ "classTableTests/") classTableFromStr testCases

testCases =
  L.map (\(x, y) -> (x ++ ".java", JSuccess y))
        [("EmptyClass", emptyClassTable Nothing "EmptyClass"),
         ("ClassWithField", simpleFieldClass)]

simpleFieldClass = classTable Nothing "ClassWithField" (addVar (jIdent "iNum") [] instanceS (jPrimType $ jIntT) userDefined emptySymtab)
