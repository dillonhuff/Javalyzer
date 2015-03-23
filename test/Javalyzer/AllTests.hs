module Javalyzer.AllTests(main) where

import Javalyzer.Analysis.NullPtrAnalysisTests
import Javalyzer.Java.ParserTests
import Javalyzer.Java.SyntaxTests

main = do
  allNullPtrAnalysisTests
  allParserTests
  allSyntaxTests
