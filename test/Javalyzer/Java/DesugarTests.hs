module Javalyzer.Java.DesugarTests(allDesugarTests) where

import Control.Monad
import Data.List as L

import Javalyzer.Java.Desugared
import Javalyzer.TestUtils

allDesugarTests =
  testFunction buildPackages buildPackagesCases

buildPackagesCases =
  L.map (\(x, y) -> (x, sequence y))
  [([], []),
   ([oneClassUnit], [oneClassPackage])]

oneClassUnit =
  dCompilationUnit (Just $ dPackage ["my", "package"]) [] [] [emptyClass "Nope"]

emptyClass n =
  dClassDecl n [] Nothing [] [] [] 

oneClassPackage =
  package ["my", "package"] ["Nope"]
