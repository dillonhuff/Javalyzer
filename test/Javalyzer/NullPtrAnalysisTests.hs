module Javalyzer.NullPtrAnalysisTests(
  allNullPtrAnalysisTests) where

import Data.List as L

import Javalyzer.NullPtrAnalysis
import Javalyzer.UJava
import Javalyzer.TestUtils
import Javalyzer.Utils

allNullPtrAnalysisTests =
  testFunction (checkClassForNullPtrs classH) nullPtrCases

classH = newClassHierarchy

nullPtrCases =
  L.map (\(x, y) -> (x, JSuccess y))
  [(objectClass, False),
   (tinyClass, True),
   (noNullDerefClass, False)]

tinyClass = uClass "Tiny" [field (cRef "Object") "m"] [nullObjMethod]

nullObjMethod =
  method
        Nothing
        "nullMethod"
        []
        [fieldDecl (cRef "Tiny") "t",
         fieldDecl (cRef "Object") "p",
         asg (vLhs "p") (fieldAccExp "t" "m")]

noNullDerefClass = uClass "NoDeref" [field primInt "m"] [noNullMethod]

noNullMethod =
  method
        Nothing
        "noNull"
        []
        [fieldDecl primInt "t",
         fieldDecl (cRef "NoDeref") "noD",
         asg (vLhs "noD") (newInst "NoDeref"),
         asg (fLhs "noD" "m") (intLit 12),
         asg (vLhs "t") (fieldAccExp "noD" "m")]
