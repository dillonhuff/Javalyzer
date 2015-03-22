module Javalyzer.NullPtrAnalysisTests(
  allNullPtrAnalysisTests) where

import Javalyzer.NullPtrAnalysis
import Javalyzer.UJava
import Javalyzer.TestUtils

allNullPtrAnalysisTests =
  testFunction (checkClassForNullPtrs classH) nullPtrCases

classH = newClassHierarchy

nullPtrCases =
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
         asg (vLhs "p") (fieldAcc "t" "m")]

noNullDerefClass = uClass "NoDeref" [field (cRef "Object") "m"] []
