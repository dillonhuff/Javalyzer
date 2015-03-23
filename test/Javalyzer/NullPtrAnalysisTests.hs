module Javalyzer.NullPtrAnalysisTests(
  allNullPtrAnalysisTests) where

import Data.List as L

import Javalyzer.NullPtrAnalysis
import Javalyzer.UJava
import Javalyzer.TestUtils
import Javalyzer.Utils

allNullPtrAnalysisTests =
  testFunction (checkClassForNullPtrs classH) nullPtrCases

classH = newClassHierarchy [objectClass,
                            tinyClass,
                            noNullDerefClass,
                            linkedListDerefClass]

nullPtrCases =
  L.map (\(x, y) -> (x, JSuccess y))
  [(objectClass, False),
   (tinyClass, True),
   (noNullDerefClass, False),
   (linkedListDerefClass, True)]

tinyClass = uClass "Tiny" [field (cRef "Object") "m"] [nullObjMethod]

nullObjMethod =
  method
        Nothing
        "nullMethod"
        []
        [fieldDeclInstr (cRef "Tiny") "t",
         fieldDeclInstr (cRef "Object") "p",
         asg (vLhs "p") (fieldAccExp "t" "m")]

noNullDerefClass = uClass "NoDeref" [field primInt "m"] [noNullMethod]

noNullMethod =
  method
        Nothing
        "noNull"
        []
        [fieldDeclInstr primInt "t",
         fieldDeclInstr (cRef "NoDeref") "noD",
         asg (vLhs "noD") (newInst "NoDeref"),
         asg (fLhs "noD" "m") (intLit 12),
         asg (vLhs "t") (fieldAccExp "noD" "m")]

linkedListDerefClass = uClass "LinkedList" [field (cRef "LinkedList") "next"] [linkDeref]

linkDeref =
  method
        Nothing
        "linkDeref"
        []
        [fieldDeclInstr (cRef "LinkedList") "l1",
         asg (vLhs "l1") (newInst "LinkedList"),
         fieldDeclInstr (cRef "LinkedList") "l2",
         asg (vLhs "l2") (newInst "LinkedList"),
         asg (fLhs "l1" "next") (vRhs "l2"),
         fieldDeclInstr (cRef "LinkedList") "l3",
         asg (vLhs "l3") (newInst "LinkedList"),
         asg (fLhs "l2" "next") (vRhs "l3"),
         fieldDeclInstr (cRef "LinkedList") "l4",
         asg (vLhs "l4") (vRhs "l1"),
         asg (vLhs "l4") (fieldAccExp "l4" "next"),
         asg (vLhs "l4") (fieldAccExp "l4" "next"),
         asg (vLhs "l4") (fieldAccExp "l4" "next"),
         asg (vLhs "l4") (fieldAccExp "l4" "next")]
