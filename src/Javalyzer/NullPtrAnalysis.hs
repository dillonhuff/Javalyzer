module Javalyzer.NullPtrAnalysis(checkClassForNullPtrs) where

import Data.List as L

import Javalyzer.ExecutionEngine
import Javalyzer.Store
import Javalyzer.UJava

checkClassForNullPtrs :: ClassHierarchy -> Class -> Bool
checkClassForNullPtrs h c =
  or $ L.map (checkMethodForNullPtrs h c) (classMethods c)

checkMethodForNullPtrs :: ClassHierarchy -> Class -> Method -> Bool
checkMethodForNullPtrs h className m =
  symbolicMethodExec nullPtrException h className m

nullPtrException :: Store -> Instruction -> Bool
nullPtrException store i =
  case instrType i of
    ASSIGN -> isNullDereference (rhs i) store
    _ -> False

isNullDereference :: Exp -> Store -> Bool
isNullDereference exp s =
  case expType exp of
    FIELDACCESS -> isNull (objAccessedName exp) s
    _ -> False
