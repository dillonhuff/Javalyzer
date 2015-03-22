module Javalyzer.NullPtrAnalysis(checkClassForNullPtrs) where

import Control.Monad
import Data.List as L

import Javalyzer.ExecutionEngine
import Javalyzer.Store
import Javalyzer.UJava
import Javalyzer.Utils

checkClassForNullPtrs :: ClassHierarchy -> Class -> JError Bool
checkClassForNullPtrs h c = do
  methodCheckResults <- mapM (checkMethodForNullPtrs h c) (classMethods c)
  return $ or methodCheckResults

checkMethodForNullPtrs :: ClassHierarchy -> Class -> Method -> JError Bool
checkMethodForNullPtrs h className m =
  symbolicMethodExec nullPtrException h className m

nullPtrException :: Store -> Instruction -> JError Bool
nullPtrException store i =
  case instrType i of
    ASSIGN -> isNullDereference (rhs i) store
    _ -> JSuccess False

isNullDereference :: Exp -> Store -> JError Bool
isNullDereference exp s =
  case expType exp of
    FIELDACCESS -> isNull (objAccessedName $ getFieldAccFromExp exp) s
    NEWINST -> success False
    LITERAL -> success False
