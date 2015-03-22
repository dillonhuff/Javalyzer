module Javalyzer.ExecutionEngine(
  symbolicMethodExec) where

import Data.List as L

import Javalyzer.Store
import Javalyzer.UJava
import Javalyzer.Utils

symbolicMethodExec :: (Store -> Instruction -> JError Bool) ->
                      ClassHierarchy ->
                      Class ->
                      Method ->
                      JError Bool
symbolicMethodExec errTest h c m =
  symbolicExecInstrs errTest h emptyStore methodInstrs
  where
    fpDecls = L.concatMap formalParamDecl (formalParams m)
    thisParamDecl = thisDecl c
    methodInstrs = fpDecls ++ thisParamDecl ++ (instructions m)

symbolicExecInstrs :: (Store -> Instruction -> JError Bool) ->
                      ClassHierarchy ->
                      Store ->
                      [Instruction] ->
                      JError Bool
symbolicExecInstrs errTest h s [] = JSuccess False
symbolicExecInstrs errTest h s (i:is) = do
  testRes <- errTest s i
  case testRes of
    True -> JSuccess True
    False -> case instrType i of
      FIELDDECL -> execFieldDecl errTest h s (fieldType i) (fieldName i) is
      ASSIGN -> execAssign errTest h s (lhs i) (rhs i) is

execFieldDecl errTest h s fieldType fieldName is =
  symbolicExecInstrs errTest h (addDefaultStoreValue fieldType fieldName s) is

execAssign errTest h s l r is =
  let expRes = symbolicExecExp h s r in
  symbolicExecInstrs errTest h (setValue l expRes s) is

symbolicExecExp :: ClassHierarchy -> Store -> Exp -> StoreValue
symbolicExecExp c s exp =
  case expType exp of
    FIELDACCESS -> error "symbolicExecExp is not implemented"--getField (objAccessedName exp) (fieldAccessedName exp) s
    _ -> error $ (show exp) ++ " is not yet supported by symbolicExecExp"
