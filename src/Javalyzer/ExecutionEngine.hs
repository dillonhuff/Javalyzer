module Javalyzer.ExecutionEngine(
  symbolicMethodExec) where

import Data.List as L

import Javalyzer.Store
import Javalyzer.UJava

symbolicMethodExec :: (Store -> Instruction -> Bool) ->
                      ClassHierarchy ->
                      Class ->
                      Method ->
                      Bool
symbolicMethodExec errTest h c m =
  symbolicExecInstrs errTest h emptyStore methodInstrs
  where
    fpDecls = L.concatMap formalParamDecl (formalParams m)
    thisParamDecl = thisDecl c
    methodInstrs = fpDecls ++ thisParamDecl ++ (instructions m)

symbolicExecInstrs :: (Store -> Instruction -> Bool) ->
                      ClassHierarchy ->
                      Store ->
                      [Instruction] ->
                      Bool
symbolicExecInstrs errTest h s [] = False
symbolicExecInstrs errTest h s (i:is) =
  case errTest s i of
    True -> True
    False -> case instrType i of
      FIELDDECL -> execFieldDecl errTest h s (fieldType i) (fieldName i) is
      ASSIGN -> execAssign errTest h s (lhs i) (rhs i) is

execFieldDecl errTest h s fieldType fieldName is =
  symbolicExecInstrs errTest h (addStoreValue fieldType fieldName s) is

execAssign errTest h s l r is =
  let expRes = symbolicExecExp h s r in
  symbolicExecInstrs errTest h (setValue l expRes s) is

symbolicExecExp :: ClassHierarchy -> Store -> Exp -> StoreValue
symbolicExecExp c s exp =
  case expType exp of
    FIELDACCESS -> getField (objAccessedName exp) (fieldAccessedName exp) s
    _ -> error $ (show exp) ++ " is not yet supported by symbolicExecExp"
