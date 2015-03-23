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
      FIELDDECL -> execFieldDecl errTest h s (fieldType $ getFieldDeclFromInstr i) (fieldName $ getFieldDeclFromInstr i) is
      ASSIGN -> execAssign errTest h s (lhs i) (rhs i) is

execFieldDecl errTest h s fieldType fieldName is =
  symbolicExecInstrs errTest h (addDefaultStoreValue fieldType fieldName s) is

execAssign errTest h s l r is = do
  (expRes, sRes) <- symbolicExecExp h s r
  newStore <- setLhsValue l expRes sRes
  symbolicExecInstrs errTest h newStore is

symbolicExecExp :: ClassHierarchy -> Store -> Exp -> JError (StoreValue, Store)
symbolicExecExp c s exp =
  case expType exp of
    FIELDACCESS -> do
      f <- getObjField (getFieldAccFromExp exp) s
      return $ (f, s)
    NEWINST -> createNewInstanceOfClass (getClassNameFromExp exp) c s
    LITERAL -> let lit = createNewLiteral (getLiteralFromExp exp) in
      return $ (lit, s)
    VRHS -> do
      val <- getNameValue (getVarFromExp exp) s
      return (val, s)
    NULL -> return (nullValue, s)
