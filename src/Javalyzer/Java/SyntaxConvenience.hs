module Javalyzer.Java.SyntaxConvenience(sJName,
                                   mJName,
                                   sRefT,
                                   sVarD,
                                   sLocRef,
                                   asgVars,
                                   asgVarsSt,
                                   asgVarsBSt) where

import Data.List as L

import Javalyzer.Java.Syntax

sJName str = jName [jIdent str]
mJName strs = jName $ L.map jIdent strs

sRefT typeName =
   jRefType $ jClassRefType $ jClassType [(jIdent typeName, [])]

sLocRef typeName varName =
  jLocalVars [] (sRefT typeName) [sVarD varName]

sVarD varName =
  jVarDecl (jVarId $ jIdent varName) Nothing

asgVars lhs rhs = jAssign (jNameLhs $ sJName lhs) jEqualA (jExpName $ sJName rhs)

asgVarsSt lhs rhs =
  jExpStmt $ asgVars lhs rhs

asgVarsBSt lhs rhs =
  jBlockStmt $ asgVarsSt lhs rhs
