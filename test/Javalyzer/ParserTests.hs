module Javalyzer.ParserTests(allParserTests) where

import Javalyzer.Parser
import Javalyzer.Syntax
import Javalyzer.TestUtils
import Javalyzer.Utils

allParserTests = do
  testFunctionFiles (testPath ++ "parserTests/") parseCompilationUnit testCases

testCases =
  [("EmptyClass.java", rc [emptyClassDecl]),
   ("EmptyMethod.java", rc [emptyMethodClassDecl]),
   ("NullSetMethod.java", rc [nullSetMethodClassDecl]),
   ("MethodInvoke.java", rc [methodInvokeClassDecl]),
   ("PackageDecl.java", rcP (jPackageDecl (jName [jIdent "stuff", jIdent "testCases"])) [packageDeclClassDecl]),
   ("FinalClass.java", rc [finalClassDecl])]

svMeth mods name stmts = jMethodDecl mods [] Nothing (jIdent name) [] [] $ jBlockMethod $ jBlock stmts

emptyMeth = svMeth [jPublic] "tinyMethod" [jBlockStmt jReturnVoid]
nullSetMeth =
  svMeth
        [jPrivate]
        "setNull"
        [jLocalVars [] (jRefType $ jClassRefType $ jClassType [(jIdent "Object", [])]) [jVarDecl (jVarId (jIdent "p")) Nothing], jBlockStmt $ jExpStmt $ jAssign (jNameLhs (jName [jIdent "p"])) jEqualA (jLit jNull), jBlockStmt jReturnVoid]
methodInvokeMeth =
  svMeth
        [jProtected]
        "invokeToString"
        [jLocalVars [] (jRefType $ jClassRefType $ jClassType [(jIdent "Object", [])]) [jVarDecl (jVarId (jIdent "p")) Nothing],
         jBlockStmt $ jExpStmt $ jAssign (jNameLhs (jName [jIdent "p"])) jEqualA (jLit jNull),
         jBlockStmt $ jExpStmt $ jMethodInv $ jMethodCall (jName [jIdent "p", jIdent "toString"]) [],
         jBlockStmt jReturnVoid]


rc :: [JTypeDecl] -> JError JCompilationUnit
rc classDecls = return $ jCompUnit Nothing [] classDecls

rcP pkg classDecls = return $ jCompUnit (Just pkg) [] classDecls

emptyClassDecl = jClassTypeDecl [] (jIdent "Empty") [] Nothing [] (jClassBody [])

emptyMethodBody =
  jClassBody [jMemberDecl emptyMeth]
  
emptyMethodClassDecl =
  jClassTypeDecl [] (jIdent "EmptyMethod") [] (Just $ jClassRefType $ jClassType [(jIdent "Object", [])]) [] emptyMethodBody

nullSetMethodClassDecl =
  jClassTypeDecl [] (jIdent "NullSetMethod") [] Nothing [] $ jClassBody [jMemberDecl emptyMeth, jMemberDecl nullSetMeth]

methodInvokeClassDecl =
  jClassTypeDecl [jPublic] (jIdent "MethodInvoke") [] Nothing [] $ jClassBody [jMemberDecl methodInvokeMeth]

packageDeclClassDecl =
  jClassTypeDecl [jPublic] (jIdent "PackageDecl") [] Nothing [] $ jClassBody [jMemberDecl methodInvokeMeth]

finalClassDecl =
  jClassTypeDecl [jFinal] (jIdent "FinalClass") [] Nothing [] $ jClassBody [jMemberDecl methodInvokeMeth, jMemberDecl emptyMeth, jMemberDecl nullSetMeth]
