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
   ("FinalClass.java", rc [finalClassDecl]),
   ("BooleanClass.java", primTypeClass "BooleanClass" jBooleanT),
   ("ByteClass.java", primTypeClass "ByteClass" jByteT),
   ("ShortClass.java", primTypeClass "ShortClass" jShortT),
   ("IntClass.java", primTypeClass "IntClass" jIntT),
   ("LongClass.java", primTypeClass "LongClass" jLongT),
   ("CharClass.java", primTypeClass "CharClass" jCharT),
   ("FloatClass.java", primTypeClass "FloatClass" jFloatT),
   ("DoubleClass.java", primTypeClass "DoubleClass" jDoubleT),
   ("AbstractClass.java", abstractClassDecl),
   ("StaticMethod.java", staticMethodClassDecl),
   ("IntAsg.java", literalVarClass "IntAsg" jIntT (jInt 12)),
   ("BoolAsg.java", literalVarClass "BoolAsg" jBooleanT (jBoolean False)),
   ("CharAsg.java", literalVarClass "CharAsg" jCharT (jChar 'x')),
   ("DoubleAsg.java", literalVarClass "DoubleAsg" jDoubleT (jDouble 1234)),
   ("EmptyConstructor.java", emptyConstructorClassDecl),
   ("StringAssign.java", stringAssignClassDecl),
   ("TypeParamClass.java", typeParamClassDecl)]

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

primTypeClass name tp =
  rc [jClassTypeDecl [] (jIdent name) [] Nothing [] (jClassBody [jMemberDecl $ jFieldDecl [] (jPrimType tp) [jVarDecl (jVarId $ jIdent "primField") Nothing]])]

abstractClassDecl =
  rc [jClassTypeDecl [jAbstract] (jIdent "AbstractClass") [] Nothing [] (jClassBody [])]

staticMethodClassDecl =
  rc [jClassTypeDecl [] (jIdent "StaticMethodClass") [] Nothing []
      (jClassBody [jMemberDecl $ jMethodDecl [jStatic] [] Nothing (jIdent "absMethod") [] [] $ jBlockMethod $ jBlock [jBlockStmt jReturnVoid]])]

vInit lit = Just $ jInitExp $ jLit lit
vDeclId = jVarId $ jIdent "v"
vDecl lit = jVarDecl vDeclId (vInit lit)
jDeclMeth pt lit = jBlockMethod $ jBlock [jLocalVars [] (jPrimType pt) [vDecl lit]]
assignMethod pt lit = jMethodDecl [] [] Nothing (jIdent "meth") [] [] (jDeclMeth pt lit)
literalVarClass name pt lit =
  rc [jClassTypeDecl [] (jIdent name) [] Nothing []
      (jClassBody [jMemberDecl (assignMethod pt lit)])]
  
emptyConstructorClassDecl =
  rc [jClassTypeDecl [] (jIdent "EmptyConstructor") [] Nothing []
      (jClassBody [jMemberDecl $ jConstructorDecl [] [] (jIdent "EmptyConstructor") [] [] (jConstructorBody Nothing [])])]

jDeclStrMeth = jBlockMethod $ jBlock [jLocalVars [] (jRefType $ jClassRefType $ jClassType [(jIdent "String", [])]) [jVarDecl (jVarId $ jIdent "str") (Just $ jInitExp $ jLit $ jString "a string!")]]
assignStrMethod = jMethodDecl [] [] Nothing (jIdent "meth") [] [] jDeclStrMeth
stringAssignClassDecl =
  rc [jClassTypeDecl [] (jIdent "StringAssign") [] Nothing [] (jClassBody [jMemberDecl assignStrMethod])]

typeParamClassDecl =
  rc [jClassTypeDecl [] (jIdent "TypeParamClass") [jTypeParam (jIdent "T") []] Nothing [] (jClassBody [])]
