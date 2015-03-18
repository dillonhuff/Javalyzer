module Javalyzer.ParserTests(allParserTests) where

import Javalyzer.Parser
import Javalyzer.Syntax
import Javalyzer.SyntaxConvenience
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
   ("TypeParamClass.java", typeParamClassDecl),
   ("SuperClassCon.java", superClassConDecl),
   ("ExpNameAsg.java", expNameAsgClassDecl),
   ("RetInt.java", retIntClassDecl),
   ("MarkAnn.java", markAnnClassDecl)]

svMeth mods name stmts = jMethodDecl mods [] Nothing (jIdent name) [] [] $ jBlockMethod $ jBlock stmts

emptyMeth = svMeth [jPublic] "tinyMethod" [jBlockStmt jReturnVoid]
nullSetMeth =
  svMeth
        [jPrivate]
        "setNull"
        [jLocalVars [] (jRefType $ jClassRefType $ jClassType [(jIdent "Object", [])]) [jVarDecl (jVarId (jIdent "p")) Nothing], jBlockStmt $ jExpStmt $ jAssign (jNameLhs (sJName "p")) jEqualA (jLit jNull), jBlockStmt jReturnVoid]
methodInvokeMeth =
  svMeth
        [jProtected]
        "invokeToString"
        [jLocalVars [] (jRefType $ jClassRefType $ jClassType [(jIdent "Object", [])]) [jVarDecl (jVarId (jIdent "p")) Nothing],
         jBlockStmt $ jExpStmt $ jAssign (jNameLhs (sJName "p")) jEqualA (jLit jNull),
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

superClassConDecl =
  rc [jClassTypeDecl [] (jIdent "SuperClassCon") [] Nothing []
      (jClassBody [jMemberDecl $ jConstructorDecl [] [] (jIdent "SuperClassCon") [] [] (jConstructorBody (Just $ jSuperInvoke [] []) [])])]

declString name = jLocalVars [] (sRefT "String") [jVarDecl (jVarId $ jIdent name) Nothing]

strBlck = jBlock [sLocRef "String" "first", sLocRef "String" "second", asgVarsBSt "first" "second"]
strMeth = jMethodDecl [] [] Nothing (jIdent "setStrs") [] [] $ jBlockMethod strBlck
expNameAsgClassDecl =
  rc [jClassTypeDecl [] (jIdent "ExpNameAsg") [] Nothing []
      $ jClassBody [jMemberDecl strMeth]]

ret5 = jBlockStmt $ jReturn $ jLit $ jInt 5
ret5Meth = jMethodDecl [] []  (Just $ jPrimType jIntT) (jIdent "retInt") [] [] $ jBlockMethod $ jBlock [ret5]
retIntClassDecl =
  rc [jClassTypeDecl [] (jIdent "RetInt") [] Nothing []
      $ jClassBody [jMemberDecl ret5Meth]]

retStr = jBlockStmt $ jReturn $ jLit $ jString "!@#$%^^&&"
retStrMeth = jMethodDecl [jAnnotation $ jMarkerAnnotation $ sJName "Override"] [] (Just $ sRefT "String") (jIdent "toString") [] [] $ jBlockMethod $ jBlock [retStr]
markAnnClassDecl =
  rc [jClassTypeDecl [] (jIdent "MarkAnn") [] Nothing []
      $ jClassBody [jMemberDecl retStrMeth]]
