module Javalyzer.Syntax(JParseError,
                        JCompilationUnit,
                        JTypeDecl,
                        compUnitToJ,
                        jPackageDecl,
                        jCompUnit,
                        jClassTypeDecl,
                        jClassBody,
                        jMemberDecl,
                        jMethodDecl,
                        jFieldDecl,
                        jConstructorDecl,
                        jConstructorBody,
                        jSuperInvoke,
                        jBlockMethod,
                        jBlockStmt,
                        jMethodInv,
                        jLocalVars,
                        jBlock,
                        jReturnVoid,
                        jReturn,
                        jExpStmt,
                        jInitExp,
                        jPrimType,
                        jClassRefType,
                        jClassType,
                        jAssign,
                        jFieldAccess,
                        jThis,
                        jLit,
                        jPrimaryFieldAccess,
                        jExpName,
                        jEqualA,
                        jRefType,
                        jVarId,
                        jVarDecl,
                        jPrimaryMethodCall,
                        jMethodCall,
                        jTypeParam,
                        jFormalParam,
                        jNameLhs,
                        jFieldLhs,
                        jNull,
                        jBoolean,
                        jWord,
                        jInt,
                        jChar,
                        jFloat,
                        jDouble,
                        jString,
                        jBooleanT,
                        jByteT,
                        jShortT,
                        jIntT,
                        jLongT,
                        jCharT,
                        jFloatT,
                        jDoubleT,
                        jIdent,
                        jName,
                        jPublic,
                        jPrivate,
                        jFinal,
                        jAbstract,
                        jStatic,
                        jProtected,
                        jAnnotation,
                        jParseError,
                        jMarkerAnnotation) where

import Control.Monad
import Data.List as L
import Language.Java.Syntax

import Javalyzer.Utils

data JParseError = JParseError String
                   deriving (Eq, Ord, Show)

jParseError = JParseError

data JCompilationUnit = JCompilationUnit (Maybe JPackageDecl) [JImportDecl] [JTypeDecl]
                        deriving (Eq, Ord, Show)

jCompUnit = JCompilationUnit

compUnitToJ :: CompilationUnit -> JError JCompilationUnit
compUnitToJ (CompilationUnit packDecl imports typeDecls) = do
  packDeclJ <- maybePackageDeclToJ packDecl
  importsJ <- mapM importToJ imports
  typeDeclsJ <- mapM typeDeclToJ typeDecls
  return $ jCompUnit packDeclJ importsJ typeDeclsJ

data JPackageDecl = JPackageDecl JName
                    deriving (Eq, Ord, Show)

jPackageDecl = JPackageDecl

maybePackageDeclToJ :: Maybe PackageDecl -> JError (Maybe JPackageDecl)
maybePackageDeclToJ Nothing = return Nothing
maybePackageDeclToJ (Just dec) =
  let pkg = packageDeclToJ dec in
  case pkg of
    (JSuccess p) -> JSuccess (Just p)
    (JFail str) -> (JFail str)
  
packageDeclToJ :: PackageDecl -> JError JPackageDecl
packageDeclToJ (PackageDecl n) = do
  nJ <- nameToJ n
  return $ jPackageDecl nJ

data JImportDecl = JID
                   deriving (Eq, Ord, Show)

importToJ :: ImportDecl -> JError JImportDecl
importToJ im = return JID

data JTypeDecl = JClassTypeDecl JClassDecl
                 deriving (Eq, Ord, Show)

jClassTypeDecl mods name typeParams super refTypes body =
  JClassTypeDecl $ jClassDecl mods name typeParams super refTypes body

typeDeclToJ :: TypeDecl -> JError JTypeDecl
typeDeclToJ (ClassTypeDecl (ClassDecl mods id tps sup refs body)) = do
  modsJ <- mapM modifierToJ mods
  idJ <- identToJ id
  tpsJ <- mapM typeParamToJ tps
  supJ <- superToJ sup
  refsJ <- mapM refTypeToJ refs
  bodyJ <- classBodyToJ body
  return $ jClassTypeDecl modsJ idJ tpsJ supJ refsJ bodyJ
typeDeclToJ other = fail $ (show other) ++ " is not supported by typeDeclToJ"

data JClassDecl = JClassDecl [JModifier] JIdent [JTypeParam] (Maybe JRefType) [JRefType] JClassBody
                  deriving (Eq, Ord, Show)

jClassDecl = JClassDecl

data JClassBody = JClassBody [JDecl]
                  deriving (Eq, Ord, Show)

jClassBody = JClassBody

classBodyToJ (ClassBody decls) = do
  declsJ <- mapM declToJ decls
  return $ jClassBody declsJ

data JDecl = JMemberDecl JMemberDecl
             deriving (Eq, Ord, Show)

jMemberDecl = JMemberDecl

declToJ (MemberDecl (MethodDecl mods tps retType id fparams exceptions body)) = do
  modsJ <- mapM modifierToJ mods
  tpsJ <- mapM typeParamToJ tps
  retTypeJ <- returnTypeToJ retType
  idJ <- identToJ id
  fparamsJ <- mapM formalParamToJ fparams
  exceptionsJ <- mapM exceptionTypeToJ exceptions
  bodyJ <- methodBodyToJ body
  return $ jMemberDecl $ jMethodDecl modsJ tpsJ retTypeJ idJ fparamsJ exceptionsJ bodyJ
declToJ (MemberDecl (FieldDecl mods tp varDecls)) = do
  modsJ <- mapM modifierToJ mods
  tpJ <- typeToJ tp
  varDeclsJ <- mapM varDeclToJ varDecls
  return $ jMemberDecl $ jFieldDecl modsJ tpJ varDeclsJ
declToJ (MemberDecl (ConstructorDecl mods tps id fps exps body)) = do
  modsJ <- mapM modifierToJ mods
  tpsJ <- mapM typeParamToJ tps
  idJ <- identToJ id
  fpsJ <- mapM formalParamToJ fps
  expsJ <- mapM exceptionTypeToJ exps
  bodJ <- constructorBodyToJ body
  return $ jMemberDecl $ jConstructorDecl modsJ tpsJ idJ fpsJ expsJ bodJ
                       
declToJ other = fail $ (show other) ++ " is not suported by declToJ"

data JMemberDecl
  = JMethodDecl [JModifier] [JTypeParam] (Maybe JType) JIdent [JFormalParam] [JExceptionType] JMethodBody
  | JFieldDecl [JModifier] JType [JVarDecl]
  | JConstructorDecl [JModifier] [JTypeParam] JIdent [JFormalParam] [JExceptionType] JConstructorBody
    deriving (Eq, Ord, Show)

jMethodDecl = JMethodDecl
jFieldDecl = JFieldDecl
jConstructorDecl = JConstructorDecl

data JConstructorBody = JConstructorBody (Maybe JExplConstrInv) [JBlockStmt]
                        deriving (Eq, Ord, Show)

jConstructorBody = JConstructorBody

constructorBodyToJ (ConstructorBody Nothing stmts) = do
  stmtsJ <- mapM blockStmtToJ stmts
  return $ jConstructorBody Nothing stmtsJ
constructorBodyToJ (ConstructorBody (Just explCall) stmts) = do
  explCallJ <- explConstrInvToJ explCall
  stmtsJ <- mapM blockStmtToJ stmts
  return $ jConstructorBody (Just explCallJ) stmtsJ
--constructorBodyToJ other = fail $ (show other) ++ " is not supported by constructorBodyToJ"

data JExplConstrInv
  = JSuperInvoke [JRefType] [JArgument]
    deriving (Eq, Ord, Show)

jSuperInvoke = JSuperInvoke

explConstrInvToJ (SuperInvoke refs args) = do
  refsJ <- mapM refTypeToJ refs
  argsJ <- mapM expToJ args
  return $ jSuperInvoke refsJ argsJ
explConstrInvToJ other = fail $ (show other) ++ " is not supported by explConstrInvToJ"

data JMethodBody = JMethodBody (Maybe JBlock)
                   deriving (Eq, Ord, Show)

jBlockMethod block = JMethodBody $ Just block

methodBodyToJ :: MethodBody -> JError JMethodBody
methodBodyToJ (MethodBody Nothing) = return $ JMethodBody Nothing
methodBodyToJ (MethodBody (Just (Block stmts))) = do
  stmtsJ <- mapM blockStmtToJ stmts
  return $ jBlockMethod $ jBlock stmtsJ

data JBlock = JBlock [JBlockStmt]
              deriving (Eq, Ord, Show)

jBlock stmts = JBlock stmts

data JBlockStmt
  = JBlockStmt JStmt
  | JLocalVars [JModifier] JType [JVarDecl]
    deriving (Eq, Ord, Show)

jBlockStmt = JBlockStmt
jLocalVars = JLocalVars

blockStmtToJ :: BlockStmt -> JError JBlockStmt
blockStmtToJ (BlockStmt stmt) = do
  stmtJ <- stmtToJ stmt
  return $ JBlockStmt stmtJ
blockStmtToJ (LocalVars mods tp decls) = do
  modsJ <- mapM modifierToJ mods
  tpJ <- typeToJ tp
  declsJ <- mapM varDeclToJ decls
  return $ jLocalVars modsJ tpJ declsJ
blockStmtToJ other = fail $ (show other) ++ " is not supported by blockStmtToJ"

data JStmt
  = JReturn (Maybe JExp)
  | JExpStmt JExp
    deriving (Eq, Ord, Show)

jReturnVoid = JReturn Nothing
jReturn exp = JReturn $ Just exp

jExpStmt = JExpStmt

stmtToJ :: Stmt -> JError JStmt
stmtToJ (Return Nothing) = return jReturnVoid
stmtToJ (Return (Just ex)) = do
  exJ <- expToJ ex
  return $ jReturn exJ
stmtToJ (ExpStmt exp) = do
  expJ <- expToJ exp
  return $ jExpStmt expJ
stmtToJ other = fail $ (show other) ++ " is not supported by stmtToJ"

data JExp
  = JLit JLiteral
  | JAssign JLhs JAssignOp JExp
  | JMethodInv JMethodInvocation
  | JExpName JName
  | JFieldAccess JFieldAccess
  | JThis
    deriving (Eq, Ord, Show)

jLit = JLit
jAssign = JAssign
jMethodInv = JMethodInv
jExpName = JExpName
jFieldAccess = JFieldAccess
jThis = JThis

expToJ (Lit l) = do
  lJ <- literalToJ l
  return $ jLit lJ
expToJ (Assign lhs asgOp exp) = do
  lhsJ <- lhsToJ lhs
  asgOpJ <- assignOpToJ asgOp
  expJ <- expToJ exp
  return $ jAssign lhsJ asgOpJ expJ
expToJ (MethodInv inv) = do
  invJ <- methodInvocationToJ inv
  return $ jMethodInv invJ
expToJ (ExpName n) = do
  nJ <- nameToJ n
  return $ jExpName nJ
expToJ (FieldAccess fa) = do
  faJ <- fieldAccessToJ fa
  return $ jFieldAccess faJ
expToJ This = return JThis
expToJ other = fail $ (show other) ++ " is not supported by expToJ"

type JArgument = JExp

data JFieldAccess
  = JPrimaryFieldAccess JExp JIdent
    deriving (Eq, Ord, Show)

jPrimaryFieldAccess = JPrimaryFieldAccess

fieldAccessToJ (PrimaryFieldAccess exp id) = do
  expJ <- expToJ exp
  idJ <- identToJ id
  return $ jPrimaryFieldAccess expJ idJ
fieldAccessToJ other = fail $ (show other) ++ " is not supported by fieldAccessToJ"

data JMethodInvocation
  = JPrimaryMethodCall JExp [JRefType] JIdent [JArgument]
  | JMethodCall JName [JArgument]
    deriving (Eq, Ord, Show)

jPrimaryMethodCall = JPrimaryMethodCall
jMethodCall = JMethodCall

methodInvocationToJ (PrimaryMethodCall exp refTs id args) = do
  expJ <- expToJ exp
  refTsJ <- mapM refTypeToJ refTs
  idJ <- identToJ id
  argsJ <- mapM expToJ args
  return $ jPrimaryMethodCall expJ refTsJ idJ argsJ
methodInvocationToJ (MethodCall n args) = do
  nJ <- nameToJ n
  argsJ <- mapM expToJ args
  return $ jMethodCall nJ argsJ
methodInvocationToJ other = fail $ (show other) ++ " is not supported by methodInvocationToJ"

data JLiteral
  = JNull
  | JBoolean Bool
  | JInt Integer
  | JWord Integer
  | JChar Char
  | JFloat Double
  | JDouble Double
  | JString String
    deriving (Eq, Ord, Show)

jNull = JNull
jBoolean = JBoolean
jInt = JInt
jWord = JWord
jChar = JChar
jFloat = JFloat
jDouble = JDouble
jString = JString

literalToJ Null = return jNull
literalToJ (Int i) = return $ JInt i
literalToJ (Boolean b) = return $ JBoolean b
literalToJ (Char c) = return $ JChar c
literalToJ (Float f) = return $ JFloat f
literalToJ (Double d) = return $ JDouble d
literalToJ (String s) = return $ JString s
literalToJ other = fail $ (show other) ++ " is not supported by literalToJ"

data JAssignOp
  = JEqualA
    deriving (Eq, Ord, Show)

jEqualA = JEqualA

assignOpToJ EqualA = return jEqualA
assignOpToJ other = fail $ (show other) ++ " is not supported by assignOpToJ"

data JVarDecl = JVarDecl JVarDeclId (Maybe JVarInit)
                deriving (Eq, Ord, Show)

jVarDecl = JVarDecl

varDeclToJ (VarDecl vid Nothing) = do
  vidJ <- varDeclIdToJ vid
  return $ jVarDecl vidJ Nothing
varDeclToJ (VarDecl vid (Just vinit)) = do
  vidJ <- varDeclIdToJ vid
  vinitJ <- varInitToJ vinit
  return $ jVarDecl vidJ (Just vinitJ)

data JVarDeclId
  = JVarId JIdent
    deriving (Eq, Ord, Show)

jVarId = JVarId

varDeclIdToJ (VarId id) = do
  idJ <- identToJ id
  return $ jVarId idJ
varDeclIdToJ other = fail $ (show other) ++ " is not supported by varDeclIdToJ"

data JVarInit
  = JInitExp JExp
    deriving (Eq, Ord, Show)

jInitExp = JInitExp

varInitToJ (InitExp ex) = do
  exJ <- expToJ ex
  return $ jInitExp exJ

data JModifier
  = JPublic
  | JPrivate
  | JProtected
  | JFinal
  | JAbstract
  | JStatic
  | JAnnotation JAnnotation
    deriving (Eq, Ord, Show)

jPublic = JPublic
jPrivate = JPrivate
jProtected = JProtected
jFinal = JFinal
jAbstract = JAbstract
jStatic = JStatic
jAnnotation = JAnnotation

modifierToJ :: Modifier -> JError JModifier
modifierToJ Public = return jPublic
modifierToJ Private = return jPrivate
modifierToJ Protected = return jProtected
modifierToJ Final = return jFinal
modifierToJ Abstract = return jAbstract
modifierToJ Static = return jStatic
modifierToJ (Annotation ann) = do
  annJ <- annotationToJ ann
  return $ JAnnotation annJ
modifierToJ m = fail $ show m ++ " is not a supported modifier"

data JLhs
  = JNameLhs JName
  | JFieldLhs JFieldAccess
            deriving (Eq, Ord, Show)

jNameLhs = JNameLhs
jFieldLhs = JFieldLhs

lhsToJ (NameLhs id) = do
  idJ <- nameToJ id
  return $ jNameLhs idJ
lhsToJ (FieldLhs fa) = do
  faJ <- fieldAccessToJ fa
  return $ jFieldLhs faJ 
lhsToJ other = fail $ (show other) ++ " is not supported by lhsToJ"

data JIdent = JIdent String
              deriving (Eq, Ord, Show)

jIdent = JIdent

identToJ :: Ident -> JError JIdent
identToJ (Ident n) = return $ jIdent n

data JName = JName [JIdent]
             deriving (Eq, Ord, Show)

jName = JName

nameToJ (Name ids) = do
  idsJ <- mapM identToJ ids
  return $ jName idsJ
--nameToJ other = fail $ (show other) ++ " is not supported by nameToJ"

data JType
  = JPrimType JPrimType
  | JRefType JRefType
    deriving (Eq, Ord, Show)

jRefType = JRefType
jPrimType = JPrimType

typeToJ (RefType rt) = do
  rtJ <- refTypeToJ rt
  return $ jRefType rtJ
typeToJ (PrimType pt) = do
  ptJ <- primTypeToJ pt
  return $ jPrimType ptJ

returnTypeToJ Nothing = return $ Nothing
returnTypeToJ (Just tp) = do
  tpJ <- typeToJ tp
  return $ Just tpJ

data JPrimType
  = JBooleanT
  | JByteT
  | JShortT
  | JIntT
  | JLongT
  | JCharT
  | JFloatT
  | JDoubleT
    deriving (Eq, Ord, Show)

jBooleanT = JBooleanT
jByteT = JByteT
jShortT = JShortT
jIntT = JIntT
jLongT = JLongT
jCharT = JCharT
jFloatT = JFloatT
jDoubleT = JDoubleT

primTypeToJ BooleanT = return jBooleanT
primTypeToJ ByteT = return jByteT
primTypeToJ ShortT = return jShortT
primTypeToJ IntT = return jIntT
primTypeToJ LongT = return jLongT
primTypeToJ CharT = return jCharT
primTypeToJ FloatT = return jFloatT
primTypeToJ DoubleT = return jDoubleT

data JTypeParam = JTypeParam JIdent [JRefType]
                  deriving (Eq, Ord, Show)

jTypeParam = JTypeParam

typeParamToJ (TypeParam id refs) = do
  idJ <- identToJ id
  refsJ <- mapM refTypeToJ refs
  return $ jTypeParam idJ refsJ

data JFormalParam = JFormalParam [JModifier] JType Bool JVarDeclId
                    deriving (Eq, Ord, Show)

jFormalParam = JFormalParam

formalParamToJ :: FormalParam -> JError JFormalParam
formalParamToJ (FormalParam mods tp False varDec) = do
  modsJ <- mapM modifierToJ mods
  tpJ <- typeToJ tp
  varDecJ <- varDeclIdToJ varDec
  return $ JFormalParam modsJ tpJ False varDecJ

data JRefType = JClassRefType JClassType
                deriving (Eq, Ord, Show)

jClassRefType = JClassRefType

refTypeToJ :: RefType -> JError JRefType
refTypeToJ (ClassRefType (ClassType idTypeArgList)) = do
  idTypeArgListJ <- idTypeArgListToJ idTypeArgList
  return $ jClassRefType $ jClassType idTypeArgListJ
refTypeToJ other = fail $ "refTypeToJ not implemented for " ++ (show other)

idTypeArgListToJ :: [(Ident, [TypeArgument])] -> JError [(JIdent, [JTypeArgument])]
idTypeArgListToJ ls = 
  let ids = L.map fst ls
      tArgs = L.map snd ls in
  do
    idsJ <- mapM identToJ ids
    tArgsJ <- mapM (\t -> mapM typeArgumentToJ t) tArgs
    return $ L.zip idsJ tArgsJ
--  L.map (\(id, tArgs) -> (identToJ id, L.map typeArgumentToJ tArgs)) ls

superToJ :: Maybe RefType -> JError (Maybe JRefType)
superToJ (Just ref) = do
  refJ <- refTypeToJ ref
  return $ Just refJ
superToJ Nothing = return Nothing

data JClassType = JClassType [(JIdent, [JTypeArgument])]
                  deriving (Eq, Ord, Show)

jClassType = JClassType

data JTypeArgument = JActualType
                     deriving (Eq, Ord, Show)

typeArgumentToJ ta = fail "typeArgumentToJ not implemented"

type JExceptionType = JRefType

exceptionTypeToJ :: ExceptionType -> JError JExceptionType
exceptionTypeToJ ex = refTypeToJ ex

data JAnnotation
  = JMarkerAnnotation JName
    deriving (Eq, Ord, Show)

jMarkerAnnotation = JMarkerAnnotation

annotationToJ (MarkerAnnotation n) = do
  nJ <- nameToJ n
  return $ jMarkerAnnotation nJ
annotationToJ other = fail $ (show other) ++ " is not supported by annotationToJ"
