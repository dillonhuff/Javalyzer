module Javalyzer.Java.Parser(parseCompilationUnit) where

import Data.List as L
import Language.Java.Parser
import Language.Java.Syntax
import Text.Parsec.Error

import Javalyzer.Java.Syntax
import Javalyzer.Utils

parseCompilationUnit :: String -> JError JCompilationUnit
parseCompilationUnit name = case parser compilationUnit name of
  Left err -> fail $ show err
  Right compUnit -> compUnitToJ compUnit

compUnitToJ :: CompilationUnit -> JError JCompilationUnit
compUnitToJ (CompilationUnit packDecl imports typeDecls) = do
  packDeclJ <- maybePackageDeclToJ packDecl
  importsJ <- mapM importToJ imports
  typeDeclsJ <- mapM typeDeclToJ typeDecls
  return $ jCompUnit packDeclJ importsJ typeDeclsJ

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

importToJ :: ImportDecl -> JError JImportDecl
importToJ (ImportDecl onlyStatic name onlyOne) = do
  nameD <- nameToJ name
  return $ jImportDecl onlyStatic nameD onlyOne

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

classBodyToJ (ClassBody decls) = do
  declsJ <- mapM declToJ decls
  return $ jClassBody declsJ

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

constructorBodyToJ (ConstructorBody Nothing stmts) = do
  stmtsJ <- mapM blockStmtToJ stmts
  return $ jConstructorBody Nothing stmtsJ
constructorBodyToJ (ConstructorBody (Just explCall) stmts) = do
  explCallJ <- explConstrInvToJ explCall
  stmtsJ <- mapM blockStmtToJ stmts
  return $ jConstructorBody (Just explCallJ) stmtsJ

explConstrInvToJ (SuperInvoke refs args) = do
  refsJ <- mapM refTypeToJ refs
  argsJ <- mapM expToJ args
  return $ jSuperInvoke refsJ argsJ
explConstrInvToJ other = fail $ (show other) ++ " is not supported by explConstrInvToJ"

methodBodyToJ :: MethodBody -> JError JMethodBody
methodBodyToJ (MethodBody Nothing) = return $ jMethodBody Nothing
methodBodyToJ (MethodBody (Just (Block stmts))) = do
  stmtsJ <- mapM blockStmtToJ stmts
  return $ jBlockMethod $ jBlock stmtsJ

blockStmtToJ :: BlockStmt -> JError JBlockStmt
blockStmtToJ (BlockStmt stmt) = do
  stmtJ <- stmtToJ stmt
  return $ jBlockStmt stmtJ
blockStmtToJ (LocalVars mods tp decls) = do
  modsJ <- mapM modifierToJ mods
  tpJ <- typeToJ tp
  declsJ <- mapM varDeclToJ decls
  return $ jLocalVars modsJ tpJ declsJ
blockStmtToJ other = fail $ (show other) ++ " is not supported by blockStmtToJ"

stmtToJ :: Stmt -> JError JStmt
stmtToJ (Return Nothing) = return jReturnVoid
stmtToJ (Return (Just ex)) = do
  exJ <- expToJ ex
  return $ jReturn exJ
stmtToJ (ExpStmt exp) = do
  expJ <- expToJ exp
  return $ jExpStmt expJ
stmtToJ other = fail $ (show other) ++ " is not supported by stmtToJ"

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
expToJ This = return jThis
expToJ other = fail $ (show other) ++ " is not supported by expToJ"

fieldAccessToJ (PrimaryFieldAccess exp id) = do
  expJ <- expToJ exp
  idJ <- identToJ id
  return $ jPrimaryFieldAccess expJ idJ
fieldAccessToJ other = fail $ (show other) ++ " is not supported by fieldAccessToJ"

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

literalToJ Null = return jNull
literalToJ (Int i) = return $ jInt i
literalToJ (Boolean b) = return $ jBoolean b
literalToJ (Char c) = return $ jChar c
literalToJ (Float f) = return $ jFloat f
literalToJ (Double d) = return $ jDouble d
literalToJ (String s) = return $ jString s
literalToJ other = fail $ (show other) ++ " is not supported by literalToJ"

varDeclToJ (VarDecl vid Nothing) = do
  vidJ <- varDeclIdToJ vid
  return $ jVarDecl vidJ Nothing
varDeclToJ (VarDecl vid (Just vinit)) = do
  vidJ <- varDeclIdToJ vid
  vinitJ <- varInitToJ vinit
  return $ jVarDecl vidJ (Just vinitJ)

varDeclIdToJ (VarId id) = do
  idJ <- identToJ id
  return $ jVarId idJ
varDeclIdToJ other = fail $ (show other) ++ " is not supported by varDeclIdToJ"

varInitToJ (InitExp ex) = do
  exJ <- expToJ ex
  return $ jInitExp exJ

modifierToJ :: Modifier -> JError JModifier
modifierToJ Public = return jPublic
modifierToJ Private = return jPrivate
modifierToJ Protected = return jProtected
modifierToJ Final = return jFinal
modifierToJ Abstract = return jAbstract
modifierToJ Static = return jStatic
modifierToJ (Annotation ann) = do
  annJ <- annotationToJ ann
  return $ jAnnotation annJ
modifierToJ m = fail $ show m ++ " is not a supported modifier"

lhsToJ (NameLhs id) = do
  idJ <- nameToJ id
  return $ jNameLhs idJ
lhsToJ (FieldLhs fa) = do
  faJ <- fieldAccessToJ fa
  return $ jFieldLhs faJ 
lhsToJ other = fail $ (show other) ++ " is not supported by lhsToJ"

nameToJ (Name ids) = do
  idsJ <- mapM identToJ ids
  return $ jName idsJ

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

primTypeToJ BooleanT = return jBooleanT
primTypeToJ ByteT = return jByteT
primTypeToJ ShortT = return jShortT
primTypeToJ IntT = return jIntT
primTypeToJ LongT = return jLongT
primTypeToJ CharT = return jCharT
primTypeToJ FloatT = return jFloatT
primTypeToJ DoubleT = return jDoubleT

typeParamToJ (TypeParam id refs) = do
  idJ <- identToJ id
  refsJ <- mapM refTypeToJ refs
  return $ jTypeParam idJ refsJ

formalParamToJ :: FormalParam -> JError JFormalParam
formalParamToJ (FormalParam mods tp False varDec) = do
  modsJ <- mapM modifierToJ mods
  tpJ <- typeToJ tp
  varDecJ <- varDeclIdToJ varDec
  return $ jFormalParam modsJ tpJ False varDecJ
formalParamToJ other = fail $ (show other) ++ " is not supported by formalParamToJ"

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

superToJ :: Maybe RefType -> JError (Maybe JRefType)
superToJ (Just ref) = do
  refJ <- refTypeToJ ref
  return $ Just refJ
superToJ Nothing = return Nothing

exceptionTypeToJ :: ExceptionType -> JError JExceptionType
exceptionTypeToJ ex = refTypeToJ ex

annotationToJ (MarkerAnnotation n) = do
  nJ <- nameToJ n
  return $ jMarkerAnnotation nJ
annotationToJ other = fail $ (show other) ++ " is not supported by annotationToJ"

assignOpToJ EqualA = return jEqualA
assignOpToJ other = fail $ (show other) ++ " is not supported by assignOpToJ"

identToJ :: Ident -> JError JIdent
identToJ (Ident n) = return $ jIdent n

typeArgumentToJ ta = fail "typeArgumentToJ not implemented"
