module Javalyzer.Syntax(JParseError,
                        JCompilationUnit,
                        compUnitToJ,
                        jCompUnit,
                        jClassTypeDecl,
                        jClassBody,
                        jMemberDecl,
                        jMethodDecl,
                        jBlockMethod,
                        jBlockStmt,
                        jBlock,
                        jReturnVoid,
                        jClassRefType,
                        jClassType,
                        jIdent,
                        jPublic,
                        jParseError) where

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

data JPackageDecl = JPD
                    deriving (Eq, Ord, Show)

maybePackageDeclToJ :: Maybe PackageDecl -> JError (Maybe JPackageDecl)
maybePackageDeclToJ Nothing = return Nothing
maybePackageDeclToJ (Just dec) =
  let pkg = packageDeclToJ dec in
  case pkg of
    (JSuccess p) -> JSuccess (Just p)
    (JFail str) -> (JFail str)
  
packageDeclToJ :: PackageDecl -> JError JPackageDecl
packageDeclToJ p = fail "packageDeclToJ not implemented"

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
  return $ jClassTypeDecl modsJ idJ tpsJ supJ refsJ bodyJ --(L.map modifierToJ mods) (identToJ id) (L.map typeParamToJ tps)(superToJ sup) (L.map refTypeToJ refs) (classBodyToJ body)

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

data JMemberDecl
  = JMethodDecl [JModifier] [JTypeParam] (Maybe JType) JIdent [JFormalParam] [JExceptionType] JMethodBody
    deriving (Eq, Ord, Show)

jMethodDecl = JMethodDecl

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

data JBlockStmt = JBlockStmt JStmt
                   deriving (Eq, Ord, Show)

jBlockStmt stmt = JBlockStmt stmt

blockStmtToJ :: BlockStmt -> JError JBlockStmt
blockStmtToJ (BlockStmt stmt) = do
  stmtJ <- stmtToJ stmt
  return $ JBlockStmt stmtJ

data JStmt = JReturn (Maybe JExp)
             deriving (Eq, Ord, Show)

jReturnVoid = JReturn Nothing

stmtToJ :: Stmt -> JError JStmt
stmtToJ (Return Nothing) = return jReturnVoid

data JExp = JExp
            deriving (Eq, Ord, Show)

data JModifier = JPublic
                 deriving (Eq, Ord, Show)

jPublic = JPublic

modifierToJ :: Modifier -> JError JModifier
modifierToJ Public = return JPublic
modifierToJ m = fail $ show m ++ " is not a supported modifier"

data JIdent = JIdent String
              deriving (Eq, Ord, Show)

jIdent = JIdent

identToJ :: Ident -> JError JIdent
identToJ (Ident n) = return $ jIdent n

data JType
  = JPrimType
  | JRefType
    deriving (Eq, Ord, Show)

returnTypeToJ Nothing = return $ Nothing

data JTypeParam = JTypeParam
                  deriving (Eq, Ord, Show)

jTypeParam = JTypeParam

typeParamToJ tp = fail "typeParamToJ not implemented"

data JFormalParam = JFP
                    deriving (Eq, Ord, Show)

formalParamToJ :: FormalParam -> JError JFormalParam
formalParamToJ fp = return JFP

data JRefType = JClassRefType JClassType
                deriving (Eq, Ord, Show)

jRefType = JClassRefType
jClassRefType = JClassRefType

refTypeToJ :: RefType -> JError JRefType
refTypeToJ (ClassRefType (ClassType idTypeArgList)) = do
  idTypeArgListJ <- idTypeArgListToJ idTypeArgList
  return $ jClassRefType $ jClassType idTypeArgListJ
refTypeToJ other = fail $ "refTypeToJ not implemented for " ++ show other

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
