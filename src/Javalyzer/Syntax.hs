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

import Data.List as L
import Language.Java.Syntax

data JParseError = JParseError String
                   deriving (Eq, Ord, Show)

jParseError = JParseError

data JCompilationUnit = JCompilationUnit (Maybe JPackageDecl) [JImportDecl] [JTypeDecl]
                        deriving (Eq, Ord, Show)

jCompUnit = JCompilationUnit

compUnitToJ (CompilationUnit packDecl imports typeDecls) =
  jCompUnit (packageDeclToJ packDecl) (L.map importToJ imports) (L.map typeDeclToJ typeDecls)

data JPackageDecl = JPD
                    deriving (Eq, Ord, Show)

packageDeclToJ (Just p) = Just $ JPD
packageDeclToJ Nothing = Nothing

data JImportDecl = JID
                   deriving (Eq, Ord, Show)

importToJ im = JID

data JTypeDecl = JClassTypeDecl JClassDecl
                 deriving (Eq, Ord, Show)

jClassTypeDecl mods name typeParams super refTypes body =
  JClassTypeDecl $ jClassDecl mods name typeParams super refTypes body

typeDeclToJ (ClassTypeDecl (ClassDecl mods id tps sup refs body)) =
  jClassTypeDecl (L.map modifierToJ mods) (identToJ id) (L.map typeParamToJ tps)(superToJ sup) (L.map refTypeToJ refs) (classBodyToJ body)

data JClassDecl = JClassDecl [JModifier] JIdent [JTypeParam] (Maybe JRefType) [JRefType] JClassBody
                  deriving (Eq, Ord, Show)

jClassDecl = JClassDecl

data JClassBody = JClassBody [JDecl]
                  deriving (Eq, Ord, Show)

jClassBody = JClassBody

classBodyToJ (ClassBody decls) = jClassBody $ L.map declToJ decls

data JDecl = JMemberDecl JMemberDecl
             deriving (Eq, Ord, Show)

jMemberDecl = JMemberDecl

declToJ (MemberDecl (MethodDecl mods tps retType id fparams exceptions body)) =
  jMemberDecl $ jMethodDecl modsJ tpsJ retTypeJ idJ fparamsJ exceptionsJ bodyJ
  where
    modsJ = L.map modifierToJ mods
    tpsJ = L.map typeParamToJ tps
    retTypeJ = returnTypeToJ retType
    idJ = identToJ id
    fparamsJ = L.map formalParamToJ fparams
    exceptionsJ = L.map exceptionTypeToJ exceptions
    bodyJ = methodBodyToJ body

data JMemberDecl
  = JMethodDecl [JModifier] [JTypeParam] (Maybe JType) JIdent [JFormalParam] [JExceptionType] JMethodBody
    deriving (Eq, Ord, Show)

jMethodDecl = JMethodDecl

data JMethodBody = JMethodBody (Maybe JBlock)
                   deriving (Eq, Ord, Show)

jBlockMethod block = JMethodBody $ Just block

methodBodyToJ (MethodBody Nothing) = JMethodBody Nothing
methodBodyToJ (MethodBody (Just (Block stmts))) =
  jBlockMethod $ jBlock $ L.map blockStmtToJ stmts

data JBlock = JBlock [JBlockStmt]
              deriving (Eq, Ord, Show)

jBlock stmts = JBlock stmts

data JBlockStmt = JBlockStmt JStmt
                   deriving (Eq, Ord, Show)

jBlockStmt stmt = JBlockStmt stmt

blockStmtToJ (BlockStmt stmt) = JBlockStmt $ stmtToJ stmt

data JStmt = JReturn (Maybe JExp)
             deriving (Eq, Ord, Show)

jReturnVoid = JReturn Nothing

stmtToJ (Return Nothing) = jReturnVoid

data JExp = JExp
            deriving (Eq, Ord, Show)

data JModifier = JPublic
                 deriving (Eq, Ord, Show)

jPublic = JPublic

modifierToJ m = jPublic

data JIdent = JIdent String
              deriving (Eq, Ord, Show)

jIdent = JIdent

identToJ (Ident n) = jIdent n

data JType
  = JPrimType
  | JRefType
    deriving (Eq, Ord, Show)

returnTypeToJ Nothing = Nothing

data JTypeParam = JTypeParam
                  deriving (Eq, Ord, Show)

jTypeParam = JTypeParam

typeParamToJ tp = jTypeParam

data JFormalParam = JFP
                    deriving (Eq, Ord, Show)

formalParamToJ fp = JFP

data JRefType = JClassRefType JClassType
                deriving (Eq, Ord, Show)

jRefType = JClassRefType
jClassRefType = JClassRefType

refTypeToJ (ClassRefType (ClassType idTypeArgList)) =
  jClassRefType $ jClassType $ idTypeArgListToJ idTypeArgList

idTypeArgListToJ :: [(Ident, [TypeArgument])] -> [(JIdent, [JTypeArgument])]
idTypeArgListToJ ls = L.map (\(id, tArgs) -> (identToJ id, L.map typeArgumentToJ tArgs)) ls

superToJ (Just ref) = Just $ refTypeToJ ref
superToJ Nothing = Nothing

data JClassType = JClassType [(JIdent, [JTypeArgument])]
                  deriving (Eq, Ord, Show)

jClassType = JClassType

data JTypeArgument = JActualType
                     deriving (Eq, Ord, Show)

typeArgumentToJ ta = JActualType

type JExceptionType = JRefType

exceptionTypeToJ ex = refTypeToJ ex
