module Javalyzer.Syntax(JParseError,
                        JCompilationUnit,
                        jCompUnit,
                        compUnitToJ,
                        jClassTypeDecl,
                        jClassBody,
                        jIdent,
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

data JDecl = JDecl
             deriving (Eq, Ord, Show)

declToJ dec = JDecl

data JModifier = JModifier
                 deriving (Eq, Ord, Show)

modifierToJ m = JModifier

data JIdent = JIdent String
              deriving (Eq, Ord, Show)

jIdent = JIdent

identToJ (Ident n) = jIdent n

data JTypeParam = JTypeParam
                  deriving (Eq, Ord, Show)

jTypeParam = JTypeParam

typeParamToJ tp = jTypeParam

data JRefType = JRefType
                deriving (Eq, Ord, Show)

jRefType = JRefType

refTypeToJ ref = jRefType

superToJ (Just ref) = Just $ refTypeToJ ref
superToJ Nothing = Nothing
