module Javalyzer.Symtab(Symtab,
                        emptySymtab,
                        addVar,
                        addDeclToSymtab,
                        userDefined,
                        localS,
                        instanceS,
                        systemDefined) where

import Data.List as L
import Data.Map as M

import Javalyzer.Syntax
import Javalyzer.Utils

data Symtab = Symtab [Map String STEntry]
              deriving (Eq, Ord, Show)

emptySymtab = Symtab [M.empty]

data OriginInfo
  = UserDefined
  | SystemDefined
    deriving (Eq, Ord, Show)

userDefined = UserDefined
systemDefined = SystemDefined

data Scope
  = Instance
  | Local
    deriving (Eq, Ord, Show)

localS = Local
instanceS = Instance

data STEntry
  = VarEntry [JModifier] Scope JType OriginInfo
    deriving (Eq, Ord, Show)

varSTEntry :: [JModifier] -> Scope -> JType -> OriginInfo -> STEntry
varSTEntry mods scp tp or = VarEntry mods scp tp or

varPrefix = "^VAR-"

addVar :: JIdent -> [JModifier] -> Scope -> JType -> OriginInfo -> Symtab -> Symtab
addVar id mods scp tp or (Symtab levels) =
  let newEnt = varSTEntry mods scp tp or
      topLev = head levels
      symName = varPrefix ++ (jIdentName id) in
  Symtab ((M.insert symName newEnt topLev) : (tail levels))

addDeclToSymtab :: JMemberDecl -> Symtab -> JError Symtab
addDeclToSymtab decl st =
  case memberDeclType decl of
    FIELD -> addInstanceFields (fieldVarDecls decl) (fieldMods decl) (fieldType decl) st
    _ -> fail $ (show decl) ++ " is not yet supported by addDeclToSymtab"

addInstanceFields :: [JVarDecl] -> [JModifier] -> JType -> Symtab -> JError Symtab
addInstanceFields [] _ _ st = return st
addInstanceFields (vd:vds) mods tp st =
  let vDeclId = getVarDeclId vd in
  case varDeclIdType vDeclId of
       VARID -> let newSt = addVar (getVarIdent vDeclId) mods instanceS tp userDefined st in
         addInstanceFields vds mods tp newSt
