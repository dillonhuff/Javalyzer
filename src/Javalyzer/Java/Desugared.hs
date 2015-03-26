module Javalyzer.Java.Desugared(
  DCompilationUnit, dCompilationUnit,
  DPackage, dPackage,
  DImportDecl, dImportDecl,
  DInterfaceDecl,
  DClassDecl, dClassDecl,
  DMethod, dMethod,
  DConstructor, dConstructor,
  DConstructorBody, dConstructorBody,
  DStmt, dVarDeclSt, dLocalVarDecl, dReturn, dExpSt,
  DExp, dLit, dPrimaryFieldAccess, dThis, dExpName,
  DName, dName,
  DLiteral, dChar,
  DVarDecl, dVarDecl,
  DFormalParam, dFormalParam,
  DVarIdent, dVarIdent,
  DTypeParam, dTypeParam, dTypeParamName,
  DType, dRefType, dPrimType,
  DRefType, dClassRefType,
  DPrimType, dIntT,
  DClassType, dClassType,
  DTypeId, dTypeVar, dClassName,
  DTypeArg, dActualType,
  Modifiers, noMods, privateMods, mods,
  Access, public, private, protected,
  ImplLevel, abstract, final, realExtendable,
  nonStatic, static,
  firstBlock,
  PackageNames, buildPackages, package) where

import Data.List as L

import Javalyzer.Utils

data DCompilationUnit
  = DCompilationUnit (Maybe DPackage) [DImportDecl] [DInterfaceDecl] [DClassDecl]
    deriving (Eq, Ord, Show)

dCompilationUnit = DCompilationUnit

firstBlock (DCompilationUnit _ _ _ classes) =
  methodBody $ firstMethod $ head classes

extractNames :: DCompilationUnit -> ([String], [String])
extractNames (DCompilationUnit (Just pkg) _ _ classes) =
  (dPackageName pkg, L.map name classes)

data DPackage
  = DPackage [String]
    deriving (Eq, Ord, Show)

dPackage = DPackage
dPackageName (DPackage n) = n

data DImportDecl = DImportDecl Bool Bool [String]
                   deriving (Eq, Ord, Show)

dImportDecl = DImportDecl

data DInterfaceDecl = DIL
                      deriving (Eq, Ord, Show)

data DClassDecl
  = DClassDecl {
    name :: String,
    typeParams :: [DTypeParam],
    super :: DRefType,
    fields :: [DVarDecl],
    methods :: [DMethod],
    constructors :: [DConstructor] }
  deriving (Eq, Ord, Show)

dClassDecl :: String ->
              [DTypeParam] ->
              Maybe DRefType ->
              [DVarDecl] ->
              [DMethod] ->
              [DConstructor] ->
              DClassDecl
dClassDecl name tps superClass fields methods constructors =
  case superClass of
    Nothing -> DClassDecl name tps dObjectRef fields methods constructors
    Just s -> DClassDecl name tps s fields methods constructors

firstMethod (DClassDecl _ _ _ _ meths _) = head meths

data DInstField
  = DInstField DType DVarIdent
    deriving (Eq, Ord, Show)

data DMethod
  = DMethod Modifiers [DTypeParam] (Maybe DType) String [DVarDecl] [DException] [DStmt]
    deriving (Eq, Ord, Show)

dMethod = DMethod

methodBody (DMethod _ _ _ _ _ _ body) = body

data DConstructor
  = DConstructor Modifiers [DTypeParam] String [DVarDecl] [DException] DConstructorBody
    deriving (Eq, Ord, Show)

dConstructor = DConstructor

data DConstructorBody
  = DConstructorBody (Maybe DStmt) [DStmt]
    deriving (Eq, Ord, Show)

dConstructorBody = DConstructorBody

data DStmt
  = DVarDeclSt DVarDecl
  | DReturn (Maybe DExp)
  | DExpSt DExp
    deriving (Eq, Ord, Show)

dVarDeclSt = DVarDeclSt
dReturn = DReturn
dExpSt = DExpSt


dLocalVarDecl mods tp varName = dVarDeclSt $ dVarDecl mods tp varName

data DExp
  = DLit DLiteral
  | DPrimaryFieldAccess DExp DVarIdent
  | DExpName DName
  | DThis
    deriving (Eq, Ord, Show)

dLit = DLit
dPrimaryFieldAccess = DPrimaryFieldAccess
dThis = DThis
dExpName = DExpName

data DName
  = DName [DVarIdent]
    deriving (Eq, Ord, Show)

dName = DName

data DLiteral
  = DChar Char
    deriving (Eq, Ord, Show)

dChar = DChar

data DVarDecl = DVarDecl Modifiers DType DVarIdent
                deriving (Eq, Ord, Show)

dVarDecl = DVarDecl

data DFormalParam
  = DFormalParam Modifiers DType DVarIdent
    deriving (Eq, Ord, Show)

dFormalParam = DFormalParam

data DVarIdent
  = DIdent String
    deriving (Eq, Ord, Show)

dVarIdent = DIdent

data DType
  = DPrimType DPrimType
  | DRefType DRefType
    deriving (Eq, Ord, Show)

dRefType = DRefType
dPrimType = DPrimType

data DTypeParam
  = DTypeParam String [DRefType]
    deriving (Eq, Ord, Show)

dTypeParam = DTypeParam
dTypeParamName (DTypeParam n _) = n

data DRefType
  = DClassRefType DClassType
    deriving (Eq, Ord, Show)

dClassRefType = DClassRefType

dObjectRef = dClassRefType $ dClassType [(dClassName "Object", [])]

data DPrimType
  = DIntT
    deriving (Eq, Ord, Show)

dIntT = DIntT

data DClassType
  = DClassType [(DTypeId, [DTypeArg])]
    deriving (Eq, Ord, Show)

dClassType = DClassType

data DTypeId
  = DClassName (Maybe DPackage) String
  | DTypeVar String
    deriving (Eq, Ord, Show)

dTypeVar = DTypeVar
dClassName str = DClassName Nothing str
dClassNameWithPackage pkg str = DClassName (Just $ dPackage pkg) str

data DTypeArg
  = DActualType DRefType
    deriving (Eq, Ord, Show)

dActualType = DActualType

type IsStatic = Bool

static = True
nonStatic = False

data Modifiers
  = Modifiers Access ImplLevel IsStatic
    deriving (Eq, Ord, Show)

mods :: Access -> ImplLevel -> IsStatic -> Modifiers
mods = Modifiers

noMods = Modifiers public realExtendable False
privateMods = Modifiers private realExtendable False

data Access
  = Public
  | Private
  | Protected
    deriving (Eq, Ord, Show)

private = Private
protected = Protected
public = Public

data ImplLevel
  = Abstract
  | Final
  | RealExtendable
    deriving (Eq, Ord, Show)

abstract = Abstract
final = Final
realExtendable = RealExtendable

type DException = DRefType

data PackageNames = PackageNames {
  packageName :: DPackage,
  classNames :: [DTypeId]
  } deriving (Eq, Ord, Show)

package :: [String] -> [String] -> JError PackageNames
package pkgName classes =
  let pkg = dPackage pkgName
      classesWithPackageNames = L.map (dClassNameWithPackage pkgName) classes in
  return $ PackageNames (dPackage pkgName) classesWithPackageNames

buildPackages :: [DCompilationUnit] -> JError [PackageNames]
buildPackages compUnits =
  let pkgClassNamesTuples = L.map extractNames compUnits
      pkgNameAllClasses = groupByFst pkgClassNamesTuples in
  mapM (\(pkgName, classNames) -> package pkgName (L.concat classNames)) pkgNameAllClasses

groupByFst :: Eq a => [(a, b)] -> [(a, [b])]
groupByFst [] = []
