module Javalyzer.Desugared(
  DCompilationUnit, dCompilationUnit,
  DPackage, dPackage,
  DImportDecl, dImportDecl,
  DInterfaceDecl,
  DClassDecl, dClassDecl,
  DMethod, dMethod,
  DConstructor,
  DStmt, dVarDeclSt, dLocalVarDecl, dReturn, dExpSt,
  DExp, dLit, dPrimaryFieldAccess, dThis,
  DLiteral, dChar,
  DVarDecl, dVarDecl,
  DFormalParam,
  DVarIdent, dVarIdent,
  DTypeParam, dTypeParam, dTypeParamName,
  DType, dRefType, dPrimType,
  DRefType, dClassRefType,
  DPrimType, dIntT,
  DClassType, dClassType,
  DTypeId, dTypeVar, dClassName,
  DTypeArg, dActualType,
  Modifiers, noMods,
  firstBlock) where

data DCompilationUnit
  = DCompilationUnit (Maybe DPackage) [DImportDecl] [DInterfaceDecl] [DClassDecl]
    deriving (Eq, Ord, Show)

dCompilationUnit = DCompilationUnit

firstBlock (DCompilationUnit _ _ _ classes) =
  methodBody $ firstMethod $ head classes

data DPackage
  = DPackage [String]
    deriving (Eq, Ord, Show)

dPackage = DPackage

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
  = DMethod Modifiers [DTypeParam] (Maybe DType) String [DFormalParam] [DException] [DStmt]
    deriving (Eq, Ord, Show)

dMethod = DMethod

methodBody (DMethod _ _ _ _ _ _ body) = body

data DConstructor
  = DC
    deriving (Eq, Ord, Show)

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
  | DThis
    deriving (Eq, Ord, Show)

dLit = DLit
dPrimaryFieldAccess = DPrimaryFieldAccess
dThis = DThis

data DLiteral
  = DChar Char
    deriving (Eq, Ord, Show)

dChar = DChar

data DVarDecl = DVarDecl Modifiers DType DVarIdent
                deriving (Eq, Ord, Show)

dVarDecl = DVarDecl

data DFormalParam
  = DFormalParam DType DVarIdent
    deriving (Eq, Ord, Show)

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
  = DClassName String
  | DTypeVar String
    deriving (Eq, Ord, Show)

dTypeVar = DTypeVar
dClassName = DClassName

data DTypeArg
  = DActualType DRefType
    deriving (Eq, Ord, Show)

dActualType = DActualType

data Modifiers
  = Mods
    deriving (Eq, Ord, Show)

noMods = Mods

type DException = DRefType
