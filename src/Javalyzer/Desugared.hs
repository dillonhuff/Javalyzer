module Javalyzer.Desugared(
  DCompilationUnit,
  dCompilationUnit,
  DClassDecl,
  dClassDecl,
  DStmt,
  DVarIdent,
  dVarIdent,
  DTypeParam,
  dTypeParam,
  dTypeParamName,
  DRefType,
  dClassRefType,
  DClassType,
  dClassType,
  DTypeId,
  dTypeVar,
  dClassName,
  DTypeArg,
  dActualType) where

data DCompilationUnit
  = DCompilationUnit (Maybe DPackage) [DImport] [DInterfaceDecl] [DClassDecl]
    deriving (Eq, Ord, Show)

dCompilationUnit = DCompilationUnit

data DPackage = DPackage [String]
                deriving (Eq, Ord, Show)

data DImport = DIP
               deriving (Eq, Ord, Show)

data DInterfaceDecl = DIL
                      deriving (Eq, Ord, Show)

data DClassDecl
  = DClassDecl {
    name :: String,
    typeParams :: [DTypeParam],
    super :: DRefType,
    fields :: [DInstField],
    methods :: [DMethod],
    constructors :: [DConstructor] }
  deriving (Eq, Ord, Show)

dClassDecl :: String ->
              [DTypeParam] ->
              Maybe DRefType ->
              [DInstField] ->
              [DMethod] ->
              [DConstructor] ->
              DClassDecl
dClassDecl name tps superClass fields methods constructors =
  case superClass of
    Nothing -> DClassDecl name tps dObjectRef fields methods constructors
    Just s -> DClassDecl name tps s fields methods constructors

data DInstField
  = DInstField DType DVarIdent
    deriving (Eq, Ord, Show)

data DMethod
  = DMethod [DTypeParam] (Maybe DType) String [DFormalParam] DBlock
    deriving (Eq, Ord, Show)

data DConstructor
  = DC
    deriving (Eq, Ord, Show)

data DBlock = DBlock
              deriving (Eq, Ord, Show)

data DStmt = DLocalVarDecl DType DVarIdent
             deriving (Eq, Ord, Show)

data DFormalParam
  = DFormalParam DType DVarIdent
    deriving (Eq, Ord, Show)

data DVarIdent
  = DIdent String
    deriving (Eq, Ord, Show)

dVarIdent = DIdent

data DType
  = DPrimType
  | DRefType DRefType
    deriving (Eq, Ord, Show)

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
