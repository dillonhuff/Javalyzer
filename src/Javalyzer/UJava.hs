module Javalyzer.UJava(
  Class, uClass, className, classMethods,
  ClassHierarchy,
  newClassHierarchy,
  objectClass,
  thisDecl,
  Field, field, formalParamDecl,
  Method, method, instructions, formalParams, fieldDecl,
  Instruction, instrType, asg, fieldType, fieldName, lhs, rhs,
  Lhs, vLhs,
  Exp, fieldAcc, expType, objAccessedName, fieldAccessedName,
  ExpType(..),
  InstrType(..),
  Type, cRef,
  Context) where

data ClassHierarchy = CL
                      deriving (Eq, Ord, Show)

newClassHierarchy = CL

data Class
  = Class String [Field] [Method]
    deriving (Eq, Ord, Show)

className (Class n _ _) = n

classMethods (Class _ _ m) = m

uClass = Class

thisDecl :: Class -> [Instruction]
thisDecl c = []

data Field
  = Field Type String
    deriving (Eq, Ord, Show)

field = Field

formalParamDecl :: Field -> [Instruction]
formalParamDecl f = []

data Method
  = Method (Maybe Type) String [Field] [Instruction]
    deriving (Eq, Ord, Show)

method = Method

instructions (Method _ _ _ is) = is
formalParams (Method _ _ fps _) = fps

data Instruction
  = FieldDecl Type String
  | Assign Lhs Exp
    deriving (Eq, Ord, Show)

fieldDecl = FieldDecl
asg = Assign

data InstrType
  = FIELDDECL
  | ASSIGN
    deriving (Eq, Ord, Show)

instrType :: Instruction -> InstrType
instrType (FieldDecl _ _) = FIELDDECL
instrType (Assign _ _) = ASSIGN

fieldType (FieldDecl t _) = t
fieldName (FieldDecl _ n) = n

lhs (Assign l _) = l
rhs (Assign _ r) = r

data Lhs
  = VarLhs String
    deriving (Eq, Ord, Show)

vLhs str = VarLhs str

data Exp
  = FieldAccess String String
  | Literal
    deriving (Eq, Ord, Show)

fieldAcc = FieldAccess

data ExpType
  = FIELDACCESS
  | LITERAL
    deriving (Eq, Ord, Show)

expType (FieldAccess _ _) = FIELDACCESS
expType Literal = LITERAL

objAccessedName (FieldAccess o _) = o
fieldAccessedName (FieldAccess _ f) = f

data Type
  = Prim PrimType
  | ClassRef String
    deriving (Eq, Ord, Show)

cRef :: String -> Type
cRef = ClassRef

data PrimType
  = IntT
    deriving (Eq, Ord, Show)

objectClass = uClass "Object" [] []

data Context
  = C
    deriving (Eq, Ord, Show)
