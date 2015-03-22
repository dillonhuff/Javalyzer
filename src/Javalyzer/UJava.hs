module Javalyzer.UJava(
  Class, uClass, className, classMethods,
  ClassHierarchy,
  newClassHierarchy,
  objectClass,
  thisDecl,
  Field, field, formalParamDecl,
  Method, method, instructions, formalParams, fieldDecl,
  Instruction, instrType, asg, fieldType, fieldName, lhs, rhs,
  Lhs, vLhs, fLhs,
  Exp, fieldAcc, expType, objAccessedName, fieldAccessedName, intLit, newInst,
  ExpType(..),
  InstrType(..),
  Type, cRef, primInt, isRef,
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
  | FieldAcc String String
    deriving (Eq, Ord, Show)

vLhs str = VarLhs str
fLhs objName fieldName = FieldAcc objName fieldName

data Exp
  = FieldAccess String String
  | Literal Lit
  | NewInst String
    deriving (Eq, Ord, Show)

fieldAcc = FieldAccess
lit = Literal
intLit = lit . int
newInst = NewInst

data ExpType
  = FIELDACCESS
  | LITERAL
  | NEWINST
    deriving (Eq, Ord, Show)

expType (FieldAccess _ _) = FIELDACCESS
expType (Literal _) = LITERAL
expType (NewInst _) = NEWINST

objAccessedName (FieldAccess o _) = o
fieldAccessedName (FieldAccess _ f) = f

data Lit
  = Int Integer
    deriving (Eq, Ord, Show)

int = Int

data Type
  = Prim PrimType
  | ClassRef String
    deriving (Eq, Ord, Show)

prim = Prim
primInt = prim intT
cRef :: String -> Type
cRef = ClassRef

isRef (ClassRef _) = True
isRef _ = False

data PrimType
  = IntT
    deriving (Eq, Ord, Show)

intT = IntT
objectClass = uClass "Object" [] []

data Context
  = C
    deriving (Eq, Ord, Show)
