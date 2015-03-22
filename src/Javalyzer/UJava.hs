module Javalyzer.UJava(
  Class, uClass, className, classMethods,
  ClassHierarchy,
  newClassHierarchy,
  objectClass,
  thisDecl,
  Field, field, formalParamDecl,
  Method, method, instructions, formalParams, fieldDecl,
  Instruction, instrType, asg, fieldType, fieldName, lhs, rhs,
  Lhs, vLhs, fLhs, lhsType, getFieldAccFromLhs,
  LhsType(..), 
  Exp, expType, intLit, newInst, fieldAccExp, getFieldAccFromExp, getClassNameFromExp,
  ExpType(..),
  FieldAccess, objAccessedName, fieldAccessedName,
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
  | FA FieldAccess
    deriving (Eq, Ord, Show)

vLhs str = VarLhs str
fLhs objName fieldName = FA $ fieldAccess objName fieldName

getFieldAccFromLhs (FA acc) = acc

lhsType (VarLhs _) = LVAR
lhsType (FA _) = LFIELDACCESS

data LhsType
  = LFIELDACCESS
  | LVAR
    deriving (Eq, Ord, Show)

data Exp
  = EFieldAcc FieldAccess
  | Literal Lit
  | NewInst String
    deriving (Eq, Ord, Show)

fieldAccExp objName fieldName = EFieldAcc $ fieldAccess objName fieldName
lit = Literal
intLit = lit . int
newInst = NewInst

getFieldAccFromExp (EFieldAcc acc) = acc
getClassNameFromExp (NewInst n) = n

data ExpType
  = FIELDACCESS
  | LITERAL
  | NEWINST
    deriving (Eq, Ord, Show)

expType (EFieldAcc _) = FIELDACCESS
expType (Literal _) = LITERAL
expType (NewInst _) = NEWINST

data FieldAccess
  = FieldAccess String String
    deriving (Eq, Ord, Show)

fieldAccess = FieldAccess

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
