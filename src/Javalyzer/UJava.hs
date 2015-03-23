module Javalyzer.UJava(
  Class, uClass, className, classMethods,
  ClassHierarchy, newClassHierarchy, getClassFields,
  objectClass, thisDecl,
  Field, field, formalParamDecl, fieldType, fieldName,
  Method, method, instructions, formalParams,
  Instruction, instrType, asg, lhs, rhs, getFieldDeclFromInstr, fieldDeclInstr,
  Lhs, vLhs, fLhs, lhsType, getFieldAccFromLhs, getNameFromLhs,
  LhsType(..), 
  Exp, expType, intLit, newInst, fieldAccExp, getFieldAccFromExp, getClassNameFromExp, 
  ExpType(..),
  FieldAccess, objAccessedName, fieldAccessedName,
  InstrType(..),
  Type, cRef, primInt, isRef) where

import Control.Monad
import Data.List as L

import Javalyzer.Utils

data ClassHierarchy = CL [(String, Class)]
                      deriving (Eq, Ord, Show)

newClassHierarchy classes = CL $ L.zip (L.map className classes) classes

getClassFields :: String -> ClassHierarchy -> JError [Field]
getClassFields className h = do
  c <- lookupClass className h
  case superClassName c of
    Just super -> liftM (\fList -> (classFields c) ++ fList) (getClassFields super h)
    Nothing -> return $ classFields c

lookupClass :: String -> ClassHierarchy -> JError Class
lookupClass className h@(CL classes) =
  case L.lookup className classes of
    Just c -> success c
    Nothing -> fail $ "Could not find class of name " ++ className ++ " in " ++ (show h)

data Class
  = Class String (Maybe String) [Field] [Method]
    deriving (Eq, Ord, Show)

className (Class n _ _ _) = n
classMethods (Class _ _ _ ms) = ms
classFields (Class _ _ fs _) = fs
superClassName (Class _ superN _ _) = superN

uClass name fs ms = Class name (Just "Object") fs ms

objectClass = Class "Object" Nothing [] []

thisDecl :: Class -> [Instruction]
thisDecl c = []

data Field
  = Field Type String
    deriving (Eq, Ord, Show)

field = Field

fieldType (Field t _) = t
fieldName (Field _ n) = n

formalParamDecl :: Field -> [Instruction]
formalParamDecl f = []

data Method
  = Method (Maybe Type) String [Field] [Instruction]
    deriving (Eq, Ord, Show)

method = Method

instructions (Method _ _ _ is) = is
formalParams (Method _ _ fps _) = fps

data Instruction
  = FD Field
  | Assign Lhs Exp
    deriving (Eq, Ord, Show)

fieldDeclInstr t n = FD  $ field t n
asg = Assign

getFieldDeclFromInstr (FD f) = f

data InstrType
  = FIELDDECL
  | ASSIGN
    deriving (Eq, Ord, Show)

instrType :: Instruction -> InstrType
instrType (FD _) = FIELDDECL
instrType (Assign _ _) = ASSIGN

lhs (Assign l _) = l
rhs (Assign _ r) = r

data Lhs
  = VarLhs String
  | FA FieldAccess
    deriving (Eq, Ord, Show)

vLhs str = VarLhs str
fLhs objName fieldName = FA $ fieldAccess objName fieldName

getFieldAccFromLhs (FA acc) = acc
getNameFromLhs (VarLhs n) = n

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

