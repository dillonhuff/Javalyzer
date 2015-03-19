module Javalyzer.Syntax(JParseError,
                        JCompilationUnit,
                        jCompUnit,
                        package,
                        imports,
                        decls,
                        classDecls,
                        isSingleClass,
                        JTypeDecl,
                        JClassDecl,
                        JMethodBody,
                        jClassDecl,
                        classBody,
                        className,
                        JPackageDecl,
                        jPackageDecl,
                        JImportDecl,
                        jClassTypeDecl,
                        jClassBody,
                        bodyDecls,
                        JMemberDecl,
                        JDeclType(..),
                        getMember,
                        declType,
                        JMemberDeclType(..),
                        memberDeclType,
                        fieldMods,
                        fieldType,
                        fieldVarDecls,
                        JDecl,
                        jMemberDecl,
                        jMethodDecl,
                        jFieldDecl,
                        jConstructorDecl,
                        jConstructorBody,
                        jSuperInvoke,
                        jBlockMethod,
                        jBlockStmt,
                        jMethodInv,
                        jLocalVars,
                        jBlock,
                        jReturnVoid,
                        jReturn,
                        jExpStmt,
                        jInitExp,
                        jPrimType,
                        jClassRefType,
                        jClassType,
                        jAssign,
                        jFieldAccess,
                        jThis,
                        jLit,
                        jPrimaryFieldAccess,
                        jExpName,
                        jEqualA,
                        JType,
                        JRefType,
                        jRefType,
                        JVarDecl,
                        JVarDeclIdType(..),
                        varDeclIdType,
                        getVarIdent,
                        getVarDeclId,
                        jVarId,
                        jVarDecl,
                        jPrimaryMethodCall,
                        jMethodCall,
                        jTypeParam,
                        jFormalParam,
                        jNameLhs,
                        jFieldLhs,
                        jNull,
                        jBoolean,
                        jWord,
                        jInt,
                        jChar,
                        jFloat,
                        jDouble,
                        jString,
                        jBooleanT,
                        jByteT,
                        jShortT,
                        jIntT,
                        jLongT,
                        jCharT,
                        jFloatT,
                        jDoubleT,
                        JIdent,
                        jIdentName,
                        jIdent,
                        jName,
                        JModifier,
                        jPublic,
                        jPrivate,
                        jFinal,
                        jAbstract,
                        jStatic,
                        jProtected,
                        jAnnotation,
                        jParseError,
                        jMarkerAnnotation,
                        JTypeArgument,
                        JExceptionType,
                        JFormalParam,
                        JStmt,
                        JAnnotation,
                        jId,
                        JBlockStmt,
                        jMethodBody,
                        dsCompilationUnit,
                        dsBlockStmt,
                        dsVarIdent,
                        dsTypeParam) where

import Control.Monad
import Data.List as L
import Data.Set as S
import Language.Java.Syntax

import Javalyzer.Desugared
import Javalyzer.Utils

data JParseError = JParseError String
                   deriving (Eq, Ord, Show)

jParseError = JParseError

data JCompilationUnit = JCompilationUnit (Maybe JPackageDecl) [JImportDecl] [JTypeDecl]
                        deriving (Eq, Ord, Show)

jCompUnit = JCompilationUnit

package (JCompilationUnit pkg _ _) = pkg
imports (JCompilationUnit _ imps _) = imps
decls (JCompilationUnit _ _ ds) = ds
classDecls jcu = L.map getClassDecl $ L.filter isClassDecl $ decls jcu

isSingleClass (JCompilationUnit _ _ decls) =
  case length decls == 1 of
    True -> case decls of
      ((JClassTypeDecl classDec):nil) -> True
      _ -> False
    False -> False


data JPackageDecl = JPackageDecl JName
                    deriving (Eq, Ord, Show)

jPackageDecl = JPackageDecl

data JImportDecl = JID
                   deriving (Eq, Ord, Show)

jId = JID

data JTypeDecl = JClassTypeDecl JClassDecl
                 deriving (Eq, Ord, Show)

jClassTypeDecl mods name typeParams super refTypes body =
  JClassTypeDecl $ jClassDecl mods name typeParams super refTypes body

isClassDecl (JClassTypeDecl _) = True

getClassDecl (JClassTypeDecl d) = d

data JClassDecl = JClassDecl [JModifier] JIdent [JTypeParam] (Maybe JRefType) [JRefType] JClassBody
                  deriving (Eq, Ord, Show)

jClassDecl = JClassDecl
className (JClassDecl _ id _ _ _ _) = jIdentName id
classBody (JClassDecl _ _ _ _ _ b) = b

data JClassBody = JClassBody [JDecl]
                  deriving (Eq, Ord, Show)

jClassBody = JClassBody

bodyDecls (JClassBody ds) = ds

data JDecl = JMemberDecl JMemberDecl
             deriving (Eq, Ord, Show)

data JDeclType
  = MEMBER
    deriving (Eq, Ord, Show)

getMember (JMemberDecl m) = m
             
declType (JMemberDecl _) = MEMBER

jMemberDecl = JMemberDecl


data JMemberDecl
  = JMethodDecl [JModifier] [JTypeParam] (Maybe JType) JIdent [JFormalParam] [JExceptionType] JMethodBody
  | JFieldDecl [JModifier] JType [JVarDecl]
  | JConstructorDecl [JModifier] [JTypeParam] JIdent [JFormalParam] [JExceptionType] JConstructorBody
    deriving (Eq, Ord, Show)

data JMemberDeclType
  = FIELD
  | METHOD
  | CONSTRUCTOR
    deriving (Eq, Ord, Show)

memberDeclType (JMethodDecl _ _ _ _ _ _ _) = METHOD
memberDeclType (JFieldDecl _ _ _) = FIELD
memberDeclType (JConstructorDecl _ _ _ _ _ _) = CONSTRUCTOR

jMethodDecl = JMethodDecl
jFieldDecl = JFieldDecl
jConstructorDecl = JConstructorDecl

fieldMods (JFieldDecl mods _ _) = mods
fieldType (JFieldDecl _ tp _) = tp
fieldVarDecls (JFieldDecl _ _ vDecls) = vDecls

data JConstructorBody = JConstructorBody (Maybe JExplConstrInv) [JBlockStmt]
                        deriving (Eq, Ord, Show)

jConstructorBody = JConstructorBody

data JExplConstrInv
  = JSuperInvoke [JRefType] [JArgument]
    deriving (Eq, Ord, Show)

jSuperInvoke = JSuperInvoke

data JMethodBody = JMethodBody (Maybe JBlock)
                   deriving (Eq, Ord, Show)

jMethodBody m = JMethodBody m
jBlockMethod block = JMethodBody $ Just block

data JBlock = JBlock [JBlockStmt]
              deriving (Eq, Ord, Show)

jBlock stmts = JBlock stmts

data JBlockStmt
  = JBlockStmt JStmt
  | JLocalVars [JModifier] JType [JVarDecl]
    deriving (Eq, Ord, Show)

jBlockStmt = JBlockStmt
jLocalVars = JLocalVars

data JStmt
  = JReturn (Maybe JExp)
  | JExpStmt JExp
    deriving (Eq, Ord, Show)

jReturnVoid = JReturn Nothing
jReturn exp = JReturn $ Just exp

jExpStmt = JExpStmt

data JExp
  = JLit JLiteral
  | JAssign JLhs JAssignOp JExp
  | JMethodInv JMethodInvocation
  | JExpName JName
  | JFieldAccess JFieldAccess
  | JThis
    deriving (Eq, Ord, Show)

jLit = JLit
jAssign = JAssign
jMethodInv = JMethodInv
jExpName = JExpName
jFieldAccess = JFieldAccess
jThis = JThis

type JArgument = JExp

data JFieldAccess
  = JPrimaryFieldAccess JExp JIdent
    deriving (Eq, Ord, Show)

jPrimaryFieldAccess = JPrimaryFieldAccess

data JMethodInvocation
  = JPrimaryMethodCall JExp [JRefType] JIdent [JArgument]
  | JMethodCall JName [JArgument]
    deriving (Eq, Ord, Show)

jPrimaryMethodCall = JPrimaryMethodCall
jMethodCall = JMethodCall

data JLiteral
  = JNull
  | JBoolean Bool
  | JInt Integer
  | JWord Integer
  | JChar Char
  | JFloat Double
  | JDouble Double
  | JString String
    deriving (Eq, Ord, Show)

jNull = JNull
jBoolean = JBoolean
jInt = JInt
jWord = JWord
jChar = JChar
jFloat = JFloat
jDouble = JDouble
jString = JString

data JAssignOp
  = JEqualA
    deriving (Eq, Ord, Show)

jEqualA = JEqualA

data JVarDecl = JVarDecl JVarDeclId (Maybe JVarInit)
                deriving (Eq, Ord, Show)

jVarDecl = JVarDecl

getVarDeclId (JVarDecl vid _) = vid

data JVarDeclId
  = JVarId JIdent
    deriving (Eq, Ord, Show)

data JVarDeclIdType
  = VARID
    deriving (Eq, Ord, Show)
             
varDeclIdType (JVarId _) = VARID

getVarIdent (JVarId id) = id

jVarId = JVarId

data JVarInit
  = JInitExp JExp
    deriving (Eq, Ord, Show)

jInitExp = JInitExp

data JModifier
  = JPublic
  | JPrivate
  | JProtected
  | JFinal
  | JAbstract
  | JStatic
  | JAnnotation JAnnotation
    deriving (Eq, Ord, Show)

jPublic = JPublic
jPrivate = JPrivate
jProtected = JProtected
jFinal = JFinal
jAbstract = JAbstract
jStatic = JStatic
jAnnotation = JAnnotation

data JLhs
  = JNameLhs JName
  | JFieldLhs JFieldAccess
            deriving (Eq, Ord, Show)

jNameLhs = JNameLhs
jFieldLhs = JFieldLhs

data JIdent = JIdent String
              deriving (Eq, Ord, Show)

jIdent = JIdent

jIdentName (JIdent str) = str

data JName = JName [JIdent]
             deriving (Eq, Ord, Show)

jName = JName

data JType
  = JPrimType JPrimType
  | JRefType JRefType
    deriving (Eq, Ord, Show)

jRefType = JRefType
jPrimType = JPrimType

data JPrimType
  = JBooleanT
  | JByteT
  | JShortT
  | JIntT
  | JLongT
  | JCharT
  | JFloatT
  | JDoubleT
    deriving (Eq, Ord, Show)

jBooleanT = JBooleanT
jByteT = JByteT
jShortT = JShortT
jIntT = JIntT
jLongT = JLongT
jCharT = JCharT
jFloatT = JFloatT
jDoubleT = JDoubleT

data JTypeParam = JTypeParam JIdent [JRefType]
                  deriving (Eq, Ord, Show)

jTypeParam = JTypeParam

data JFormalParam = JFormalParam [JModifier] JType Bool JVarDeclId
                    deriving (Eq, Ord, Show)

jFormalParam = JFormalParam

data JRefType = JClassRefType JClassType
                deriving (Eq, Ord, Show)

jClassRefType = JClassRefType

data JClassType = JClassType [(JIdent, [JTypeArgument])]
                  deriving (Eq, Ord, Show)

jClassType = JClassType

data JTypeArgument = JActualType JRefType
                     deriving (Eq, Ord, Show)

type JExceptionType = JRefType

data JAnnotation
  = JMarkerAnnotation JName
    deriving (Eq, Ord, Show)

jMarkerAnnotation = JMarkerAnnotation

-- Desugaring conversion
dsCompilationUnit :: JCompilationUnit -> JError DCompilationUnit
dsCompilationUnit jcu@(JCompilationUnit pkg imps typeDecls) =
  let classes = classDecls jcu in
  do
    dsClasses <- mapM dsClassDecl classes
    return $ dCompilationUnit Nothing [] [] dsClasses

dsClassDecl :: JClassDecl -> JError DClassDecl
dsClassDecl cd = return $ dClassDecl "Empty" [] Nothing [] [] []

dsBlockStmt :: Set DTypeParam -> JBlockStmt -> JError DStmt
dsBlockStmt tvs (JLocalVars mods tp decls) = fail "dsBlockStmt not implemented"

dsVarIdent :: JIdent -> DVarIdent
dsVarIdent (JIdent n) = dVarIdent n

dsTypeParam :: Set DTypeParam -> JTypeParam -> DTypeParam
dsTypeParam existingTps (JTypeParam id refs) =
  let dRefs = L.map (dsRefType existingTps) refs in
  dTypeParam (jIdentName id) dRefs

dsRefType :: Set DTypeParam -> JRefType -> DRefType
dsRefType typeParams (JClassRefType ct) =
  dClassRefType $ dsClassType typeParams ct

dsClassType :: Set DTypeParam -> JClassType -> DClassType
dsClassType typeParams (JClassType cts) =
  let ids = L.map fst cts
      tpArgs = L.map snd cts
      dsIds = L.map (dsTypeId typeParams) ids
      dsTpArgs = L.map (L.map (dsTypeArgument typeParams)) tpArgs in
  dClassType $ L.zip dsIds dsTpArgs

dsTypeId :: Set DTypeParam -> JIdent -> DTypeId
dsTypeId typeParams (JIdent name) =
  case isTypeParam typeParams name of
    True -> dTypeVar name
    False -> dClassName name

isTypeParam tps name =
  S.member name $ S.map dTypeParamName tps

dsTypeArgument :: Set DTypeParam -> JTypeArgument -> DTypeArg
dsTypeArgument typeParams (JActualType rt) =
  dActualType $ dsRefType typeParams rt
