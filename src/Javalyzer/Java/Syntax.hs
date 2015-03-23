module Javalyzer.Java.Syntax(JParseError,
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
                        jImportDecl,
                        JBlockStmt,
                        jMethodBody,
                        dsCompilationUnit,
                        dsBlockStmt,
                        dsVarIdent,
                        dsTypeParam,
                        dsMods) where

import Control.Monad
import Data.List as L
import Data.Set as S

import Javalyzer.Java.Desugared
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
interfaceDecls jcu = L.map getInterfaceDecl $ L.filter isInterfaceDecl $ decls jcu

isSingleClass (JCompilationUnit _ _ decls) =
  case length decls == 1 of
    True -> case decls of
      ((JClassTypeDecl classDec):nil) -> True
      _ -> False
    False -> False


data JPackageDecl = JPackageDecl JName
                    deriving (Eq, Ord, Show)

jPackageDecl = JPackageDecl

data JImportDecl = JImportDecl Bool JName Bool
                   deriving (Eq, Ord, Show)

jImportDecl = JImportDecl

data JTypeDecl
  = JClassTypeDecl JClassDecl
  | JInterfaceTypeDecl JInterfaceDecl
    deriving (Eq, Ord, Show)

jClassTypeDecl mods name typeParams super refTypes body =
  JClassTypeDecl $ jClassDecl mods name typeParams super refTypes body

isClassDecl (JClassTypeDecl _) = True
isClassDecl _ = False

isInterfaceDecl (JInterfaceTypeDecl _) = True
isInterfaceDecl _ = False

getClassDecl (JClassTypeDecl d) = d
getInterfaceDecl (JInterfaceTypeDecl i) = i

data JClassDecl = JClassDecl [JModifier] JIdent [JTypeParam] (Maybe JRefType) [JRefType] JClassBody
                  deriving (Eq, Ord, Show)

jClassDecl = JClassDecl
className (JClassDecl _ id _ _ _ _) = jIdentName id
classBody (JClassDecl _ _ _ _ _ b) = b

data JInterfaceDecl = JInterfaceDecl
                      deriving (Eq, Ord, Show)

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

getMembers decls = L.map getMember $ L.filter (\d -> declType d == MEMBER) decls
getMethods decls = L.filter (\m -> memberDeclType m == METHOD) $ getMembers decls
getConstructors decls = L.filter (\m -> memberDeclType m == CONSTRUCTOR) $ getMembers decls
getFieldDecls decls = L.filter (\m -> memberDeclType m == FIELD) $ getMembers decls

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
  let classes = classDecls jcu
      interfaces = interfaceDecls jcu
      pkgD = dsPackageDecl pkg
      impsD = L.map dsImportDecl imps in
  do
    classesD <- mapM dsClassDecl classes
    interfacesD <- mapM dsInterfaceDecl interfaces
    return $ dCompilationUnit pkgD impsD interfacesD classesD

dsPackageDecl :: Maybe JPackageDecl -> Maybe DPackage
dsPackageDecl Nothing = Nothing
dsPackageDecl (Just (JPackageDecl (JName strs))) = Just $ dPackage $ L.map jIdentName strs

dsImportDecl :: JImportDecl -> DImportDecl
dsImportDecl (JImportDecl onlyStatic (JName strs) onlyOneName) =
  dImportDecl onlyStatic onlyOneName $ L.map jIdentName strs

dsInterfaceDecl :: JInterfaceDecl -> JError DInterfaceDecl
dsInterfaceDecl int = fail "dsInterfaceDecl is not implemented"

dsClassDecl :: JClassDecl -> JError DClassDecl
dsClassDecl (JClassDecl mods id tps super refs body) = do
  modsD <- dsMods mods
  let name = jIdentName id
      tpsD = L.map (dsTypeParam S.empty) tps
      refsD = L.map (dsRefType S.empty) refs
      superD = dsSuperType super in
   do
     (fields, methods, constructors) <- dsClassBody (S.fromList tpsD) body
     return $ dClassDecl name tpsD superD fields methods constructors

dsSuperType :: Maybe JRefType -> Maybe DRefType
dsSuperType (Just rt) = Just $ dsRefType S.empty rt
dsSuperType Nothing = Nothing

dsClassBody :: Set DTypeParam ->
               JClassBody ->
               JError ([DVarDecl], [DMethod], [DConstructor])
dsClassBody tps (JClassBody decls) =
  let methods = getMethods decls
      fieldDecls = getFieldDecls decls
      constructors = getConstructors decls in
  do
    methodsD <- mapM (dsMethod tps) methods
    fieldDeclsD <- liftM L.concat $ mapM (dsInstanceFieldDecl tps) fieldDecls
    constructorsD <- mapM (dsConstructor tps) constructors
    return $ (fieldDeclsD, methodsD, constructorsD)

dsMethod :: Set DTypeParam -> JMemberDecl -> JError DMethod
dsMethod tps (JMethodDecl mods methTps retType name args exps body) =
  let methTpsD = L.map (dsTypeParam tps) methTps
      expsD = L.map (dsRefType tps) exps in
  do
    modsD <- dsMods mods

    let nameStr = jIdentName name
        newTps = S.union (S.fromList methTpsD) tps in
      do
        retTypeD <- dsRetType newTps retType
        argsD <- mapM (dsFormalParam newTps) args
        bodyStmts <- dsMethodBody newTps body
        return $ dMethod modsD methTpsD retTypeD nameStr argsD expsD bodyStmts

dsRetType :: Set DTypeParam -> Maybe JType -> JError (Maybe DType)
dsRetType tps (Just tp) = do
  retType <- dsType tps tp
  return $ Just $ retType
dsRetType tps Nothing = return Nothing

dsFormalParam :: Set DTypeParam -> JFormalParam -> JError DVarDecl
dsFormalParam tps (JFormalParam mods tp False varDeclId) = do
  res <- dsInstanceFieldDecl tps (jFieldDecl mods tp [jVarDecl varDeclId Nothing])
  return $ head res
dsFormalParam tps param = fail $ (show param) ++ " is not supported by dsFormalParam"

dsMethodBody :: Set DTypeParam -> JMethodBody -> JError [DStmt]
dsMethodBody tps (JMethodBody (Just (JBlock stmts))) =
  liftM L.concat $ mapM (dsBlockStmt tps) stmts

dsInstanceFieldDecl :: Set DTypeParam -> JMemberDecl -> JError [DVarDecl]
dsInstanceFieldDecl tps (JFieldDecl mods tp decls) = do
  tpD <- dsType tps tp
  decls <- liftM L.concat $ mapM (dsVarDecl mods tpD) decls
  return decls

dsConstructor :: Set DTypeParam -> JMemberDecl -> JError DConstructor
dsConstructor tps (JConstructorDecl mods conTps name args exps body) =
  let conTpsD = L.map (dsTypeParam tps) conTps
      expsD = L.map (dsRefType tps) exps
      nameStr = jIdentName name
      newTps = S.union (S.fromList conTpsD) tps in
  do
    modsD <- dsMods mods
    argsD <- mapM (dsFormalParam newTps) args
    bodyStmts <- dsConstructorBody newTps body
    return $ dConstructor modsD conTpsD nameStr argsD expsD bodyStmts

dsConstructorBody :: Set DTypeParam -> JConstructorBody -> JError DConstructorBody
dsConstructorBody tps (JConstructorBody Nothing stmts) = do
  stmtsD <- liftM L.concat $ mapM (dsBlockStmt tps) stmts
  return $ dConstructorBody Nothing stmtsD
dsConstructorBody tps other = fail $ (show other) ++ " is not supported by dsConstructorBody"

dsBlockStmt :: Set DTypeParam -> JBlockStmt -> JError [DStmt]
dsBlockStmt tvs (JLocalVars mods tp decls) = do
  tpD <- dsType tvs tp
  liftM L.concat $ mapM (dsVarDeclStmt mods tpD) decls
dsBlockStmt tvs (JBlockStmt stmt) = do
  st <- dsStmt tvs stmt
  return [st]

dsStmt :: Set DTypeParam -> JStmt -> JError DStmt
dsStmt tvs (JReturn Nothing) = return $ dReturn Nothing
dsStmt tvs (JReturn (Just exp)) = do
  expD <- dsExp tvs exp
  return $ dReturn $ Just expD
dsStmt tvs other = fail $ (show other) ++ " is not yet supported by dsStmt"

dsExp :: Set DTypeParam -> JExp -> JError DExp
dsExp tvs JThis = return dThis
dsExp tvs (JLit l) = return $ dLit $ dsLiteral l
dsExp tvs (JExpName n) = return $ dExpName $ dsName n
dsExp tvs (JFieldAccess (JPrimaryFieldAccess exp id)) = do
  expD <- dsExp tvs exp
  return $ dPrimaryFieldAccess expD (dsVarIdent id)
dsExp tvs other = fail $ (show other) ++ " is not yet supported by dsExp"

dsLiteral :: JLiteral -> DLiteral
dsLiteral (JChar c) = dChar c

dsVarDeclStmt :: [JModifier] -> DType -> JVarDecl -> JError [DStmt]
dsVarDeclStmt mods tp vdecl = do
  vdeclD <- dsVarDecl mods tp vdecl
  return $ L.map dVarDeclSt vdeclD

dsVarDecl :: [JModifier] -> DType -> JVarDecl -> JError [DVarDecl]
dsVarDecl mods tp (JVarDecl vdid Nothing) = do
  modsD <- dsMods mods
  (vdidD, arrDepth) <- dsVarDeclId vdid
  case arrDepth of
    0 -> return [dVarDecl modsD tp vdidD]
    _ -> fail $ (show arrDepth) ++ " is not supported array depth in dsVarDecl"
dsVarDecl mods tp other =
  fail $ (show other) ++ " is not supported by dsVarDecl"

dsVarDeclId :: JVarDeclId -> JError (DVarIdent, Int)
dsVarDeclId (JVarId id) = return $ (dsVarIdent id, 0)

dsVarIdent :: JIdent -> DVarIdent
dsVarIdent (JIdent n) = dVarIdent n

dsTypeParam :: Set DTypeParam -> JTypeParam -> DTypeParam
dsTypeParam existingTps (JTypeParam id refs) =
  let dRefs = L.map (dsRefType existingTps) refs in
  dTypeParam (jIdentName id) dRefs

dsType :: Set DTypeParam -> JType -> JError DType
dsType typeParams (JRefType rt) =
  let rtD = dsRefType typeParams rt in
  return $ dRefType rtD
dsType typeParams (JPrimType pt) =
  return $ dPrimType $ dsPrimType pt

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

dsPrimType :: JPrimType -> DPrimType
dsPrimType JIntT = dIntT

dsName :: JName -> DName
dsName (JName strs) = dName $ L.map dsVarIdent strs

dsMods :: [JModifier] -> JError Modifiers
dsMods mds = do
  acc <- dsAccessMod mds
  implLevel <- dsImplLevel mds
  isStatic <- dsStaticMod mds
  return $ mods acc implLevel isStatic

dsAccessMod mods =
  let accessMods = L.filter (\x -> or [x == jPublic, x == jPrivate, x == jProtected]) mods in
  case length accessMods > 1 of
    True -> fail $ (show mods) ++ " includes conflicting access modifiers"
    False -> case length accessMods == 0 of
      True -> return public
      False -> case head accessMods of
        JPublic -> return public
        JPrivate -> return private
        JProtected -> return protected

dsImplLevel mods =
  let implMods = L.filter (\x -> or [x == jAbstract, x == jFinal]) mods in
  case length implMods > 1 of
    True -> fail $ (show mods) ++ " includes conflicting access modifiers"
    False -> case length implMods == 0 of
      True -> return realExtendable
      False -> case head implMods of
        JFinal -> return final
        JAbstract -> return abstract

dsStaticMod mods = return $ L.elem jStatic mods
