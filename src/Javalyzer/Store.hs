module Javalyzer.Store(
  Store, emptyStore, addDefaultStoreValue, getNameValue, setNameValue, setLhsValue, isNull, createNewInstanceOfClass,
  StoreValue) where

import Data.List as L
import Data.Map as M

import Javalyzer.UJava
import Javalyzer.Utils

data Store
  = Store (Map String Int) (Map Int StoreValue) Int
    deriving (Eq, Ord, Show)

emptyStore :: Store
emptyStore = Store M.empty M.empty 0

addDefaultStoreValue :: Type -> String -> Store -> Store
addDefaultStoreValue t name (Store namesToInds indsToVals i) =
  Store newNames newInds (i+1)
  where
    newNames = M.insert name i namesToInds
    newInds = M.insert i (defaultValue t) indsToVals

getNameValue :: String -> Store -> JError StoreValue
getNameValue name store = do
  ind <- getInd name store
  val <- indexValue ind store
  return val

setNameValue :: String -> StoreValue -> Store -> JError Store
setNameValue name val s = do
  ind <- getInd name s
  return $ setInd ind val s

createNewInstanceOfClass :: String -> ClassHierarchy -> Store -> JError (StoreValue, Store)
createNewInstanceOfClass className h s = do
  classFields <- getClassFields className h
  let newObj = createNewObj className classFields
      (objInd, newStore) = addValue newObj s in
    do
      return $ (classRef objInd, newStore)

addValue :: StoreValue -> Store -> (Int, Store)
addValue val (Store namesToInds indsToVals c) =
  (c, Store namesToInds (M.insert c val indsToVals) (c+1))

createNewObj :: String -> [Field] -> StoreValue
createNewObj className fields =
  let defaultVals = L.map (defaultValue . fieldType) fields
      fieldNames = L.map fieldName fields
      fieldMap = M.fromList $ L.zip fieldNames defaultVals in
  obj className fieldMap

setLhsValue :: Lhs -> StoreValue -> Store -> JError Store
setLhsValue l v s =
  case lhsType l of
    LFIELDACCESS -> setObjField (getFieldAccFromLhs l) v s
    LVAR -> setNameValue (getNameFromLhs l) v s
--    _ -> error $ (show l) ++ " " ++ (show v) ++ " setValue not implemented"

getInd :: String -> Store -> JError Int
getInd name s@(Store nameInds _ _) =
  case M.lookup name nameInds of
    Just ind -> JSuccess ind
    Nothing -> fail $ name ++ " is not defined in store " ++ show s

setInd :: Int -> StoreValue -> Store -> Store
setInd ind val (Store nameInds indsToVals c) =
  Store nameInds (M.insert ind val indsToVals) c

indexValue :: Int -> Store -> JError StoreValue
indexValue index s@(Store _ indsToVals _) =
  case M.lookup index indsToVals of
    Just v -> JSuccess v
    Nothing -> fail $ (show index) ++ " is not defined in store " ++ show s

setObjField:: FieldAccess -> StoreValue -> Store -> JError Store
setObjField fa val store = do
  objRef <- getNameValue (objAccessedName fa) store
  oldObj <- indexValue (referencedIndex objRef) store
  let newObj = replaceObjField (fieldAccessedName fa) val oldObj in
    return $ setInd (referencedIndex objRef) newObj store

getField :: String -> String -> Store -> StoreValue
getField objN fName s@(Store nameVals sValRefs _) =
  case M.lookup objN nameVals of
    Just ind -> case M.lookup ind sValRefs of
      Just val -> val
      Nothing -> error $ objN ++ "." ++ fName ++ " does not exist in store " ++ show s
    Nothing -> error $ objN ++ " does not exist in store " ++ show s

data StoreValue
  = ClassRef Int
  | Obj String (Map String StoreValue)
  | Null
    deriving (Eq, Ord, Show)

classRef = ClassRef
obj = Obj

isNull :: String -> Store -> JError Bool
isNull name s = do
  val <- getNameValue name s
  return $ isStoreValueNull val

isStoreValueNull :: StoreValue -> Bool
isStoreValueNull Null = True
isStoreValueNull _ = False

referencedIndex (ClassRef ind) = ind

defaultValue :: Type -> StoreValue
defaultValue tp = case isRef tp of
  True -> Null
  False -> error $ "No support for primitive default values yet"

replaceObjField :: String -> StoreValue -> StoreValue -> StoreValue
replaceObjField fieldName newVal oldObj = error $ "not implemented"
