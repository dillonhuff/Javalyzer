module Javalyzer.Store(
  Store, emptyStore, addDefaultStoreValue, getNameValue, setNameValue, setValue, isNull,
  StoreValue) where

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

setValue :: Lhs -> StoreValue -> Store -> Store
setValue l v s =
  case lhsType l of
    LFIELDACCESS -> error "setValue not implemented" --setFieldOjbField (getFieldAccFromLhs l) v s
    _ -> error $ (show l) ++ " " ++ (show v) ++ "setValue not implemented"

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

setObjField:: FieldAccess -> StoreValue -> Store -> Store
setObjField fa val store = error "setObjField not implemented"
{-  case getInd objName store of
    Just ind -> setClassField ind objFieldName val store
    Nothing -> error $ objName ++ " with field " ++ objFieldName ++ " does not exist in store " ++ show store-}

setClassField :: Int -> String -> StoreValue -> Store -> Store
setClassField classLoc fieldName val s = error "setClassField"
--  case getVal classLoc s of
--    Just obj@(Obj className fields) -> error "setClassField not implemented"--setInd classLoc (setField fieldName val obj) s
--    _ -> error $ (show classLoc) ++ " either is not a class or does not exist in " ++ (show s)
  
getField :: String -> String -> Store -> StoreValue
getField objN fName s@(Store nameVals sValRefs _) =
  case M.lookup objN nameVals of
    Just ind -> case M.lookup ind sValRefs of
      Just val -> val
      Nothing -> error $ objN ++ "." ++ fName ++ " does not exist in store " ++ show s
    Nothing -> error $ objN ++ " does not exist in store " ++ show s

data StoreValue
  = ClassRef Int
  | Obj String [Field]
  | Null
    deriving (Eq, Ord, Show)

isNull :: String -> Store -> JError Bool
isNull name s = do
  val <- getNameValue name s
  return $ isStoreValueNull val

isStoreValueNull :: StoreValue -> Bool
isStoreValueNull Null = True
isStoreValueNull _ = False

defaultValue :: Type -> StoreValue
defaultValue tp = case isRef tp of
  True -> Null
  False -> error $ "No support for primitive default values yet"
