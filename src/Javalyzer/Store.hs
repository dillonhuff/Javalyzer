module Javalyzer.Store(
  Store, emptyStore, addStoreValue, setValue, getField, isNull,
  StoreValue) where

import Data.Map as M

import Javalyzer.UJava

data Store
  = Store (Map String Int) (Map Int StoreValue) Int
    deriving (Eq, Ord, Show)

emptyStore :: Store
emptyStore = Store M.empty M.empty 0

addStoreValue :: Type -> String -> Store -> Store
addStoreValue t name (Store namesToInds indsToVals i) =
  Store newNames newInds (i+1)
  where
    newNames = M.insert name i namesToInds
    newInds = M.insert i (defaultValue t) indsToVals

setValue :: Lhs -> StoreValue -> Store -> Store
setValue l v s = error "setValue not implemented"
  

getInd :: String -> Store -> Maybe Int
getInd name (Store nameInds _ _) = M.lookup name nameInds

indexValue :: Int -> Store -> Maybe StoreValue
indexValue name (Store _ indsToVals _) = M.lookup name indsToVals

getField :: String -> String -> Store -> StoreValue
getField objN fName s@(Store nameVals sValRefs _) =
  case M.lookup objN nameVals of
    Just ind -> case M.lookup ind sValRefs of
      Just val -> val
      Nothing -> error $ objN ++ "." ++ fName ++ " does not exist in store " ++ show s
    Nothing -> error $ objN ++ " does not exist in store " ++ show s

data StoreValue
  = ClassRef Int
  | Null
    deriving (Eq, Ord, Show)

isNull :: String -> Store -> Bool
isNull name s = case getInd name s of
  Just ind -> case indexValue ind s of
    Just val -> val == Null
    Nothing -> error $ name ++ " does not have an index in store " ++ show s
  Nothing -> error $ name ++ " is not defined in store " ++ show s

defaultValue :: Type -> StoreValue
defaultValue tp = case isRef tp of
  True -> Null
  False -> error $ "No support for primitive default values yet"
