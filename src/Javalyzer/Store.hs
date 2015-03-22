module Javalyzer.Store(
  Store, emptyStore, addStoreValue, setValue, getField,
  StoreValue) where

import Data.Map as M

import Javalyzer.UJava

data Store
  = Store (Map String Int) (Map Int StoreValue) Int
    deriving (Eq, Ord, Show)

emptyStore :: Store
emptyStore = Store M.empty M.empty 0

addStoreValue :: Type -> String -> Store -> Store
addStoreValue t name s = s

setValue :: Lhs -> StoreValue -> Store -> Store
setValue l v s = s

getField :: String -> String -> Store -> StoreValue
getField objN fName s@(Store nameVals sValRefs _) =
  case M.lookup objN nameVals of
    Just ind -> case M.lookup ind sValRefs of
      Just val -> val
      Nothing -> error $ objN ++ "." ++ fName ++ " does not exist in store " ++ show s
    Nothing -> error $ objN ++ " does not exist in store " ++ show s

data StoreValue
  = ClassRef Int
    deriving (Eq, Ord, Show)

