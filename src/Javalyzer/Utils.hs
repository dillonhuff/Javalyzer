module Javalyzer.Utils(JError,
                       success,
                       getSuccess,
                       isFail) where

import Control.Applicative
import Control.Monad

data JError a
     = JFail ErrorMsg
     | JSuccess a
       deriving (Eq, Ord)

jErr = JFail

isFail (JFail _) = True
isFail _ = False

success = JSuccess
getSuccess (JSuccess m) = m

instance (Show a) => (Show (JError a)) where
  show (JFail err) = show err
  show (JSuccess res) = "Success: " ++ show res

instance Monad JError where
  (>>=) (JFail str) _ = JFail str
  (>>=) (JSuccess res) f = f res
  return a = JSuccess a
  fail str = defaultErr str

instance Applicative JError where
  pure a = JSuccess a
  (<*>) (JFail str) _ = JFail str
  (<*>) _ (JFail str) = JFail str
  (<*>) (JSuccess f) (JSuccess a) = JSuccess (f a)
  
instance Functor JError where
  fmap f (JFail str) = JFail str
  fmap f (JSuccess a) = JSuccess (f a)

data ErrorMsg
  = ErrorMsg ErrorType String
    deriving (Eq, Ord)

instance Show ErrorMsg where
  show (ErrorMsg et msg) = show et ++ ": " ++ show msg

errMsg = ErrorMsg

data ErrorType
  = Parse
  | Desugar
  | ConvertToUJava
  | SymbolicExecution
  | Default
    deriving (Eq, Ord)

defaultErr str = jErr (errMsg Default str)

errStr = "Error during "
instance Show ErrorType where
  show Parse = errStr ++ "parsing"
  show Desugar = errStr ++ "desugaring"
  show ConvertToUJava = errStr ++ "conversion to UJava"
  show SymbolicExecution = errStr ++ "symbolic execution"
  show Default = "Error <DEFAULT_ERROR_TYPE>"
