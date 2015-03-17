module Javalyzer.Utils(JError(..)) where

import Control.Applicative
import Control.Monad

data JError a
     = JFail String
     | JSuccess a
       deriving (Eq, Ord)

instance (Show a) => (Show (JError a)) where
  show (JFail str) = "JError: " ++ str
  show (JSuccess res) = "Success: " ++ show res

jFail :: String -> JError a
jFail str = JFail str

instance Monad JError where
  (>>=) (JFail str) _ = JFail str
  (>>=) (JSuccess res) f = f res
  return a = JSuccess a
  fail str = JFail str

instance Applicative JError where
  pure a = JSuccess a
  (<*>) (JFail str) _ = JFail str
  (<*>) _ (JFail str) = JFail str
  (<*>) (JSuccess f) (JSuccess a) = JSuccess (f a)
  
instance Functor JError where
  fmap f (JFail str) = JFail str
  fmap f (JSuccess a) = JSuccess (f a)
