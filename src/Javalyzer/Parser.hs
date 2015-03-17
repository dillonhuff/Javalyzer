module Javalyzer.Parser(parseCompilationUnit,
                        jCompUnit,
                        jClassTypeDecl,
                        jClassBody,
                        jIdent) where

import Data.List as L
import Language.Java.Parser
import Text.Parsec.Error

import Javalyzer.Syntax
import Javalyzer.Utils

parseCompilationUnit :: String -> JError JCompilationUnit
parseCompilationUnit name = case parser compilationUnit name of
  Left err -> fail $ show err
  Right compUnit -> return $ compUnitToJ compUnit
