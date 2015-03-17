module Javalyzer.Parser(parseCompilationUnit,
                        jCompUnit,
                        jClassTypeDecl,
                        jClassBody,
                        jIdent) where

import Data.List as L
import Language.Java.Parser
import Text.Parsec.Error

import Javalyzer.Syntax

parseCompilationUnit :: String -> Either JParseError JCompilationUnit
parseCompilationUnit name = case parser compilationUnit name of
  Left err -> Left $ jParseError $ show err
  Right compUnit ->
    Right $ compUnitToJ compUnit
