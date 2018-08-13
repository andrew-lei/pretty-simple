{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Text.Pretty.Simple.Internal.ExprParser
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Text.Pretty.Simple.Internal.ExprParser
  where

import Text.Pretty.Simple.Internal.Expr (CommaSeparated(..), Expr(..))
import Control.Arrow (first, (***))

testString1, testString2 :: String
testString1 = "Just [TextInput {textInputClass = Just (Class {unClass = \"class\"}), textInputId = Just (Id {unId = \"id\"}), textInputName = Just (Name {unName = \"name\"}), textInputValue = Just (Value {unValue = \"value\"}), textInputPlaceholder = Just (Placeholder {unPlaceholder = \"placeholder\"})}, TextInput {textInputClass = Just (Class {unClass = \"class\"}), textInputId = Just (Id {unId = \"id\"}), textInputName = Just (Name {unName = \"name\"}), textInputValue = Just (Value {unValue = \"value\"}), textInputPlaceholder = Just (Placeholder {unPlaceholder = \"placeholder\"})}]"
testString2 = "some stuff (hello [\"dia\\x40iahello\", why wh, bye] ) (bye)"

expressionParse :: String -> [Expr]
expressionParse = fst . parseExprs ""

parseExpr :: String -> String -> (Expr, String)
parseExpr _ ('(':rest) = makeBracketed Parens $ parseCSep ')' rest
parseExpr _ ('[':rest) = makeBracketed Brackets $ parseCSep ']' rest
parseExpr _ ('{':rest) = makeBracketed Braces $ parseCSep '}' rest
parseExpr _ ('"':rest) = StringLit *** (drop 1) $ parseStringLit rest
parseExpr end other    = first Other $ parseOther (end ++ "({[") other

makeBracketed :: (CommaSeparated [Expr] -> Bool -> Expr) -> ([[Expr]], String) -> (Expr, String)
makeBracketed constr res@(_, rest) = ((flip constr) (length rest > 0) . CommaSeparated) *** (drop 1) $ res

parseExprs :: String -> String -> ([Expr], String)
parseExprs _ [] = ([], "")
parseExprs end s@(c:_)
  | c `elem` end = ([], s)
  | otherwise = let (parsed, rest') = parseExpr end s
                    (toParse, rest) = parseExprs end rest'
                 in (parsed : toParse, rest)

parseCSep :: Char -> String -> ([[Expr]], String)
parseCSep _ [] = ([], "")
parseCSep end s@(c:cs)
  | c == end = ([], s)
  | c == ',' = parseCSep end cs
  | otherwise = let (parsed, rest') = parseExprs [end,','] s
                    (toParse, rest) = parseCSep end rest'
                 in (parsed : toParse, rest)

parseStringLit :: String -> (String, String)
parseStringLit [] = ("", "")
parseStringLit s@('"':_) = ("", s)
parseStringLit (c:cs)   = (c:cs', rest)
  where (cs', rest) = parseStringLit cs

parseOther :: String -> String -> (String, String)
parseOther _ [] = ("", "")
parseOther end s@(c:cs)
  | c `elem` end = ("", s) --("{[()]}\"," :: String) = ("", s)
  | otherwise = let (toParse, rest) = parseOther end cs
                 in (c : toParse, rest)
