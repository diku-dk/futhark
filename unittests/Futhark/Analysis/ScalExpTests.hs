{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.Analysis.ScalExpTests
  ( tests
  , parseScalExp
  , parseScalExp'
  )
where

import Test.Framework

import Control.Applicative
import Control.Monad.Identity
import qualified Data.Map as M
import Text.Parsec hiding (token, (<|>), many)
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import Futhark.Analysis.ScalExp
import Futhark.Representation.AST hiding (constant, SDiv)

tests :: [Test]
tests = []

languageDef :: GenLanguageDef String u Identity
languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = ["max", "min", "False", "True"]
           , Token.reservedOpNames = ["+", "-", "*", "/"
                                     , "<", ">", "<=", ">="
                                     , "&&", "||", "not"
                                     ]
           }

lexer :: Token.GenTokenParser String ParserState Identity
lexer = Token.makeTokenParser languageDef

parseScalExp :: String -> ScalExp
parseScalExp = parseScalExp' M.empty

parseScalExp' :: M.Map String (Int, Type) -> String -> ScalExp
parseScalExp' m s = case runParser expr (0, m) ("string: " ++ s) s of
  Left err -> error $ show err
  Right e  -> e

type ParserState = (Int, M.Map String (Int, Type))
type Parser = ParsecT String ParserState Identity

newVar :: String -> Type -> Parser Ident
newVar s t = do (x, m) <- getState
                case M.lookup s m of
                  Just _ -> fail $ "Variable " ++ s ++ " double-declared."
                  Nothing -> do setState (x+1, M.insert s (x,t) m)
                                return $ Ident (VName (nameFromString s) x) t

knownVar :: String -> Parser Ident
knownVar s = do (_, m) <- getState
                case M.lookup s m of
                  Just (y,t) -> return $ Ident (VName (nameFromString s) y) t
                  Nothing -> fail $ "Undeclared variable " ++ s

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

identifier :: Parser Ident
identifier = do s <- (:) <$> letter <*> many alphaNum
                varDecl s <|> knownVar s
  where varDecl s = do
          t <- parens $
               (reserved "int" >> pure (Prim $ IntType Int32)) <|>
               (reserved "float32" >> pure (Prim $ FloatType Float32)) <|>
               (reserved "float64" >> pure (Prim $ FloatType Float64)) <|>
               (reserved "bool" >> pure (Prim Bool))
          newVar s t

constant :: Parser ScalExp
constant = (reserved "True" >> pure (Val $ BoolValue True)) <|>
           (reserved "False" >> pure (Val $ BoolValue True)) <|>
           (Val . IntValue . Int32Value <$> integer)
  where integer = fromIntegral <$> Token.integer lexer

expr :: Parser ScalExp
expr = buildExpressionParser operators prim

prim :: Parser ScalExp
prim = parens expr <|>
       constant <|>
       maxapp <|>
       minapp <|>
       (scalExpId <$> identifier)
  where maxapp = reserved "max" >> MaxMin False <$> parens (expr `sepBy` comma)
        minapp = reserved "min" >> MaxMin True <$> parens (expr `sepBy` comma)
        comma = Token.comma lexer
        scalExpId (Ident name (Prim t)) = Id name t
        scalExpId (Ident name t) = error $
                                   pretty name ++ " is of type " ++ pretty t ++
                                   " but supposed to be a ScalExp."

operators :: [[Operator String ParserState Identity ScalExp]]
operators = [ [Prefix (reservedOp "-"   >> return SNeg)          ]
            , [Infix  (reservedOp "*"   >> return STimes) AssocLeft]
            , [Infix  (reservedOp "pow"   >> return SPow) AssocLeft]
            , [Infix  (reservedOp "/"   >> return SDiv) AssocLeft]
            , [Infix  (reservedOp "+"   >> return SPlus) AssocLeft]
            , [Infix  (reservedOp "-"   >> return SMinus) AssocLeft]
            , [Infix  (reservedOp "<="   >> return leq) AssocLeft]
            , [Infix  (reservedOp "<"   >> return lth) AssocLeft]
            , [Infix  (reservedOp ">="   >> return (flip leq)) AssocLeft]
            , [Infix  (reservedOp ">"   >> return (flip lth)) AssocLeft]
            , [Infix  (reservedOp "&&"   >> return SLogAnd) AssocLeft]
            , [Infix  (reservedOp "||"   >> return SLogOr) AssocLeft]
            ]
  where leq x y =
          RelExp LEQ0 $ x `SMinus` y
        lth x y =
          RelExp LTH0 $ x `SMinus` y
