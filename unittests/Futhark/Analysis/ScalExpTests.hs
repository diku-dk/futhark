{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.Analysis.ScalExpTests
  ( tests
  , parseScalExp
  )
where

import Test.Tasty

import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import Data.Void
import Text.Megaparsec hiding (token, (<|>), many, State)
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Futhark.Analysis.ScalExp
import Futhark.Representation.AST hiding (constant, SDiv)

tests :: TestTree
tests = testGroup "ScalExpTests" []

parseScalExp :: M.Map String (Int, Type) -> String -> ScalExp
parseScalExp m s = case evalState (runParserT expr ("string: " ++ s) s) (0, m) of
  Left err -> error $ show err
  Right e  -> e

type ParserState = (Int, M.Map String (Int, Type))
type Parser = ParsecT Void String (State ParserState)

newVar :: String -> Type -> Parser Ident
newVar s t = do (x, m) <- lift get
                case M.lookup s m of
                  Just _ -> fail $ "Variable " ++ s ++ " double-declared."
                  Nothing -> do lift $ put (x+1, M.insert s (x,t) m)
                                return $ Ident (VName (nameFromString s) x) t

knownVar :: String -> Parser Ident
knownVar s = do (_, m) <- lift get
                case M.lookup s m of
                  Just (y,t) -> return $ Ident (VName (nameFromString s) y) t
                  Nothing -> fail $ "Undeclared variable " ++ s

token :: String -> Parser ()
token = L.lexeme space . void . string

parens :: Parser a -> Parser a
parens = between (token "(") (token ")")

identifier :: Parser Ident
identifier = do s <- (:) <$> letterChar <*> many alphaNumChar
                varDecl s <|> knownVar s
  where varDecl s = do
          t <- parens $
               (token "int" >> pure (Prim $ IntType Int32)) <|>
               (token "float32" >> pure (Prim $ FloatType Float32)) <|>
               (token "float64" >> pure (Prim $ FloatType Float64)) <|>
               (token "bool" >> pure (Prim Bool))
          newVar s t

constant :: Parser ScalExp
constant = (token "True" >> pure (Val $ BoolValue True)) <|>
           (token "False" >> pure (Val $ BoolValue True)) <|>
           (Val . IntValue . Int32Value <$> integer)
  where integer = L.lexeme space (L.signed space L.decimal)

expr :: Parser ScalExp
expr = makeExprParser prim operators

prim :: Parser ScalExp
prim = parens expr <|>
       constant <|>
       maxapp <|>
       minapp <|>
       (scalExpId <$> identifier)
  where maxapp = token "max" >> MaxMin False <$> parens (expr `sepBy` comma)
        minapp = token "min" >> MaxMin True <$> parens (expr `sepBy` comma)
        comma = token ","
        scalExpId (Ident name (Prim t)) = Id name t
        scalExpId (Ident name t) = error $
                                   pretty name ++ " is of type " ++ pretty t ++
                                   " but supposed to be a ScalExp."

operators :: [[Operator Parser ScalExp]]
operators = [ [Prefix (token "-"   >> return SNeg)]
            , [InfixL (token "*"   >> return STimes)]
            , [InfixL (token "pow" >> return SPow)]
            , [InfixL (token "/"   >> return SDiv)]
            , [InfixL (token "+"   >> return SPlus)]
            , [InfixL (token "-"   >> return SMinus)]
            , [InfixL (token "<="  >> return leq)]
            , [InfixL (token "<"   >> return lth)]
            , [InfixL (token ">="  >> return (flip leq))]
            , [InfixL (token ">"   >> return (flip lth))]
            , [InfixL (token "&&"  >> return SLogAnd)]
            , [InfixL (token "||"  >> return SLogOr)]
            ]
  where leq x y =
          RelExp LEQ0 $ x `SMinus` y
        lth x y =
          RelExp LTH0 $ x `SMinus` y
