{-# LANGUAGE GADTs #-}

module Futhark.SoP.Parse
  ( parseSoP,
    parseRange,
    parseRangeEnv,
    parsePrimExp,
    parsePrimExpToSoP,
  )
where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Char
import Data.Functor
import Data.Map qualified as M
import Data.Set qualified as S
import Data.String
import Data.Text qualified as T
import Data.Void
import Futhark.Analysis.PrimExp
import Futhark.Analysis.PrimExp.Parse
import Futhark.SoP.Convert
import Futhark.SoP.Monad
import Futhark.SoP.SoP
import Language.Futhark.Primitive.Parse
import Text.Megaparsec (Parsec, manyTill_, notFollowedBy, parse, try, (<?>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char.Lexer qualified as L
import Text.ParserCombinators.ReadP

tokenize :: ReadP a -> ReadP a
tokenize p = p <* skipSpaces

pChar :: Char -> ReadP Char
pChar = tokenize . char

pString :: String -> ReadP String
pString = tokenize . string

pInt :: (Read a, Integral a) => ReadP a
pInt =
  tokenize $ do
    sign <- option 1 (char '-' >> pure (-1))
    n <- read <$> munch1 isDigit
    pure $ sign * n

pSym :: ReadP String
pSym =
  tokenize $
    (:)
      <$> satisfy isLetter
      <*> munch (\c -> isAlphaNum c || c == '_')

pAtom :: ReadP (SoP String)
pAtom =
  choice
    [ between (pChar '(') (pChar ')') pSoP,
      sym2SoP <$> pSym,
      int2SoP <$> pInt
    ]

pMult :: ReadP (SoP String)
pMult = chainl1 pAtom (pChar '*' $> (.*.))

pPlus :: ReadP (SoP String)
pPlus = chainl1 pMult $ do
  op <- option '+' $ pChar '+' <|> pChar '-'
  let sign
        | op == '-' = (-1)
        | otherwise = 1
  pure $ \l r -> l .+. scaleSoP sign r

pSoP :: ReadP (SoP String)
pSoP = pPlus

pRange :: ReadP (String, Range String)
pRange = do
  lb <- pLowerBound
  k <- pK
  sym <- pSym
  ub <- pUpperBound
  pure (sym, Range lb k ub)
  where
    pK = option 1 $ pInt <* pChar '*'
    pLowerBound = option mempty $ do
      bset <- pBoundSet
      sign <- pString "<=" <|> pString "<"
      case sign of
        "<" -> pure $ S.map (.+. int2SoP 1) bset
        _ -> pure bset
    pUpperBound = option mempty $ do
      sign <- pString "<=" <|> pString "<"
      bset <- pBoundSet
      case sign of
        "<" -> pure $ S.map (.+. int2SoP (-1)) bset
        _ -> pure bset
    pBoundSet =
      S.fromList
        <$> choice
          [ pure <$> pSoP,
            between (pChar '{') (pChar '}') $
              sepBy pSoP (pChar ',')
          ]

parse' :: (Show a) => ReadP a -> String -> a
parse' p s =
  case readP_to_S (tokenize p <* eof) s of
    [(e, "")] -> e
    res -> error $ show res ++ "\n" ++ show s

parseSoP :: String -> SoP String
parseSoP = parse' pSoP

parseRange :: String -> (String, Range String)
parseRange = parse' pRange

parseRangeEnv :: [String] -> RangeEnv String
parseRangeEnv = M.fromList . map parseRange

pLeaf :: Parsec Void T.Text String
pLeaf = try $ lexeme $ MP.choice [try pVNameString, pFun]
  where
    pVNameString = do
      (s, tag) <-
        MP.satisfy constituent
          `manyTill_` try pTag
          <?> "variable name"
      pure $ s <> "_" <> show tag
    pTag :: Parsec Void T.Text Integer
    pTag =
      "_" *> L.decimal <* notFollowedBy (MP.satisfy constituent)
    pFun = do
      fun <- lexeme $ T.unpack <$> MP.takeWhileP Nothing constituent
      guard (fun `elem` M.keys primFuns)
      args <- pBalanced (0 :: Integer) (0 :: Integer) ""
      pure (fun <> args)
    pBalanced open close s
      | open > 0 && open == close = pure s
      | open < close = fail ""
      | otherwise = do
          c <- MP.anySingle
          let s' = s ++ [c]
          case c of
            '(' -> pBalanced (open + 1) close s'
            ')' -> pBalanced open (close + 1) s'
            _ -> pBalanced open close s'

parsePrimExp :: String -> PrimExp String
parsePrimExp s =
  case parse (pPrimExp (IntType Int64) pLeaf) "" (fromString s) of
    Left bundle -> error $ show bundle
    Right pe -> pe

parsePrimExpToSoP :: String -> SoPM String (PrimExp String) p (Integer, SoP String)
parsePrimExpToSoP = toSoPNum . parsePrimExp
