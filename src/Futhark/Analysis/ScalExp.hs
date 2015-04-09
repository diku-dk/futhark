{-# LANGUAGE FlexibleContexts #-}
module Futhark.Analysis.ScalExp
  ( RelOp0(..)
  , ScalExp(..)
  , scalExpType
  , subExpToScalExp
  , toScalExp
  , expandScalExp
  , LookupVar
  , fromScalExp
  , fromScalExp'
  , sproduct
  , ssum
  )
where

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.HashSet as HS
import Data.Maybe
import Data.Monoid

import Text.PrettyPrint.Mainland hiding (pretty)

import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Substitute
import Futhark.Tools
import Futhark.Renamer

-----------------------------------------------------------------
-- BINARY OPERATORS for Numbers                                --
-- Note that MOD, BAND, XOR, BOR, SHIFTR, SHIFTL not supported --
--   `a SHIFTL/SHIFTR p' can be translated if desired as as    --
--   `a * 2^p' or `a / 2^p                                     --
-----------------------------------------------------------------

-- | Relational operators.
data RelOp0 = LTH0
            | LEQ0
             deriving (Eq, Ord, Enum, Bounded, Show)

-- | Representation of a scalar expression, which is:
--
--    (i) an algebraic expression, e.g., min(a+b, a*b),
--
--   (ii) a relational expression: a+b < 5,
--
--  (iii) a logical expression: e1 and (not (a+b>5)
data ScalExp= Val     BasicValue
            | Id      VName
            | SNeg    ScalExp
            | SNot    ScalExp
            | SPlus   ScalExp ScalExp
            | SMinus  ScalExp ScalExp
            | STimes  ScalExp ScalExp
            | SPow    ScalExp ScalExp
            | SDivide ScalExp ScalExp
            | MaxMin  Bool   [ScalExp]
            | RelExp  RelOp0  ScalExp
            | SLogAnd ScalExp ScalExp
            | SLogOr  ScalExp ScalExp
              deriving (Eq, Ord, Show)

instance Pretty ScalExp where
  pprPrec _ (Val val) = ppr $ BasicVal val
  pprPrec _ (Id v) = ppr v
  pprPrec _ (SNeg e) = text "-" <> pprPrec 9 e
  pprPrec _ (SNot e) = text "not" <+> pprPrec 9 e
  pprPrec prec (SPlus x y) = ppBinOp prec "+" 4 4 x y
  pprPrec prec (SMinus x y) = ppBinOp prec "-" 4 10 x y
  pprPrec prec (SPow x y) = ppBinOp prec "^" 6 6 x y
  pprPrec prec (STimes x y) = ppBinOp prec "*" 5 5 x y
  pprPrec prec (SDivide x y) = ppBinOp prec "/" 5 10 x y
  pprPrec prec (SLogOr x y) = ppBinOp prec "||" 0 0 x y
  pprPrec prec (SLogAnd x y) = ppBinOp prec "&&" 1 1 x y
  pprPrec prec (RelExp LTH0 e) = ppBinOp prec "<" 2 2 e (Val $ IntVal 0)
  pprPrec prec (RelExp LEQ0 e) = ppBinOp prec "<=" 2 2 e (Val $ IntVal 0)
  pprPrec _ (MaxMin True es) = text "min" <> parens (commasep $ map ppr es)
  pprPrec _ (MaxMin False es) = text "max" <> parens (commasep $ map ppr es)

ppBinOp :: Int -> String -> Int -> Int -> ScalExp -> ScalExp -> Doc
ppBinOp p bop precedence rprecedence x y =
  parensIf (p > precedence) $
           pprPrec precedence x <+/>
           text bop <+>
           pprPrec rprecedence y

instance Substitute ScalExp where
  substituteNames subst e =
    case e of Id v -> Id $ substituteNames subst v
              Val v -> Val v
              SNeg x -> SNeg $ substituteNames subst x
              SNot x -> SNot $ substituteNames subst x
              SPlus x y -> substituteNames subst x `SPlus` substituteNames subst y
              SMinus x y -> substituteNames subst x `SMinus` substituteNames subst y
              SPow x y -> substituteNames subst x `SPow` substituteNames subst y
              STimes x y -> substituteNames subst x `STimes` substituteNames subst y
              SDivide x y -> substituteNames subst x `SDivide` substituteNames subst y
              MaxMin m es -> MaxMin m $ map (substituteNames subst) es
              RelExp r x -> RelExp r $ substituteNames subst x
              SLogAnd x y -> substituteNames subst x `SLogAnd` substituteNames subst y
              SLogOr x y -> substituteNames subst x `SLogOr` substituteNames subst y

instance Rename ScalExp where
  rename se = do
    substs <- renamerSubstitutions
    return $ substituteNames substs se

scalExpType :: HasTypeEnv f => ScalExp -> f BasicType
scalExpType (Val ( IntVal _) ) = pure Int
scalExpType (Val (RealVal _) ) = pure Real
scalExpType (Val ( LogVal _) ) = pure Bool
scalExpType (Val val) =
  error $ "scalExpType: scalar exp cannot have type " ++
          pretty (basicValueType val) ++ "."
scalExpType (Id idd) =
  withType <$> lookupType idd
  where withType (Basic bt) = bt
        withType t          = error $ "scalExpType: var " ++ pretty idd ++
                              " in scalar exp cannot have type " ++
                              pretty t ++ "."
scalExpType (SNeg  e) = scalExpType e
scalExpType (SNot  _) = pure Bool
scalExpType (SPlus   e _) = scalExpType e
scalExpType (SMinus  e _) = scalExpType e
scalExpType (STimes  e _) = scalExpType e
scalExpType (SDivide e _) = scalExpType e
scalExpType (SPow    e _) = scalExpType e
scalExpType (SLogAnd _ _) = pure Bool
scalExpType (SLogOr  _ _) = pure Bool
scalExpType (RelExp  _ _) = pure Bool
scalExpType (MaxMin _ []) = pure Int
scalExpType (MaxMin _ (e:_)) = scalExpType e

-- | A function that checks whether a variable name corresponds to a
-- scalar expression.
type LookupVar = VName -> Maybe ScalExp

-- | Non-recursively convert a subexpression to a 'ScalExp'.
subExpToScalExp :: SubExp -> ScalExp
subExpToScalExp (Var v)        = Id v
subExpToScalExp (Constant val) = Val val

toScalExp :: LookupVar -> Exp lore -> Maybe ScalExp
toScalExp look (PrimOp (SubExp se))    =
  toScalExp' look se
toScalExp look (PrimOp (BinOp Less x y _)) =
  RelExp LTH0 <$> (sminus <$> toScalExp' look x <*> toScalExp' look y)
toScalExp look (PrimOp (BinOp Leq x y _)) =
  RelExp LEQ0 <$> (sminus <$> toScalExp' look x <*> toScalExp' look y)
toScalExp look (PrimOp (BinOp Equal x y Int)) = do
  x' <- toScalExp' look x
  y' <- toScalExp' look y
  return $ RelExp LEQ0 (x' `sminus` y') `SLogAnd` RelExp LEQ0 (y' `sminus` x')
toScalExp look (PrimOp (Negate e)) =
  SNeg <$> toScalExp' look e
toScalExp look (PrimOp (Not e)) =
  SNot <$> toScalExp' look e
toScalExp look (PrimOp (BinOp bop x y t))
  | t `elem` [Int, Bool] = -- XXX: Only integers and booleans, OK?
  binOpScalExp bop <*> toScalExp' look x <*> toScalExp' look y

toScalExp _ _ = Nothing

-- | If you have a scalar expression that has been created with
-- incomplete symbol table information, you can use this function to
-- grow its 'Id' leaves.
expandScalExp :: LookupVar -> ScalExp -> ScalExp
expandScalExp _ (Val v) = Val v
expandScalExp look (Id v) = fromMaybe (Id v) $ look v
expandScalExp look (SNeg se) = SNeg $ expandScalExp look se
expandScalExp look (SNot se) = SNot $ expandScalExp look se
expandScalExp look (MaxMin b ses) = MaxMin b $ map (expandScalExp look) ses
expandScalExp look (SPlus x y) = SPlus (expandScalExp look x) (expandScalExp look y)
expandScalExp look (SMinus x y) = SMinus (expandScalExp look x) (expandScalExp look y)
expandScalExp look (STimes x y) = STimes (expandScalExp look x) (expandScalExp look y)
expandScalExp look (SDivide x y) = SDivide (expandScalExp look x) (expandScalExp look y)
expandScalExp look (SPow x y) = SPow (expandScalExp look x) (expandScalExp look y)
expandScalExp look (SLogAnd x y) = SLogAnd (expandScalExp look x) (expandScalExp look y)
expandScalExp look (SLogOr x y) = SLogOr (expandScalExp look x) (expandScalExp look y)
expandScalExp look (RelExp relop x) = RelExp relop $ expandScalExp look x

-- | "Smart constructor" that checks whether we are subtracting zero,
-- and if so just returns the first argument.
sminus :: ScalExp -> ScalExp -> ScalExp
sminus x (Val (IntVal 0))  = x
sminus x (Val (RealVal 0)) = x
sminus x y                 = x `SMinus` y

-- | Take the product of a list of 'ScalExp's, or the integer @1@ if
-- the list is empty.
sproduct :: [ScalExp] -> ScalExp
sproduct []       = Val $ IntVal 1
sproduct (se:ses) = foldl STimes se ses

-- | Take the sum of a list of 'ScalExp's, or the integer @0@ if the
-- list is empty.
ssum :: [ScalExp] -> ScalExp
ssum []       = Val $ IntVal 0
ssum (se:ses) = foldl SPlus se ses

binOpScalExp :: BinOp -> Maybe (ScalExp -> ScalExp -> ScalExp)
binOpScalExp bop = liftM snd $ find ((==bop) . fst)
                   [ (Plus, SPlus)
                   , (Minus, SMinus)
                   , (Times, STimes)
                   , (Divide, SDivide)
                   , (Pow, SPow)
                   , (LogAnd, SLogAnd)
                   , (LogOr, SLogOr)
                   ]

toScalExp' :: LookupVar -> SubExp -> Maybe ScalExp
toScalExp' look (Var v) =
  look v <|> Just (Id v)
toScalExp' _ (Constant val) =
  Just $ Val val

fromScalExp :: (Proper lore, Bindable lore, MonadFreshNames m) =>
               ScalExp
            -> m (Exp lore, [Binding lore])
fromScalExp = runBinder'' . fromScalExp'

fromScalExp' :: MonadBinder m => ScalExp
             -> m (Exp (Lore m))
fromScalExp' = convert
  where convert (Val val) = return $ PrimOp $ SubExp $ Constant val
        convert (Id v)    = return $ PrimOp $ SubExp $ Var v
        convert (SNeg se) = eNegate $ convert se
        convert (SNot se) = eNot $ convert se
        convert (SPlus x y) = arithBinOp Plus x y
        convert (SMinus x y) = arithBinOp Minus x y
        convert (STimes x y) = arithBinOp Times x y
        convert (SDivide x y) = arithBinOp Divide x y
        convert (SPow x y) = arithBinOp Pow x y
        convert (SLogAnd x y) = eBinOp LogAnd (convert x) (convert y) Bool
        convert (SLogOr x y) = eBinOp LogOr (convert x) (convert y) Bool
        convert (RelExp LTH0 x) = eBinOp Less (convert x) (zero <$> scalExpType x) Bool
        convert (RelExp LEQ0 x) = eBinOp Leq (convert x) (zero <$> scalExpType x) Bool
        convert (MaxMin _ []) = fail "ScalExp.fromScalExp: MaxMin empty list"
        convert (MaxMin isMin (e:es)) = do
          e'  <- convert e
          es' <- mapM convert es
          foldM (select isMin) e' es'

        arithBinOp bop x y =
          eBinOp bop (convert x) (convert y) =<< scalExpType x

        select isMin cur next =
          let cmp = eBinOp Less (pure cur) (pure next) Bool
              (pick, discard)
                | isMin     = (cur, next)
                | otherwise = (next, cur)
          in eIf cmp (eBody [pure pick]) (eBody [pure discard])

        zero Int = PrimOp $ SubExp $ intconst 0
        zero _   = PrimOp $ SubExp $ constant (0::Double)

instance FreeIn ScalExp where
  freeIn (Val   _) = mempty
  freeIn (Id    i) = HS.singleton i
  freeIn (SNeg  e) = freeIn e
  freeIn (SNot  e) = freeIn e
  freeIn (SPlus x y)   = freeIn x <> freeIn y
  freeIn (SMinus x y)  = freeIn x <> freeIn y
  freeIn (SPow x y)    = freeIn x <> freeIn y
  freeIn (STimes x y)  = freeIn x <> freeIn y
  freeIn (SDivide x y) = freeIn x <> freeIn y
  freeIn (SLogOr x y)  = freeIn x <> freeIn y
  freeIn (SLogAnd x y) = freeIn x <> freeIn y
  freeIn (RelExp LTH0 e) = freeIn e
  freeIn (RelExp LEQ0 e) = freeIn e
  freeIn (MaxMin _  es) = mconcat $ map freeIn es
