{-# LANGUAGE FlexibleContexts #-}
module Futhark.Analysis.ScalExp
  ( RelOp0(..)
  , ScalExp(..)
  , scalExpType
  , subExpToScalExp
  , intSubExpToScalExp
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

import Prelude

import Text.PrettyPrint.Mainland hiding (pretty)

import Futhark.Representation.AST
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
            | Id      VName BasicType
            | SNeg    ScalExp
            | SNot    ScalExp
            | SAbs    ScalExp
            | SSignum ScalExp
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

instance Num ScalExp where
  0 + y = y
  x + 0 = x
  x + y = SPlus x y

  x - 0 = x
  x - y = SMinus x y

  0 * _ = 0
  _ * 0 = 0
  1 * y = y
  y * 1 = y
  x * y = STimes x y

  abs = SAbs
  signum = SSignum
  fromInteger = Val . IntVal . fromInteger
  negate = SNeg

instance Fractional ScalExp where
  0 / _ = 0
  x / 1 = x
  x / y = SDivide x y
  fromRational = Val . Float64Val . fromRational

instance Pretty ScalExp where
  pprPrec _ (Val val) = ppr $ BasicVal val
  pprPrec _ (Id v _) = ppr v
  pprPrec _ (SNeg e) = text "-" <> pprPrec 9 e
  pprPrec _ (SNot e) = text "not" <+> pprPrec 9 e
  pprPrec _ (SAbs e) = text "abs" <+> pprPrec 9 e
  pprPrec _ (SSignum e) = text "signum" <+> pprPrec 9 e
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
    case e of Id v t -> Id (substituteNames subst v) t
              Val v -> Val v
              SNeg x -> SNeg $ substituteNames subst x
              SNot x -> SNot $ substituteNames subst x
              SAbs x -> SAbs $ substituteNames subst x
              SSignum x -> SSignum $ substituteNames subst x
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

scalExpType :: ScalExp -> BasicType
scalExpType (Val v) = basicValueType v
scalExpType (Id _ t) = t
scalExpType (SNeg    e) = scalExpType e
scalExpType (SNot    _) = Bool
scalExpType (SAbs    e) = scalExpType e
scalExpType (SSignum _) = Int
scalExpType (SPlus   e _) = scalExpType e
scalExpType (SMinus  e _) = scalExpType e
scalExpType (STimes  e _) = scalExpType e
scalExpType (SDivide e _) = scalExpType e
scalExpType (SPow    e _) = scalExpType e
scalExpType (SLogAnd _ _) = Bool
scalExpType (SLogOr  _ _) = Bool
scalExpType (RelExp  _ _) = Bool
scalExpType (MaxMin _ []) = Int
scalExpType (MaxMin _ (e:_)) = scalExpType e

-- | A function that checks whether a variable name corresponds to a
-- scalar expression.
type LookupVar = VName -> Maybe ScalExp

-- | Non-recursively convert a subexpression to a 'ScalExp'.  The
-- (scalar) type of the subexpression must be given in advance.
subExpToScalExp :: SubExp -> BasicType -> ScalExp
subExpToScalExp (Var v) t        = Id v t
subExpToScalExp (Constant val) _ = Val val

-- | Non-recursively convert an integral subexpression to a 'ScalExp'.
intSubExpToScalExp :: SubExp -> ScalExp
intSubExpToScalExp se = subExpToScalExp se Int

toScalExp :: (HasTypeEnv f, Monad f) =>
             LookupVar -> Exp lore -> f (Maybe ScalExp)
toScalExp look (PrimOp (SubExp (Var v)))
  | Just se <- look v =
    return $ Just se
  | otherwise = do
    t <- lookupType v
    case t of
      Basic bt -> return $ Just $ Id v bt
      _        -> return Nothing
toScalExp _ (PrimOp (SubExp (Constant val))) =
  return $ Just $ Val val
toScalExp look (PrimOp (BinOp Less x y _)) =
  Just <$> RelExp LTH0 <$> (sminus <$> subExpToScalExp' look x <*> subExpToScalExp' look y)
toScalExp look (PrimOp (BinOp Leq x y _)) =
  Just <$> RelExp LEQ0 <$> (sminus <$> subExpToScalExp' look x <*> subExpToScalExp' look y)
toScalExp look (PrimOp (BinOp Equal x y Int)) = do
  x' <- subExpToScalExp' look x
  y' <- subExpToScalExp' look y
  return $ Just $ RelExp LEQ0 (x' `sminus` y') `SLogAnd` RelExp LEQ0 (y' `sminus` x')
toScalExp look (PrimOp (Negate e)) =
  Just <$> SNeg <$> subExpToScalExp' look e
toScalExp look (PrimOp (Not e)) =
  Just <$> SNot <$> subExpToScalExp' look e
toScalExp look (PrimOp (BinOp bop x y t))
  | t `elem` [Int, Bool],
    Just f <- binOpScalExp bop = -- XXX: Only integers and booleans, OK?
  Just <$> (f <$> subExpToScalExp' look x <*> subExpToScalExp' look y)

toScalExp _ _ = return Nothing

subExpToScalExp' :: HasTypeEnv f => LookupVar -> SubExp -> f ScalExp
subExpToScalExp' look (Var v)
  | Just se <- look v =
    pure se
  | otherwise =
    withType <$> lookupType v
    where withType (Basic t) =
            subExpToScalExp (Var v) t
          withType t =
            error $ "Cannot create ScalExp from variable " ++ pretty v ++
            " of type " ++ pretty t
subExpToScalExp' _ (Constant val) =
  pure $ Val val

-- | If you have a scalar expression that has been created with
-- incomplete symbol table information, you can use this function to
-- grow its 'Id' leaves.
expandScalExp :: LookupVar -> ScalExp -> ScalExp
expandScalExp _ (Val v) = Val v
expandScalExp look (Id v t) = fromMaybe (Id v t) $ look v
expandScalExp look (SNeg se) = SNeg $ expandScalExp look se
expandScalExp look (SNot se) = SNot $ expandScalExp look se
expandScalExp look (SAbs se) = SAbs $ expandScalExp look se
expandScalExp look (SSignum se) = SSignum $ expandScalExp look se
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
sminus x (Val (Float32Val 0)) = x
sminus x (Val (Float64Val 0)) = x
sminus x y = x `SMinus` y

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
                   , (IntDivide, SDivide)
                   , (Pow, SPow)
                   , (LogAnd, SLogAnd)
                   , (LogOr, SLogOr)
                   ]

fromScalExp :: MonadBinder m =>
               ScalExp
            -> m (Exp (Lore m), [Binding (Lore m)])
fromScalExp = collectBindings . fromScalExp'

fromScalExp' :: MonadBinder m => ScalExp
             -> m (Exp (Lore m))
fromScalExp' = convert
  where convert (Val val) = return $ PrimOp $ SubExp $ Constant val
        convert (Id v _)  = return $ PrimOp $ SubExp $ Var v
        convert (SNeg se) = eNegate $ convert se
        convert (SNot se) = eNot $ convert se
        convert (SAbs se) = eAbs $ convert se
        convert (SSignum se) = eSignum $ convert se
        convert (SPlus x y) = arithBinOp Plus x y
        convert (SMinus x y) = arithBinOp Minus x y
        convert (STimes x y) = arithBinOp Times x y
        convert (SDivide x y)
          | scalExpType x == Int = arithBinOp IntDivide x y
          | otherwise            = arithBinOp Divide x y
        convert (SPow x y) = arithBinOp Pow x y
        convert (SLogAnd x y) = eBinOp LogAnd (convert x) (convert y) Bool
        convert (SLogOr x y) = eBinOp LogOr (convert x) (convert y) Bool
        convert (RelExp LTH0 x) = eBinOp Less (convert x) (pure $ zero $ scalExpType x) Bool
        convert (RelExp LEQ0 x) = eBinOp Leq (convert x) (pure $ zero $ scalExpType x) Bool
        convert (MaxMin _ []) = fail "ScalExp.fromScalExp: MaxMin empty list"
        convert (MaxMin isMin (e:es)) = do
          e'  <- convert e
          es' <- mapM convert es
          foldM (select isMin) e' es'

        arithBinOp bop x y =
          eBinOp bop (convert x) (convert y) $ scalExpType x

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
  freeIn (Id i _)  = HS.singleton i
  freeIn (SNeg  e) = freeIn e
  freeIn (SNot  e) = freeIn e
  freeIn (SAbs  e) = freeIn e
  freeIn (SSignum e) = freeIn e
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
