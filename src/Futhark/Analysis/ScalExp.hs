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
  , sproduct
  , ssum
  , freeIn
  , module Futhark.Representation.Primitive
  )
where

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.HashSet as HS
import Data.Maybe
import Data.Monoid

import Prelude

import Futhark.Representation.Primitive hiding (SQuot, SRem, SDiv, SMod, SSignum)
import Futhark.Representation.AST hiding (SQuot, SRem, SDiv, SMod, SSignum)
import qualified Futhark.Representation.AST as AST
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
import Futhark.Construct
import Futhark.Util.IntegralExp
import Futhark.Util.Pretty hiding (pretty)

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
data ScalExp= Val     PrimValue
            | Id      VName PrimType
            | SNeg    ScalExp
            | SNot    ScalExp
            | SAbs    ScalExp
            | SSignum ScalExp
            | SPlus   ScalExp ScalExp
            | SMinus  ScalExp ScalExp
            | STimes  ScalExp ScalExp
            | SPow    ScalExp ScalExp
            | SDiv ScalExp ScalExp
            | SMod    ScalExp ScalExp
            | SQuot   ScalExp ScalExp
            | SRem    ScalExp ScalExp
            | MaxMin  Bool   [ScalExp]
            | RelExp  RelOp0  ScalExp
            | SLogAnd ScalExp ScalExp
            | SLogOr  ScalExp ScalExp

            | SOneIfZero ScalExp
            | SIfZero ScalExp ScalExp ScalExp
            | SIfLessThan ScalExp ScalExp ScalExp ScalExp
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
  fromInteger = Val . IntValue . Int32Value . fromInteger
  negate = SNeg

instance IntegralExp ScalExp where
  quot = SQuot
  rem = SRem
  div = SDiv
  mod = SMod

instance IntegralCond ScalExp where
  ifZero = SIfZero
  ifLessThan = SIfLessThan
  oneIfZero = SOneIfZero

instance Pretty ScalExp where
  pprPrec _ (Val val) = ppr $ PrimVal val
  pprPrec _ (Id v _) = ppr v
  pprPrec _ (SNeg e) = text "-" <> pprPrec 9 e
  pprPrec _ (SNot e) = text "not" <+> pprPrec 9 e
  pprPrec _ (SAbs e) = text "abs" <+> pprPrec 9 e
  pprPrec _ (SSignum e) = text "signum" <+> pprPrec 9 e
  pprPrec prec (SPlus x y) = ppBinOp prec "+" 4 4 x y
  pprPrec prec (SMinus x y) = ppBinOp prec "-" 4 10 x y
  pprPrec prec (SPow x y) = ppBinOp prec "^" 6 6 x y
  pprPrec prec (STimes x y) = ppBinOp prec "*" 5 5 x y
  pprPrec prec (SDiv x y) = ppBinOp prec "/" 5 10 x y
  pprPrec prec (SMod x y) = ppBinOp prec "%" 5 10 x y
  pprPrec prec (SQuot x y) = ppBinOp prec "//" 5 10 x y
  pprPrec prec (SRem x y) = ppBinOp prec "%%" 5 10 x y
  pprPrec prec (SLogOr x y) = ppBinOp prec "||" 0 0 x y
  pprPrec prec (SLogAnd x y) = ppBinOp prec "&&" 1 1 x y
  pprPrec prec (RelExp LTH0 e) = ppBinOp prec "<" 2 2 e 0
  pprPrec prec (RelExp LEQ0 e) = ppBinOp prec "<=" 2 2 e 0
  pprPrec _ (MaxMin True es) = text "min" <> parens (commasep $ map ppr es)
  pprPrec _ (MaxMin False es) = text "max" <> parens (commasep $ map ppr es)
  pprPrec prec (SIfLessThan a b t f) =
    parensIf (prec >= 0) $
    ppr a <+> text "<" <+> ppr b <+> text "?" <+> ppr t <+> text ":" <+> ppr f
  pprPrec prec (SIfZero x t f) =
    parensIf (prec >= 0) $
    ppr x <+> text "== 0" <+> ppr t <+> text ":" <+> ppr f
  pprPrec _ (SOneIfZero x) =
    text "oneIfZero" <> parens (ppr x)

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
              SDiv x y -> substituteNames subst x `SDiv` substituteNames subst y
              SMod x y -> substituteNames subst x `SMod` substituteNames subst y
              SQuot x y -> substituteNames subst x `SDiv` substituteNames subst y
              SRem x y -> substituteNames subst x `SRem` substituteNames subst y
              MaxMin m es -> MaxMin m $ map (substituteNames subst) es
              RelExp r x -> RelExp r $ substituteNames subst x
              SLogAnd x y -> substituteNames subst x `SLogAnd` substituteNames subst y
              SLogOr x y -> substituteNames subst x `SLogOr` substituteNames subst y
              SOneIfZero x -> SOneIfZero $ substituteNames subst x
              SIfZero x t f -> SIfZero
                               (substituteNames subst x)
                               (substituteNames subst t)
                               (substituteNames subst f)
              SIfLessThan a b t f -> SIfLessThan
                                     (substituteNames subst a)
                                     (substituteNames subst b)
                                     (substituteNames subst t)
                                     (substituteNames subst f)

instance Rename ScalExp where
  rename = substituteRename

scalExpType :: ScalExp -> PrimType
scalExpType (Val v) = primValueType v
scalExpType (Id _ t) = t
scalExpType (SNeg    e) = scalExpType e
scalExpType (SNot    _) = Bool
scalExpType (SAbs    e) = scalExpType e
scalExpType (SSignum e) = scalExpType e
scalExpType (SPlus   e _) = scalExpType e
scalExpType (SMinus  e _) = scalExpType e
scalExpType (STimes  e _) = scalExpType e
scalExpType (SDiv e _) = scalExpType e
scalExpType (SMod e _)    = scalExpType e
scalExpType (SPow e _) = scalExpType e
scalExpType (SQuot e _) = scalExpType e
scalExpType (SRem e _) = scalExpType e
scalExpType (SLogAnd _ _) = Bool
scalExpType (SLogOr  _ _) = Bool
scalExpType (RelExp  _ _) = Bool
scalExpType (MaxMin _ []) = IntType Int32 -- arbitrary and probably wrong.
scalExpType (MaxMin _ (e:_)) = scalExpType e
scalExpType (SOneIfZero e) = scalExpType e
scalExpType (SIfZero _ t _) = scalExpType t
scalExpType (SIfLessThan _ _ t _) = scalExpType t

-- | A function that checks whether a variable name corresponds to a
-- scalar expression.
type LookupVar = VName -> Maybe ScalExp

-- | Non-recursively convert a subexpression to a 'ScalExp'.  The
-- (scalar) type of the subexpression must be given in advance.
subExpToScalExp :: SubExp -> PrimType -> ScalExp
subExpToScalExp (Var v) t        = Id v t
subExpToScalExp (Constant val) _ = Val val

-- | Non-recursively convert an integral subexpression to a 'ScalExp'.
intSubExpToScalExp :: SubExp -> ScalExp
intSubExpToScalExp se = subExpToScalExp se $ IntType Int32

toScalExp :: (HasScope t f, Monad f) =>
             LookupVar -> Exp lore -> f (Maybe ScalExp)
toScalExp look (BasicOp (SubExp (Var v)))
  | Just se <- look v =
    return $ Just se
  | otherwise = do
    t <- lookupType v
    case t of
      Prim bt | bt `elem` [Bool, int32] ->
        return $ Just $ Id v bt
      _ ->
        return Nothing
toScalExp _ (BasicOp (SubExp (Constant val)))
  | typeIsOK $ primValueType val =
    return $ Just $ Val val
toScalExp look (BasicOp (CmpOp (CmpSlt t) x y))
  | typeIsOK $ IntType t =
  Just . RelExp LTH0 <$> (sminus <$> subExpToScalExp' look x <*> subExpToScalExp' look y)
toScalExp look (BasicOp (CmpOp (CmpSle t) x y))
  | typeIsOK $ IntType t =
  Just . RelExp LEQ0 <$> (sminus <$> subExpToScalExp' look x <*> subExpToScalExp' look y)
toScalExp look (BasicOp (CmpOp (CmpEq t) x y))
  | typeIsOK t = do
  x' <- subExpToScalExp' look x
  y' <- subExpToScalExp' look y
  return $ Just $ RelExp LEQ0 (x' `sminus` y') `SLogAnd` RelExp LEQ0 (y' `sminus` x')
toScalExp look (BasicOp (BinOp (Sub t) (Constant x) y))
  | typeIsOK $ IntType t, zeroIsh x =
  Just . SNeg <$> subExpToScalExp' look y
toScalExp look (BasicOp (UnOp AST.Not e)) =
  Just . SNot <$> subExpToScalExp' look e
toScalExp look (BasicOp (BinOp bop x y))
  | Just f <- binOpScalExp bop =
  Just <$> (f <$> subExpToScalExp' look x <*> subExpToScalExp' look y)

toScalExp _ _ = return Nothing

typeIsOK :: PrimType -> Bool
typeIsOK = (`elem` [Bool, int32])

subExpToScalExp' :: HasScope t f =>
                    LookupVar -> SubExp -> f ScalExp
subExpToScalExp' look (Var v)
  | Just se <- look v =
    pure se
  | otherwise =
    withType <$> lookupType v
    where withType (Prim t) =
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
expandScalExp look (SDiv x y) = SDiv (expandScalExp look x) (expandScalExp look y)
expandScalExp look (SMod x y) = SMod (expandScalExp look x) (expandScalExp look y)
expandScalExp look (SQuot x y) = SQuot (expandScalExp look x) (expandScalExp look y)
expandScalExp look (SRem x y) = SRem (expandScalExp look x) (expandScalExp look y)
expandScalExp look (SPow x y) = SPow (expandScalExp look x) (expandScalExp look y)
expandScalExp look (SLogAnd x y) = SLogAnd (expandScalExp look x) (expandScalExp look y)
expandScalExp look (SLogOr x y) = SLogOr (expandScalExp look x) (expandScalExp look y)
expandScalExp look (RelExp relop x) = RelExp relop $ expandScalExp look x
expandScalExp look (SOneIfZero x) = SOneIfZero $ expandScalExp look x
expandScalExp look (SIfZero x t f) = SIfZero
                                     (expandScalExp look x)
                                     (expandScalExp look t)
                                     (expandScalExp look f)
expandScalExp look (SIfLessThan a b t f) = SIfLessThan
                                           (expandScalExp look a)
                                           (expandScalExp look b)
                                           (expandScalExp look t)
                                           (expandScalExp look f)

-- | "Smart constructor" that checks whether we are subtracting zero,
-- and if so just returns the first argument.
sminus :: ScalExp -> ScalExp -> ScalExp
sminus x (Val v) | zeroIsh v = x
sminus x y = x `SMinus` y

-- | Take the product of a list of 'ScalExp's, or the integer @1@ if
-- the list is empty.
sproduct :: [ScalExp] -> ScalExp
sproduct []       = Val $ IntValue $ Int32Value 1
sproduct (se:ses) = foldl STimes se ses

-- | Take the sum of a list of 'ScalExp's, or the integer @0@ if the
-- list is empty.
ssum :: [ScalExp] -> ScalExp
ssum []       = Val $ IntValue $ Int32Value 0
ssum (se:ses) = foldl SPlus se ses

 -- XXX: Only integers and booleans, OK?
binOpScalExp :: BinOp -> Maybe (ScalExp -> ScalExp -> ScalExp)
binOpScalExp bop = snd <$> find ((==bop) . fst)
                   [ (Add Int32, SPlus)
                   , (Sub Int32, SMinus)
                   , (Mul Int32, STimes)
                   , (AST.SDiv Int32, SDiv)
                   , (AST.Pow Int32, SPow)
                   , (LogAnd, SLogAnd)
                   , (LogOr, SLogOr)
                   ]

fromScalExp :: MonadBinder m => ScalExp
             -> m (Exp (Lore m))
fromScalExp = convert
  where convert (Val val) = return $ BasicOp $ SubExp $ Constant val
        convert (Id v _)  = return $ BasicOp $ SubExp $ Var v
        convert (SNeg se) = eNegate $ convert se
        convert (SNot se) = eNot $ convert se
        convert (SAbs se) = eAbs $ convert se
        convert (SSignum se) = eSignum $ convert se
        convert (SPlus x y) = arithBinOp (Add Int32) x y
        convert (SMinus x y) = arithBinOp (Sub Int32) x y
        convert (STimes x y) = arithBinOp (Mul Int32) x y
        convert (SDiv x y)  = arithBinOp (AST.SDiv Int32) x y
        convert (SMod x y) = arithBinOp (AST.SMod Int32) x y
        convert (SQuot x y) = arithBinOp (AST.SQuot Int32) x y
        convert (SRem x y) = arithBinOp (AST.SRem Int32) x y
        convert (SPow x y) = arithBinOp (AST.Pow Int32) x y
        convert (SLogAnd x y) = eBinOp LogAnd (convert x) (convert y)
        convert (SLogOr x y) = eBinOp LogOr (convert x) (convert y)
        convert (RelExp LTH0 x) = eCmpOp (CmpSlt Int32) (convert x) (pure zero)
        convert (RelExp LEQ0 x) = eCmpOp (CmpSle Int32) (convert x) (pure zero)
        convert (MaxMin _ []) = fail "ScalExp.fromScalExp: MaxMin empty list"
        convert (MaxMin isMin (e:es)) = do
          e'  <- convert e
          es' <- mapM convert es
          foldM (select isMin) e' es'
        convert (SOneIfZero e) = do
          e' <- letSubExp "one_if_zero_arg" =<< convert e
          eIf
            (eCmpOp (CmpEq int32) (eSubExp e') (pure zero))
            (eBody [eSubExp $ intConst Int32 1])
            (eBody [eSubExp e'])
        convert (SIfZero x t f) =
          eIf
          (eCmpOp (CmpEq int32) (convert x) (pure zero))
          (eBody [convert t])
          (eBody [convert f])
        convert (SIfLessThan a b t f) =
          eIf
          (eCmpOp (CmpSlt Int32) (convert a) (convert b))
          (eBody [convert t])
          (eBody [convert f])

        arithBinOp bop x y =
          eBinOp bop (convert x) (convert y)

        select isMin cur next =
          let cmp = eCmpOp (CmpSlt Int32) (pure cur) (pure next)
              (pick, discard)
                | isMin     = (cur, next)
                | otherwise = (next, cur)
          in eIf cmp (eBody [pure pick]) (eBody [pure discard])

        zero = BasicOp $ SubExp $ intConst Int32 0

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
  freeIn (SDiv x y) = freeIn x <> freeIn y
  freeIn (SMod x y) = freeIn x <> freeIn y
  freeIn (SQuot x y) = freeIn x <> freeIn y
  freeIn (SRem x y) = freeIn x <> freeIn y
  freeIn (SLogOr x y)  = freeIn x <> freeIn y
  freeIn (SLogAnd x y) = freeIn x <> freeIn y
  freeIn (RelExp LTH0 e) = freeIn e
  freeIn (RelExp LEQ0 e) = freeIn e
  freeIn (MaxMin _  es) = mconcat $ map freeIn es
  freeIn (SOneIfZero e) = freeIn e
  freeIn (SIfZero x t f) = freeIn x <> freeIn t <> freeIn f
  freeIn (SIfLessThan a b t f) = freeIn a <> freeIn b <> freeIn t <> freeIn f
