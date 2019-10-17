{-# LANGUAGE FlexibleContexts #-}
module Futhark.Analysis.ScalExp
  ( RelOp0(..)
  , ScalExp(..)
  , scalExpType
  , scalExpSize
  , subExpToScalExp
  , toScalExp
  , expandScalExp
  , LookupVar
  , module Futhark.Representation.Primitive
  )
where

import Data.List
import Data.Maybe

import Futhark.Representation.Primitive hiding (SQuot, SRem, SDiv, SMod, SSignum)
import Futhark.Representation.AST hiding (SQuot, SRem, SDiv, SMod, SSignum)
import qualified Futhark.Representation.AST as AST
import Futhark.Transform.Substitute
import Futhark.Transform.Rename
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
  fromInteger = Val . IntValue . Int32Value . fromInteger -- probably not OK
  negate = SNeg

instance Pretty ScalExp where
  pprPrec _ (Val val) = ppr val
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
  pprPrec prec (RelExp LTH0 e) = ppBinOp prec "<" 2 2 e (0::Int)
  pprPrec prec (RelExp LEQ0 e) = ppBinOp prec "<=" 2 2 e (0::Int)
  pprPrec _ (MaxMin True es) = text "min" <> parens (commasep $ map ppr es)
  pprPrec _ (MaxMin False es) = text "max" <> parens (commasep $ map ppr es)

ppBinOp :: (Pretty a, Pretty b) => Int -> String -> Int -> Int -> a -> b -> Doc
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

-- | Number of nodes in the scalar expression.
scalExpSize :: ScalExp -> Int
scalExpSize Val{} = 1
scalExpSize Id{} = 1
scalExpSize (SNeg    e) = scalExpSize e
scalExpSize (SNot    e) = scalExpSize e
scalExpSize (SAbs    e) = scalExpSize e
scalExpSize (SSignum e) = scalExpSize e
scalExpSize (SPlus   x y) = scalExpSize x + scalExpSize y
scalExpSize (SMinus  x y) = scalExpSize x + scalExpSize y
scalExpSize (STimes  x y) = scalExpSize x + scalExpSize y
scalExpSize (SDiv x y) = scalExpSize x + scalExpSize y
scalExpSize (SMod x y)    = scalExpSize x + scalExpSize y
scalExpSize (SPow x y) = scalExpSize x + scalExpSize y
scalExpSize (SQuot x y) = scalExpSize x + scalExpSize y
scalExpSize (SRem x y) = scalExpSize x + scalExpSize y
scalExpSize (SLogAnd x y) = scalExpSize x + scalExpSize y
scalExpSize (SLogOr  x y) = scalExpSize x + scalExpSize y
scalExpSize (RelExp  _ x) = scalExpSize x
scalExpSize (MaxMin _ []) = 0
scalExpSize (MaxMin _ es) = sum $ map scalExpSize es

-- | A function that checks whether a variable name corresponds to a
-- scalar expression.
type LookupVar = VName -> Maybe ScalExp

-- | Non-recursively convert a subexpression to a 'ScalExp'.  The
-- (scalar) type of the subexpression must be given in advance.
subExpToScalExp :: SubExp -> PrimType -> ScalExp
subExpToScalExp (Var v) t        = Id v t
subExpToScalExp (Constant val) _ = Val val

toScalExp :: (HasScope t f, Monad f) =>
             LookupVar -> Exp lore -> f (Maybe ScalExp)
toScalExp look (BasicOp (SubExp (Var v)))
  | Just se <- look v =
    return $ Just se
  | otherwise = do
    t <- lookupType v
    case t of
      Prim bt | typeIsOK bt ->
        return $ Just $ Id v bt
      _ ->
        return Nothing
toScalExp _ (BasicOp (SubExp (Constant val)))
  | typeIsOK $ primValueType val =
    return $ Just $ Val val
toScalExp look (BasicOp (CmpOp (CmpSlt _) x y)) =
  Just . RelExp LTH0 <$> (sminus <$> subExpToScalExp' look x <*> subExpToScalExp' look y)
toScalExp look (BasicOp (CmpOp (CmpSle _) x y)) =
  Just . RelExp LEQ0 <$> (sminus <$> subExpToScalExp' look x <*> subExpToScalExp' look y)
toScalExp look (BasicOp (CmpOp (CmpEq t) x y))
  | typeIsOK t = do
  x' <- subExpToScalExp' look x
  y' <- subExpToScalExp' look y
  return $ Just $ case t of
    Bool ->
      SLogAnd x' y' `SLogOr` SLogAnd (SNot x') (SNot y')
    _ ->
      RelExp LEQ0 (x' `sminus` y') `SLogAnd` RelExp LEQ0 (y' `sminus` x')
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
typeIsOK = (`elem` Bool : map IntType allIntTypes)

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

-- | "Smart constructor" that checks whether we are subtracting zero,
-- and if so just returns the first argument.
sminus :: ScalExp -> ScalExp -> ScalExp
sminus x (Val v) | zeroIsh v = x
sminus x y = x `SMinus` y

 -- XXX: Only integers and booleans, OK?
binOpScalExp :: BinOp -> Maybe (ScalExp -> ScalExp -> ScalExp)
binOpScalExp bop = fmap snd . find ((==bop) . fst) $
                   concatMap intOps allIntTypes ++
                   [ (LogAnd, SLogAnd), (LogOr, SLogOr) ]
  where intOps t = [ (Add t, SPlus)
                   , (Sub t, SMinus)
                   , (Mul t, STimes)
                   , (AST.SDiv t, SDiv)
                   , (AST.Pow t, SPow)
                   , (AST.SMax t, \x y -> MaxMin False [x,y])
                   , (AST.SMin t, \x y -> MaxMin True [x,y])
                   ]

instance FreeIn ScalExp where
  freeIn' (Val   _) = mempty
  freeIn' (Id i _) = fvName i
  freeIn' (SNeg  e) = freeIn' e
  freeIn' (SNot  e) = freeIn' e
  freeIn' (SAbs  e) = freeIn' e
  freeIn' (SSignum e) = freeIn' e
  freeIn' (SPlus x y) = freeIn' x <> freeIn' y
  freeIn' (SMinus x y) = freeIn' x <> freeIn' y
  freeIn' (SPow x y) = freeIn' x <> freeIn' y
  freeIn' (STimes x y) = freeIn' x <> freeIn' y
  freeIn' (SDiv x y) = freeIn' x <> freeIn' y
  freeIn' (SMod x y) = freeIn' x <> freeIn' y
  freeIn' (SQuot x y) = freeIn' x <> freeIn' y
  freeIn' (SRem x y) = freeIn' x <> freeIn' y
  freeIn' (SLogOr x y)  = freeIn' x <> freeIn' y
  freeIn' (SLogAnd x y) = freeIn' x <> freeIn' y
  freeIn' (RelExp LTH0 e) = freeIn' e
  freeIn' (RelExp LEQ0 e) = freeIn' e
  freeIn' (MaxMin _  es) = freeIn' es
