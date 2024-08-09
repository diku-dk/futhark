{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

-- | Translating to-and-from PrimExp to the sum-of-product representation.
module Futhark.SoP.Convert
  ( FromSoP (..),
    ToSoP (..),
    toSoPNum_,
    toSoPCmp_,
  )
where

import Control.Monad.State
import Data.List (find)
import Data.Set (Set)
import Data.Set qualified as S
import Futhark.Analysis.PrimExp (PrimExp, PrimType, (~*~), (~+~), (~-~), (~/~), (~==~))
import Futhark.Analysis.PrimExp qualified as PE
import Futhark.SoP.Monad
import Futhark.SoP.SoP
import Futhark.SoP.Util
import Futhark.Util.Pretty
import Language.Futhark.Core
import Language.Futhark.Prop
import Language.Futhark.Syntax (VName)
import Language.Futhark.Syntax qualified as E

toSoPNum_ :: (ToSoP u e, MonadSoP u e m) => e -> m (SoP u)
toSoPNum_ e = snd <$> toSoPNum e

toSoPCmp_ :: (ToSoP u e, MonadSoP u e m) => e -> m (SoP u >= 0)
toSoPCmp_ e = snd <$> toSoPNum e

-- | Conversion from 'SoP's to other representations.
class FromSoP u e where
  fromSoP :: MonadSoP u e m => SoP u -> m e

-- instance Ord u => FromSoP u (PrimExp u) where
--  fromSoP sop =
--    foldr ((~+~) . fromTerm) (PE.ValueExp $ PE.IntValue $ PE.intValue PE.Int64 (0 :: Integer)) (sopToLists sop)
--    where
--      fromTerm (term, n) =
--        foldl (~*~) (PE.ValueExp $ PE.IntValue $ PE.intValue PE.Int64 n) $
--          map fromSym term
--      fromSym sym = PE.LeafExp sym $ PE.IntType PE.Int64

-- | Conversion from some expressions to
--   'SoP's. Monadic because it may involve look-ups in the
--   untranslatable expression environment.
class ToSoP u e where
  toSoPNum :: MonadSoP u e m => e -> m (Integer, SoP u)

instance (Nameable u, Ord u, Show u, Pretty u) => ToSoP u Integer where
  toSoPNum x = pure (1, int2SoP x)

instance (Nameable u, Ord u, Show u, Pretty u) => ToSoP u (PrimExp u) where
  toSoPNum primExp = do
    (f, sop) <- toSoPNum' 1 primExp
    pure (abs f, signum f `scaleSoP` sop)
    where
      notIntType :: PrimType -> Bool
      notIntType (PE.IntType _) = False
      notIntType _ = True

      divideIsh :: PE.BinOp -> Bool
      divideIsh (PE.UDiv _ _) = True
      divideIsh (PE.UDivUp _ _) = True
      divideIsh (PE.SDiv _ _) = True
      divideIsh (PE.SDivUp _ _) = True
      divideIsh (PE.FDiv _) = True
      divideIsh _ = False
      toSoPNum' _ pe
        | notIntType (PE.primExpType pe) =
            error "toSoPNum' applied to a PrimExp whose prim type is not Integer"
      toSoPNum' f (PE.LeafExp vnm _) =
        pure (f, sym2SoP vnm)
      toSoPNum' f (PE.ValueExp (PE.IntValue iv)) =
        pure (1, int2SoP $ getIntVal iv `div` f)
        where
          getIntVal :: PE.IntValue -> Integer
          getIntVal (PE.Int8Value v) = fromIntegral v
          getIntVal (PE.Int16Value v) = fromIntegral v
          getIntVal (PE.Int32Value v) = fromIntegral v
          getIntVal (PE.Int64Value v) = fromIntegral v
      toSoPNum' f (PE.UnOpExp PE.Complement {} x) = do
        (f', x_sop) <- toSoPNum' f x
        pure (f', negSoP x_sop)
      toSoPNum' f (PE.BinOpExp PE.Add {} x y) = do
        (x_f, x_sop) <- toSoPNum x
        (y_f, y_sop) <- toSoPNum y
        let l_c_m = lcm x_f y_f
            (x_m, y_m) = (l_c_m `div` x_f, l_c_m `div` y_f)
            x_sop' = mulSoPs (int2SoP x_m) x_sop
            y_sop' = mulSoPs (int2SoP y_m) y_sop
        pure (f * l_c_m, addSoPs x_sop' y_sop')
      toSoPNum' f (PE.BinOpExp PE.Sub {} x y) = do
        (x_f, x_sop) <- toSoPNum x
        (y_f, y_sop) <- toSoPNum y
        let l_c_m = lcm x_f y_f
            (x_m, y_m) = (l_c_m `div` x_f, l_c_m `div` y_f)
            x_sop' = mulSoPs (int2SoP x_m) x_sop
            n_y_sop' = mulSoPs (int2SoP (-y_m)) y_sop
        pure (f * l_c_m, addSoPs x_sop' n_y_sop')
      toSoPNum' f pe@(PE.BinOpExp PE.Mul {} x y) = do
        (x_f, x_sop) <- toSoPNum x
        (y_f, y_sop) <- toSoPNum y
        case (x_f, y_f) of
          (1, 1) -> pure (f, mulSoPs x_sop y_sop)
          _ -> do
            x' <- lookupUntransPE pe
            toSoPNum' f $ PE.LeafExp x' $ PE.primExpType pe
      -- pe / 1 == pe
      toSoPNum' f (PE.BinOpExp divish pe q)
        | divideIsh divish && PE.oneIshExp q =
            toSoPNum' f pe
      -- evaluate `val_x / val_y`
      toSoPNum' f (PE.BinOpExp divish x y)
        | divideIsh divish,
          PE.ValueExp v_x <- x,
          PE.ValueExp v_y <- y = do
            let f' = v_x `vdiv` v_y
            toSoPNum' f $ PE.ValueExp f'
        -- Trivial simplifications:
        -- (y * v) / y = v and (u * y) / y = u
        | divideIsh divish,
          PE.BinOpExp (PE.Mul _ _) u v <- x,
          (is_fst, is_snd) <- (u == y, v == y),
          is_fst || is_snd = do
            toSoPNum' f $ if is_fst then v else u
        where
          vdiv (PE.IntValue (PE.Int64Value x')) (PE.IntValue (PE.Int64Value y')) =
            PE.IntValue $ PE.Int64Value (x' `div` y')
          vdiv (PE.IntValue (PE.Int32Value x')) (PE.IntValue (PE.Int32Value y')) =
            PE.IntValue $ PE.Int32Value (x' `div` y')
          vdiv (PE.IntValue (PE.Int16Value x')) (PE.IntValue (PE.Int16Value y')) =
            PE.IntValue $ PE.Int16Value (x' `div` y')
          vdiv (PE.IntValue (PE.Int8Value x')) (PE.IntValue (PE.Int8Value y')) =
            PE.IntValue $ PE.Int8Value (x' `div` y')
          --    vdiv (FloatValue (Float32Value x)) (FloatValue (Float32Value y)) =
          --      FloatValue $ Float32Value $ x / y
          --    vdiv (FloatValue (Float64Value x)) (FloatValue (Float64Value y)) =
          --      FloatValue $ Float64Value $ x / y
          vdiv _ _ = error "In vdiv: illegal type for division!"
      -- try heuristic for exact division
      toSoPNum' f pe@(PE.BinOpExp divish x y)
        | divideIsh divish = do
            (x_f, x_sop) <- toSoPNum x
            (y_f, y_sop) <- toSoPNum y
            case (x_f, y_f, divSoPs x_sop y_sop) of
              (1, 1, Just res) -> pure (f, res)
              _ -> do
                x' <- lookupUntransPE pe
                toSoPNum' f $ PE.LeafExp x' $ PE.primExpType pe
      --  Anything that is not handled by specific cases of toSoPNum'
      --   is handled by this default procedure:
      --    If the target `pe` is in the unknwon `env`
      --    Then return thecorresponding binding
      --    Else make a fresh symbol `v`, bind it in the environment
      --         and return it.
      toSoPNum' f pe = do
        x <- lookupUntransPE pe
        toSoPNum' f $ PE.LeafExp x $ PE.primExpType pe

instance ToSoP VName Exp where
  toSoPNum (E.Literal v _) =
    (pure . (1,)) $
      case v of
        E.SignedValue x -> int2SoP $ PE.valueIntegral x
        E.UnsignedValue x -> int2SoP $ PE.valueIntegral x
        _ -> error ""
  toSoPNum (E.IntLit v _ _) = pure (1, int2SoP v)
  toSoPNum (E.Var (E.QualName [] v) _ _) = pure (1, sym2SoP v)
  toSoPNum e@(E.AppExp (E.BinOp (op, _) _ (e_x, _) (e_y, _) _) _)
    | E.baseTag (E.qualLeaf op) <= maxIntrinsicTag,
      name <- E.baseString $ E.qualLeaf op,
      Just bop <- find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
        (_, x) <- toSoPNum e_x
        (_, y) <- toSoPNum e_y
        (1,)
          <$> case bop of
            E.Plus -> pure $ x .+. y
            E.Minus -> pure $ x .-. y
            E.Times -> pure $ x .*. y
            _ -> sym2SoP <$> lookupUntransPE e
  toSoPNum e = do
    x <- lookupUntransPE e
    pure (1, sym2SoP x)

--
-- {--
---- This is a more refined treatment, but probably
----   an overkill (harmful if you get the type wrong)
-- fromSym unknowns sym
--  | Nothing <- M.lookup sym (dir unknowns) =
--      LeafExp sym $ IntType Integer
--  | Just pe1 <- M.lookup sym (dir unknowns),
--    IntType Integer <- PE.primExpType pe1 =
--      pe1
-- fromSym unknowns sym =
--  error ("Type error in fromSym: type of " ++
--          show sym ++ " is not Integer")
----}
