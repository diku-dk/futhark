{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

-- | Translating to-and-from PrimExp to the sum-of-product representation.
module Futhark.SoP.ToFromSoP
  ( toNumSoP,
    toNumSoPCmp,
    fromNumSoP,
    FromSoP (..),
    ToSoPM (..),
  )
where

import Control.Monad.State
import Data.Set (Set)
import Data.Set qualified as S
import Futhark.Analysis.PrimExp
import Futhark.SoP.AlgEnv
import Futhark.SoP.PrimExp
import Futhark.SoP.SoP

-- | Conversion from (structures of) 'SoP's to other representations
--   (e.g., 'PrimExp's).
class FromSoP a b where
  fromSoP :: a -> b

instance FromSoP (SoP u) (SoP u) where
  fromSoP = id

instance Ord u => FromSoP (SoP u) (PrimExp u) where
  fromSoP = fromNumSoP

instance (Functor t, FromSoP a b) => FromSoP (t a) (t b) where
  fromSoP = fmap fromSoP

instance {-# OVERLAPS #-} (Ord b, FromSoP a b) => FromSoP (Set a) (Set b) where
  fromSoP = S.fromList . fromSoP . S.toList

-- | Conversion from (strctures of) some expressions to
--   'SoP's. Monadic because it may involve look-ups in the
--   untranslatable expression environment.
class Monad m => ToSoPM m a b where
  toSoP :: a -> m b

instance (Nameable u, Ord u) => ToSoPM (AlgM u) (PrimExp u) (Integer, SoP u) where
  toSoP = toNumSoP

instance (Traversable t, ToSoPM m a b) => ToSoPM m (t a) (t b) where
  toSoP = traverse toSoP

instance {-# OVERLAPS #-} (Ord b, ToSoPM m a b) => ToSoPM m (Set a) (Set b) where
  toSoP = fmap S.fromList . toSoP . S.toList

-- | Translates 'PrimExp's to a 'SoP' representation, scaled by the
--   returned integer.
--
--   TODO: please extend to return also an integral
--   quotient, e.g., in order to support, e.g., @i <= (n+1)/16 + 3@.
toNumSoP :: (Ord u, Nameable u) => PrimExp u -> AlgM u (Integer, SoP u)
toNumSoP primExp = do
  (f, sop) <- toNumSoP' 1 primExp
  pure (abs f, signum f `scaleSoP` sop)
  where
    toNumSoP' :: (Ord u, Nameable u) => Integer -> PrimExp u -> AlgM u (Integer, SoP u)
    toNumSoP' _ pe
      | notIntType (primExpType pe) =
          error "toNumSoP applied to a PrimExp whose prim type is not Integer"
    toNumSoP' f (LeafExp vnm _) =
      pure (f, sym2SoP vnm)
    toNumSoP' f (ValueExp (IntValue iv)) =
      pure (1, int2SoP $ getIntVal iv `div` f)
      where
        getIntVal :: IntValue -> Integer
        getIntVal (Int8Value v) = fromIntegral v
        getIntVal (Int16Value v) = fromIntegral v
        getIntVal (Int32Value v) = fromIntegral v
        getIntVal (Int64Value v) = fromIntegral v
    toNumSoP' f (UnOpExp Complement {} x) = do
      (f', x_sop) <- toNumSoP' f x
      pure (f', negSoP x_sop)
    toNumSoP' f (BinOpExp Add {} x y) = do
      (x_f, x_sop) <- toNumSoP x
      (y_f, y_sop) <- toNumSoP y
      let l_c_m = lcm x_f y_f
          (x_m, y_m) = (l_c_m `div` x_f, l_c_m `div` y_f)
          x_sop' = mulSoPs (int2SoP x_m) x_sop
          y_sop' = mulSoPs (int2SoP y_m) y_sop
      pure (f * l_c_m, addSoPs x_sop' y_sop')
    toNumSoP' f (BinOpExp Sub {} x y) = do
      (x_f, x_sop) <- toNumSoP x
      (y_f, y_sop) <- toNumSoP y
      let l_c_m = lcm x_f y_f
          (x_m, y_m) = (l_c_m `div` x_f, l_c_m `div` y_f)
          x_sop' = mulSoPs (int2SoP x_m) x_sop
          n_y_sop' = mulSoPs (int2SoP (-y_m)) y_sop
      pure (f * l_c_m, addSoPs x_sop' n_y_sop')
    toNumSoP' f pe@(BinOpExp Mul {} x y) = do
      (x_f, x_sop) <- toNumSoP x
      (y_f, y_sop) <- toNumSoP y
      case (x_f, y_f) of
        (1, 1) -> pure (f, mulSoPs x_sop y_sop)
        _ -> do
          x' <- lookupUntransPE pe
          toNumSoP' f $ LeafExp x' $ primExpType pe
    -- pe / 1 == pe
    toNumSoP' f (BinOpExp divish pe q)
      | divideIsh divish && oneIshExp q =
          toNumSoP' f pe
    -- evaluate `val_x / val_y`
    toNumSoP' f (BinOpExp divish x y)
      | divideIsh divish,
        ValueExp v_x <- x,
        ValueExp v_y <- y = do
          let f' = v_x `vdiv` v_y
          toNumSoP' f $ ValueExp f'
      -- Trivial simplifications:
      -- (y * v) / y = v and (u * y) / y = u
      | divideIsh divish,
        BinOpExp (Mul _ _) u v <- x,
        (is_fst, is_snd) <- (u == y, v == y),
        is_fst || is_snd = do
          toNumSoP' f $ if is_fst then v else u
      where
        vdiv (IntValue (Int64Value x')) (IntValue (Int64Value y')) =
          IntValue $ Int64Value (x' `div` y')
        vdiv (IntValue (Int32Value x')) (IntValue (Int32Value y')) =
          IntValue $ Int32Value (x' `div` y')
        vdiv (IntValue (Int16Value x')) (IntValue (Int16Value y')) =
          IntValue $ Int16Value (x' `div` y')
        vdiv (IntValue (Int8Value x')) (IntValue (Int8Value y')) =
          IntValue $ Int8Value (x' `div` y')
        --    vdiv (FloatValue (Float32Value x)) (FloatValue (Float32Value y)) =
        --      FloatValue $ Float32Value $ x / y
        --    vdiv (FloatValue (Float64Value x)) (FloatValue (Float64Value y)) =
        --      FloatValue $ Float64Value $ x / y
        vdiv _ _ = error "In vdiv: illegal type for division!"
    -- try heuristic for exact division
    toNumSoP' f pe@(BinOpExp divish x y)
      | divideIsh divish = do
          (x_f, x_sop) <- toNumSoP x
          (y_f, y_sop) <- toNumSoP y
          case (x_f, y_f, divSoPs x_sop y_sop) of
            (1, 1, Just res) -> pure (f, res)
            _ -> do
              x' <- lookupUntransPE pe
              toNumSoP' f $ LeafExp x' $ primExpType pe
    --  Anything that is not handled by specific cases of toNumSoP
    --   is handled by this default procedure:
    --    If the target `pe` is in the unknwon `env`
    --    Then return thecorresponding binding
    --    Else make a fresh symbol `v`, bind it in the environment
    --         and return it.
    toNumSoP' f pe = do
      x <- lookupUntransPE pe
      toNumSoP' f $ LeafExp x $ primExpType pe

-- | Translates from a 'SoP' representation to a 'PrimExp' representation.
fromNumSoP :: Ord u => SoP u -> PrimExp u
fromNumSoP sop =
  foldr ((~+~) . fromTerm) (ValueExp $ IntValue $ intValue Int64 (0 :: Integer)) (sopToLists sop)
  where
    fromTerm (term, n) =
      foldl (~*~) (ValueExp $ IntValue $ intValue Int64 n) $
        map fromSym term

-- | Translates a symbol into a 'PrimExp'.
fromSym :: u -> PrimExp u
fromSym sym = LeafExp sym $ IntType Int64

-- | Translates a 'PrimExp' containing a (top-level) comparison
-- operator into a 'SoP' representation such that @sop >= 0@.
toNumSoPCmp :: (Ord u, Nameable u) => PrimExp u -> AlgM u (Integer, SoP u >= 0)
toNumSoPCmp (CmpOpExp (CmpEq ptp) x y)
  -- x = y => x - y = 0
  | IntType {} <- ptp = toNumSoP $ x ~-~ y
toNumSoPCmp (CmpOpExp lessop x y)
  -- x < y => x + 1 <= y => y >= x + 1 => y - (x+1) >= 0
  | Just itp <- lthishType lessop =
      toNumSoP $ y ~-~ (x ~+~ ValueExp (IntValue $ intValue itp (1 :: Integer)))
  -- x <= y => y >= x => y - x >= 0
  | Just _ <- leqishType lessop =
      toNumSoP $ y ~-~ x
  where
    lthishType (CmpSlt itp) = Just itp
    lthishType (CmpUlt itp) = Just itp
    lthishType _ = Nothing
    leqishType (CmpUle itp) = Just itp
    leqishType (CmpSle itp) = Just itp
    leqishType _ = Nothing
toNumSoPCmp pe = toNumSoP pe

{--
-- This is a more refined treatment, but probably
--   an overkill (harmful if you get the type wrong)
fromSym unknowns sym
  | Nothing <- M.lookup sym (dir unknowns) =
      LeafExp sym $ IntType Integer
  | Just pe1 <- M.lookup sym (dir unknowns),
    IntType Integer <- primExpType pe1 =
      pe1
fromSym unknowns sym =
  error ("Type error in fromSym: type of " ++
          show sym ++ " is not Integer")
--}
