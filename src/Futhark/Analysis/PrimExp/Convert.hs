{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Converting back and forth between 'PrimExp's.
module Futhark.Analysis.PrimExp.Convert
  (
    primExpToExp
  , primExpToSubExp
  , primExpFromExp
  , primExpFromSubExp
  , primExpFromSubExpM
  , replaceInPrimExp
  , substituteInPrimExp

    -- * Module reexport
    , module Futhark.Analysis.PrimExp
  ) where

import qualified Control.Monad.Fail as Fail
import           Data.Loc
import qualified Data.Map.Strict as M
import           Data.Maybe

import           Futhark.Analysis.PrimExp
import           Futhark.Construct
import           Futhark.Representation.AST

-- | Convert a 'PrimExp' to a Futhark expression.  The provided
-- function converts the leaves.
primExpToExp :: MonadBinder m =>
                (v -> m (Exp (Lore m))) -> PrimExp v -> m (Exp (Lore m))
primExpToExp f (BinOpExp op x y) =
  BasicOp <$> (BinOp op
               <$> primExpToSubExp "binop_x" f x
               <*> primExpToSubExp "binop_y" f y)
primExpToExp f (CmpOpExp op x y) =
  BasicOp <$> (CmpOp op
               <$> primExpToSubExp "cmpop_x" f x
               <*> primExpToSubExp "cmpop_y" f y)
primExpToExp f (UnOpExp op x) =
  BasicOp <$> (UnOp op <$> primExpToSubExp "unop_x" f x)
primExpToExp f (ConvOpExp op x) =
  BasicOp <$> (ConvOp op <$> primExpToSubExp "convop_x" f x)
primExpToExp _ (ValueExp v) =
  return $ BasicOp $ SubExp $ Constant v
primExpToExp f (FunExp h args t) =
  Apply (nameFromString h) <$> args' <*> pure [primRetType t] <*> pure (Safe, noLoc, [])
  where args' = zip <$> mapM (primExpToSubExp "apply_arg" f) args <*> pure (repeat Observe)
primExpToExp f (LeafExp v _) =
  f v

instance ToExp v => ToExp (PrimExp v) where
  toExp = primExpToExp toExp

primExpToSubExp :: MonadBinder m =>
                   String -> (v -> m (Exp (Lore m))) -> PrimExp v -> m SubExp
primExpToSubExp s f e = letSubExp s =<< primExpToExp f e

-- | Convert an expression to a 'PrimExp'.  The provided function is
-- used to convert expressions that are not trivially 'PrimExp's.
-- This includes constants and variable names, which are passed as
-- 'SubExp's.
primExpFromExp :: (Fail.MonadFail m, Annotations lore) =>
                  (VName -> m (PrimExp v)) -> Exp lore -> m (PrimExp v)
primExpFromExp f (BasicOp (BinOp op x y)) =
  BinOpExp op <$> primExpFromSubExpM f x <*> primExpFromSubExpM f y
primExpFromExp f (BasicOp (CmpOp op x y)) =
  CmpOpExp op <$> primExpFromSubExpM f x <*> primExpFromSubExpM f y
primExpFromExp f (BasicOp (UnOp op x)) =
  UnOpExp op <$> primExpFromSubExpM f x
primExpFromExp f (BasicOp (ConvOp op x)) =
  ConvOpExp op <$> primExpFromSubExpM f x
primExpFromExp _ (BasicOp (SubExp (Constant v))) =
  return $ ValueExp v
primExpFromExp f (Apply fname args ts _)
  | isBuiltInFunction fname, [Prim t] <- retTypeValues ts =
      FunExp (nameToString fname) <$> mapM (primExpFromSubExpM f . fst) args <*> pure t
primExpFromExp _ _ = fail "Not a PrimExp"

primExpFromSubExpM :: Fail.MonadFail m =>
                      (VName -> m (PrimExp v)) -> SubExp -> m (PrimExp v)
primExpFromSubExpM f (Var v) = f v
primExpFromSubExpM _ (Constant v) = return $ ValueExp v

-- | Convert 'SubExp's of a given type.
primExpFromSubExp :: PrimType -> SubExp -> PrimExp VName
primExpFromSubExp t (Var v)      = LeafExp v t
primExpFromSubExp _ (Constant v) = ValueExp v

-- | Applying a transformation to the leaves in a 'PrimExp'.
replaceInPrimExp :: (v -> PrimType -> PrimExp v) ->
                    PrimExp v -> PrimExp v
replaceInPrimExp f (LeafExp v pt) =
  f v pt
replaceInPrimExp _ (ValueExp v) =
  ValueExp v
replaceInPrimExp f (BinOpExp bop pe1 pe2) =
  constFoldPrimExp $ BinOpExp bop (replaceInPrimExp f pe1) (replaceInPrimExp f pe2)
replaceInPrimExp f (CmpOpExp cop pe1 pe2) =
  CmpOpExp cop (replaceInPrimExp f pe1) (replaceInPrimExp f pe2)
replaceInPrimExp f (UnOpExp uop pe) =
  UnOpExp uop $ replaceInPrimExp f pe
replaceInPrimExp f (ConvOpExp cop pe) =
  ConvOpExp cop $ replaceInPrimExp f pe
replaceInPrimExp f (FunExp h args t) =
  FunExp h (map (replaceInPrimExp f) args) t

-- | Substituting names in a PrimExp with other PrimExps
substituteInPrimExp :: Ord v => M.Map v (PrimExp v)
                    -> PrimExp v -> PrimExp v
substituteInPrimExp tab = replaceInPrimExp $ \v t ->
  fromMaybe (LeafExp v t) $ M.lookup v tab
