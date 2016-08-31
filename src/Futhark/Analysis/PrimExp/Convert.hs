-- | Converting back and forth between 'PrimExp's.
module Futhark.Analysis.PrimExp.Convert
  (
    primExpToExp
  , primExpFromExp

    -- * Module reexport
    , module Futhark.Analysis.PrimExp
  ) where

import           Control.Applicative

import           Prelude

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
primExpToExp f (LeafExp v _) =
  f v

primExpToSubExp :: MonadBinder m =>
                   String -> (v -> m (Exp (Lore m))) -> PrimExp v -> m SubExp
primExpToSubExp s f e = letSubExp s =<< primExpToExp f e

-- | Convert an expression to a 'PrimExp'.  The provided function is
-- used to convert expressions that are not trivially 'PrimExp's.
-- This includes constants and variable names, which are passed as
-- 'SubExp's.
primExpFromExp :: (Applicative m, Monad m) =>
                  (Exp lore -> m (PrimExp v)) -> Exp lore -> m (PrimExp v)
primExpFromExp f (BasicOp (BinOp op x y)) =
  BinOpExp op <$> primExpFromSubExp f x <*> primExpFromSubExp f y
primExpFromExp f (BasicOp (CmpOp op x y)) =
  CmpOpExp op <$> primExpFromSubExp f x <*> primExpFromSubExp f y
primExpFromExp f (BasicOp (UnOp op x)) =
  UnOpExp op <$> primExpFromSubExp f x
primExpFromExp f (BasicOp (ConvOp op x)) =
  ConvOpExp op <$> primExpFromSubExp f x
primExpFromExp f e = f e

primExpFromSubExp :: (Applicative m, Monad m) =>
                     (Exp lore -> m (PrimExp v)) -> SubExp -> m (PrimExp v)
primExpFromSubExp f = primExpFromExp f . BasicOp . SubExp
