{-# LANGUAGE TypeFamilies #-}

-- | This module exports a type class covering representations of
-- function return types.
module Futhark.IR.RetType
  ( IsBodyType (..),
    IsRetType (..),
    expectedTypes,
  )
where

import Control.Monad.Identity
import Data.Map.Strict qualified as M
import Futhark.IR.Prop.Types
import Futhark.IR.Syntax.Core

-- | A type representing the return type of a body.  It should contain
-- at least the information contained in a list of 'ExtType's, but may
-- have more, notably an existential context.
class (Show rt, Eq rt, Ord rt, ExtTyped rt) => IsBodyType rt where
  -- | Construct a body type from a primitive type.
  primBodyType :: PrimType -> rt

instance IsBodyType ExtType where
  primBodyType = Prim

-- | A type representing the return type of a function.  In practice,
-- a list of these will be used.  It should contain at least the
-- information contained in an 'ExtType', but may have more, notably
-- an existential context.
class (Show rt, Eq rt, Ord rt, ExtTyped rt, DeclExtTyped rt) => IsRetType rt where
  -- | Contruct a return type from a primitive type.
  primRetType :: PrimType -> rt

  -- | Given a function return type, the parameters of the function,
  -- and the arguments for a concrete call, return the instantiated
  -- return type for the concrete call, if valid.
  applyRetType ::
    (Typed dec) =>
    [rt] ->
    [Param dec] ->
    [(SubExp, Type)] ->
    Maybe [rt]

-- | Given shape parameter names and types, produce the types of
-- arguments accepted.
expectedTypes :: (Typed t) => [VName] -> [t] -> [SubExp] -> [Type]
expectedTypes shapes value_ts args = map (correctDims . typeOf) value_ts
  where
    parammap :: M.Map VName SubExp
    parammap = M.fromList $ zip shapes args

    correctDims = runIdentity . mapOnType (pure . f)
      where
        f (Var v)
          | Just se <- M.lookup v parammap = se
        f se = se

instance IsRetType DeclExtType where
  primRetType = Prim

  applyRetType extret params args =
    if length args == length params
      && and
        ( zipWith subtypeOf argtypes $
            expectedTypes (map paramName params) params $
              map fst args
        )
      then Just $ map correctExtDims extret
      else Nothing
    where
      argtypes = map snd args

      parammap :: M.Map VName SubExp
      parammap = M.fromList $ zip (map paramName params) (map fst args)

      correctExtDims = runIdentity . mapOnExtType (pure . f)
        where
          f (Var v)
            | Just se <- M.lookup v parammap = se
          f se = se
