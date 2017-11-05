{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
-- | This module exports a type class covering representations of
-- function return types.
module Futhark.Representation.AST.RetType
       (
         IsBodyType (..)
       , bodyTypeValues
       , IsRetType (..)
       , retTypeValues
       , expectedTypes
       )
       where

import qualified Data.Map.Strict as M

import Futhark.Representation.AST.Syntax.Core
import Futhark.Representation.AST.Attributes.Types

-- | A type representing the return type of a body.  It should contain
-- at least the information contained in a list of 'ExtType's, but may
-- have more, notably an existential context.
class (Show rt, Eq rt, Ord rt, ExtTyped rt) => IsBodyType rt where
  -- | Construct a body type from a primitive type.
  primBodyType :: PrimType -> rt

bodyTypeValues :: IsBodyType rt => [rt] -> [ExtType]
bodyTypeValues = map extTypeOf

instance IsBodyType ExtType where
  primBodyType = Prim

-- | A type representing the return type of a function.  In practice,
-- a list of these will be used.  It should contain at least the
-- information contained in an 'ExtType', but may have more, notably
-- an existential context.
class (Show rt, Eq rt, Ord rt, DeclExtTyped rt) => IsRetType rt where
  -- | Contruct a return type from a primitive type.
  primRetType :: PrimType -> rt

  -- | Given a function return type, the parameters of the function,
  -- and the arguments for a concrete call, return the instantiated
  -- return type for the concrete call, if valid.
  applyRetType :: Typed attr =>
                  [rt]
               -> [Param attr]
               -> [(SubExp, Type)]
               -> Maybe [rt]

retTypeValues :: IsRetType rt => [rt] -> [DeclExtType]
retTypeValues = map declExtTypeOf

-- | Given shape parameter names and value parameter types, produce the
-- types of arguments accepted.
expectedTypes :: Typed t => [VName] -> [t] -> [SubExp] -> [Type]
expectedTypes shapes value_ts args = map (correctDims . typeOf) value_ts
    where parammap :: M.Map VName SubExp
          parammap = M.fromList $ zip shapes args

          correctDims t =
            t `setArrayShape`
            Shape (map correctDim $ shapeDims $ arrayShape t)

          correctDim (Constant v) = Constant v
          correctDim (Var v)
            | Just se <- M.lookup v parammap = se
            | otherwise                       = Var v

instance IsRetType DeclExtType where
  primRetType = Prim

  applyRetType extret params args =
    if length args == length params &&
       and (zipWith subtypeOf argtypes $
            expectedTypes (map paramName params) params $ map fst args)
    then Just $ map correctExtDims extret
    else Nothing
    where argtypes = map snd args

          parammap :: M.Map VName SubExp
          parammap = M.fromList $ zip (map paramName params) (map fst args)

          correctExtDims t =
            t `setArrayShape`
            Shape (map correctExtDim $ shapeDims $ arrayShape t)

          correctExtDim (Ext i)  = Ext i
          correctExtDim (Free d) = Free $ correctDim d

          correctDim (Constant v) = Constant v
          correctDim (Var v)
            | Just se <- M.lookup v parammap = se
            | otherwise                       = Var v
