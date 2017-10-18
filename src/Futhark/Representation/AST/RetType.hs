{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
-- | This module exports a type class covering representations of
-- function return types.
module Futhark.Representation.AST.RetType
       (
         IsRetType (..)
       , ExtRetType (..)
       , expectedTypes
       )
       where

import qualified Data.Map.Strict as M

import Futhark.Representation.AST.Syntax.Core
import Futhark.Representation.AST.Attributes.Types

-- | A type representing the return type of a function.  It should
-- contain at least the information contained in a list of 'ExtType's,
-- but may have more, notably an existential context.
class (Show rt, Eq rt, Ord rt) => IsRetType rt where
  -- | Contruct a return type from a primitive type.
  primRetType :: PrimType -> rt

  -- | Extract the simple type from the return type - although this
  -- may still involve an existential shape context.
  retTypeValues :: rt -> [DeclExtType]

  -- | Given a function return type, the parameters of the function,
  -- and the arguments for a concrete call, return the instantiated
  -- return type for the concrete call, if valid.
  applyRetType :: Typed attr =>
                  rt
               -> [Param attr]
               -> [(SubExp, Type)]
               -> Maybe rt

-- | A simple return type that is just a list of 'ExtType's.  The
-- reason we do not simply use a list is that we want to define our
-- own prettyprinting instance.
newtype ExtRetType = ExtRetType [DeclExtType]
                   deriving (Eq, Ord, Show)

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

instance IsRetType ExtRetType where
  primRetType = ExtRetType . staticShapes . return . Prim

  retTypeValues (ExtRetType ts) = ts

  applyRetType (ExtRetType extret) params args =
    if length args == length params &&
       and (zipWith subtypeOf argtypes $
            expectedTypes (map paramName params) params $ map fst args)
    then Just $ ExtRetType $ map correctExtDims extret
    else Nothing
    where argtypes = map snd args

          parammap :: M.Map VName SubExp
          parammap = M.fromList $ zip (map paramName params) (map fst args)

          correctExtDims t =
            t `setArrayShape`
            ExtShape (map correctExtDim $ extShapeDims $ arrayShape t)

          correctExtDim (Ext i)  = Ext i
          correctExtDim (Free d) = Free $ correctDim d

          correctDim (Constant v) = Constant v
          correctDim (Var v)
            | Just se <- M.lookup v parammap = se
            | otherwise                       = Var v
