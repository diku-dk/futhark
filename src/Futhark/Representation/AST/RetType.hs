{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
-- | This module exports a type class covering representations of
-- function return types.
module Futhark.Representation.AST.RetType
       (
         IsRetType (..)
       , ExtRetType (..)
       )
       where

import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.AST.Syntax.Core
import Futhark.Representation.AST.Attributes.Types

-- | A type representing the return type of a function.  It should
-- contain at least the information contained in a list of 'ExtType's,
-- but may have more, notably an existential context.
class (Show rt, Eq rt, Ord rt) => IsRetType rt where
  -- | Contruct a return type from a basic (scalar) type.
  basicRetType :: BasicType -> rt

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

instance IsRetType ExtRetType where
  basicRetType = ExtRetType . staticShapes . return . Basic

  retTypeValues (ExtRetType ts) = ts

  applyRetType (ExtRetType extret) params args =
    if length args == length params &&
       and (zipWith subtypeOf
            (map rankShaped argtypes)
            (map rankShaped paramtypes))
    then Just $ ExtRetType $ map correctDims extret
    else Nothing
    where argtypes = map snd args
          paramtypes = map typeOf params

          parammap :: HM.HashMap VName SubExp
          parammap = HM.fromList $
                     zip (map paramName params) (map fst args)

          correctDims t =
            t `setArrayShape`
            ExtShape (map correctDim $ extShapeDims $ arrayShape t)

          correctDim (Ext i) =
            Ext i
          correctDim (Free (Constant v)) =
            Free $ Constant v
          correctDim (Free (Var v))
            | Just se <- HM.lookup v parammap =
              Free se
            | otherwise =
              Free $ Var v
