{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, ConstraintKinds #-}
-- | This module provides various simple ways to query and manipulate
-- fundamental Futhark terms, such as types and values.  The intent is to
-- keep "Futhark.Reprsentation.AST.Syntax" simple, and put whatever
-- embellishments we need here.  This is an internal, desugared
-- representation.
module Futhark.Representation.AST.Attributes
  ( module Futhark.Representation.AST.Attributes.Reshape
  , module Futhark.Representation.AST.Attributes.Rearrange
  , module Futhark.Representation.AST.Attributes.Types
  , module Futhark.Representation.AST.Attributes.Values
  , module Futhark.Representation.AST.Attributes.Constants
  , module Futhark.Representation.AST.Attributes.TypeOf
  , module Futhark.Representation.AST.Attributes.Patterns
  , module Futhark.Representation.AST.Attributes.Names
  , module Futhark.Representation.AST.RetType
  , module Futhark.Representation.AST.Attributes.Context

  -- * Built-in functions
  , isBuiltInFunction
  , builtInFunctions

  -- * Extra tools
  , funDefByName
  , asPrimOp
  , safeExp
  , subExpVars
  , subExpVar
  , shapeVars

  , IsOp (..)
  , Attributes (..)
  )
  where

import Data.List
import Data.Maybe (mapMaybe)
import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.AST.Attributes.Reshape
import Futhark.Representation.AST.Attributes.Rearrange
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Values
import Futhark.Representation.AST.Attributes.Constants
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Attributes.TypeOf
import Futhark.Representation.AST.Attributes.Context
import Futhark.Representation.AST.RetType
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Pretty
import Futhark.Transform.Rename (Rename, Renameable)
import Futhark.Transform.Substitute (Substitute, Substitutable)
import Futhark.Util.Pretty (Pretty)

-- | @isBuiltInFunction k@ is 'True' if @k@ is an element of 'builtInFunctions'.
isBuiltInFunction :: Name -> Bool
isBuiltInFunction fnm = fnm `HM.member` builtInFunctions

-- | A map of all built-in functions and their types.
builtInFunctions :: HM.HashMap Name (PrimType,[PrimType])
builtInFunctions = HM.fromList $ map namify
                   [("sqrt32", (FloatType Float32, [FloatType Float32]))
                   ,("log32", (FloatType Float32, [FloatType Float32]))
                   ,("exp32", (FloatType Float32, [FloatType Float32]))
                   ,("cos32", (FloatType Float32, [FloatType Float32]))
                   ,("sin32", (FloatType Float32, [FloatType Float32]))
                   ,("atan2_32", (FloatType Float32, [FloatType Float32, FloatType Float32]))
                   ,("isinf32", (Bool, [FloatType Float32]))
                   ,("isnan32", (Bool, [FloatType Float32]))

                   ,("sqrt64", (FloatType Float64, [FloatType Float64]))
                   ,("log64", (FloatType Float64, [FloatType Float64]))
                   ,("exp64", (FloatType Float64, [FloatType Float64]))
                   ,("cos64", (FloatType Float64, [FloatType Float64]))
                   ,("sin64", (FloatType Float64, [FloatType Float64]))
                   ,("atan2_64", (FloatType Float64, [FloatType Float64, FloatType Float64]))
                   ,("isinf64", (Bool, [FloatType Float64]))
                   ,("isnan64", (Bool, [FloatType Float64]))
                   ]
  where namify (k,v) = (nameFromString k, v)

-- | Find the function of the given name in the Futhark program.
funDefByName :: Name -> Prog lore -> Maybe (FunDef lore)
funDefByName fname = find ((fname ==) . funDefName) . progFunctions

-- | If the expression is a 'PrimOp', return that 'PrimOp', otherwise 'Nothing'.
asPrimOp :: Exp lore -> Maybe (PrimOp lore)
asPrimOp (PrimOp op) = Just op
asPrimOp _           = Nothing

-- | An expression is safe if it is always well-defined (assuming that
-- any required certificates have been checked) in any context.  For
-- example, array indexing is not safe, as the index may be out of
-- bounds.  On the other hand, adding two numbers cannot fail.
safeExp :: IsOp (Op lore) => Exp lore -> Bool
safeExp (PrimOp op) = safePrimOp op
  where safePrimOp (BinOp SDiv{} _ (Constant y)) = not $ zeroIsh y
        safePrimOp (BinOp SDiv{} _ _) = False
        safePrimOp (BinOp UDiv{} _ (Constant y)) = not $ zeroIsh y
        safePrimOp (BinOp UDiv{} _ _) = False
        safePrimOp (BinOp SMod{} _ (Constant y)) = not $ zeroIsh y
        safePrimOp (BinOp SMod{} _ _) = False
        safePrimOp (BinOp UMod{} _ (Constant y)) = not $ zeroIsh y
        safePrimOp (BinOp UMod{} _ _) = False
        safePrimOp (BinOp Pow{} (Constant x) _) = not $ zeroIsh x
        safePrimOp (BinOp Pow{} _ _) = False
        safePrimOp BinOp{} = True
        safePrimOp SubExp{} = True
        safePrimOp UnOp{} = True
        safePrimOp CmpOp{} = True
        safePrimOp ConvOp{} = True
        safePrimOp _ = False

safeExp DoLoop{} = False
safeExp Apply{} = False
safeExp (If _ tbranch fbranch _) =
  all (safeExp . bindingExp) (bodyBindings tbranch) &&
  all (safeExp . bindingExp) (bodyBindings fbranch)
safeExp (Op op) = safeOp op

-- | Return the variable names used in 'Var' subexpressions.  May contain
-- duplicates.
subExpVars :: [SubExp] -> [VName]
subExpVars = mapMaybe subExpVar

subExpVar :: SubExp -> Maybe VName
subExpVar (Var v)    = Just v
subExpVar Constant{} = Nothing

-- | Return the variable dimension sizes.  May contain
-- duplicates.
shapeVars :: Shape -> [VName]
shapeVars = subExpVars . shapeDims

class (Eq op, Ord op, Show op,
       TypedOp op,
       Rename op,
       Substitute op,
       FreeIn op,
       Pretty op) => IsOp op where
  -- | Like 'safeExp', but for arbitrary ops.
  safeOp :: op -> Bool

instance IsOp () where
  safeOp () = True

-- | Lore-specific attributes; also means the lore supports some basic
-- facilities.
class (Annotations lore,

       PrettyLore lore,

       Renameable lore, Substitutable lore,
       FreeIn (ExpAttr lore),
       FreeIn (LetAttr lore),
       FreeIn (BodyAttr lore),
       FreeIn (FParamAttr lore),
       FreeIn (LParamAttr lore),
       FreeIn (RetType lore),

       IsOp (Op lore)) => Attributes lore where
  -- | As far as possible, determine the subexpression to which each
  -- context pattern element will be bound due to evaluation of the
  -- given expression.  The resulting list must have the same number
  -- of elements as there are context elements in the pattern.
  --
  -- The default method invokes 'expExtContext'.
  expContext :: (HasScope lore m, Monad m) =>
                Pattern lore ->
                Exp lore ->
                m [Maybe SubExp]
  expContext = expExtContext
