{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, ConstraintKinds #-}
-- | This module provides various simple ways to query and manipulate
-- fundamental Futhark terms, such as types and values.  The intent is to
-- keep "Futhark.Reprsentation.AST.Syntax" simple, and put whatever
-- embellishments we need here.  This is an internal, desugared
-- representation.
module Futhark.Representation.AST.Attributes
  ( module Futhark.Representation.AST.Attributes.Reshape
  , module Futhark.Representation.AST.Attributes.Rearrange
  , module Futhark.Representation.AST.Attributes.Stripe
  , module Futhark.Representation.AST.Attributes.Types
  , module Futhark.Representation.AST.Attributes.Values
  , module Futhark.Representation.AST.Attributes.Constants
  , module Futhark.Representation.AST.Attributes.TypeOf
  , module Futhark.Representation.AST.Attributes.Patterns
  , module Futhark.Representation.AST.Attributes.Names
  , module Futhark.Representation.AST.RetType
  , module Futhark.Representation.AST.Attributes.Context

  -- * Extra tools
  , funDecByName
  , asPrimOp
  , asLoopOp
  , safeExp
  , loopResultValues
  , getStreamAccums
  , getStreamOrder
  , subExpVars
  , shapeVars

  , IsOp (..)

  , Attributes (..)
  )
  where

import Data.List
import Data.Maybe (mapMaybe)

import Futhark.Representation.AST.Attributes.Reshape
import Futhark.Representation.AST.Attributes.Rearrange
import Futhark.Representation.AST.Attributes.Stripe
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

-- | Get Stream's accumulators as a sub-expression list
getStreamAccums :: StreamForm lore -> [SubExp]
getStreamAccums (MapLike _       ) = []
getStreamAccums (RedLike _ _ accs) = accs
getStreamAccums (Sequential  accs) = accs

getStreamOrder :: StreamForm lore -> StreamOrd
getStreamOrder (MapLike o    ) = o
getStreamOrder (RedLike o _ _) = o
getStreamOrder (Sequential  _) = InOrder


-- | Figure out which parts of a loop body result correspond to which
-- value identifiers in the pattern.
--
-- The result of @loopResultValues patidents res mergeparams
-- loopresult@ is a mapping from elements of @loopresult@ to elements
-- of @patidents@.  Here, @patidents@ must be the identifiers of the
-- value part of the pattern storing the result of the loop, @res@ the
-- names of the loop result list, @vname@ the names of the merge
-- parameters, and @loopresult@ the result of the loop body -
-- typically, a list of 'SubExp's.
loopResultValues :: [Ident] -> [VName] -> [VName] -> [a]
                 -> [(a, Maybe Ident)]
loopResultValues patidents res mergeparams ses =
  [ (se, resultMapping mergeparam)
  | (mergeparam, se) <- zip mergeparams ses
  ]
  where (_, validents) =
          splitAt (length patidents - length res) patidents
        resmap =
          zip res validents
        resultMapping fparam =
          lookup fparam resmap


-- | Find the function of the given name in the Futhark program.
funDecByName :: Name -> Prog lore -> Maybe (FunDec lore)
funDecByName fname = find ((fname ==) . funDecName) . progFunctions

-- | If the expression is a 'PrimOp', return that 'PrimOp', otherwise 'Nothing'.
asPrimOp :: Exp lore -> Maybe (PrimOp lore)
asPrimOp (PrimOp op) = Just op
asPrimOp _           = Nothing

-- | If the expression is a 'LoopOp', return that 'LoopOp', otherwise 'Nothing'.
asLoopOp :: Exp lore -> Maybe (LoopOp lore)
asLoopOp (LoopOp op) = Just op
asLoopOp _           = Nothing

-- | An expression is safe if it is always well-defined (assuming that
-- any required certificates have been checked) in any context.  For
-- example, array indexing is not safe, as the index may be out of
-- bounds.  On the other hand, adding two numbers cannot fail.
safeExp :: IsOp (Op lore) => Exp lore -> Bool
safeExp (PrimOp op) = safePrimOp op
  where safePrimOp (BinOp FloatDiv _ (Constant (IntVal k)) _) = k /= 0
        safePrimOp (BinOp FloatDiv _ (Constant (Float32Val k)) _) = k /= 0
        safePrimOp (BinOp FloatDiv _ (Constant (Float64Val k)) _) = k /= 0
        safePrimOp (BinOp FloatDiv _ _ _) = False
        safePrimOp (BinOp Mod _ (Constant (IntVal k)) _) = k /= 0
        safePrimOp (BinOp Mod _ (Constant (Float32Val k)) _) = k /= 0
        safePrimOp (BinOp Mod _ (Constant (Float64Val k)) _) = k /= 0
        safePrimOp (BinOp Mod _ _ _) = False
        safePrimOp (BinOp Pow _ _ _) = False
        safePrimOp BinOp{} = True
        safePrimOp SubExp{} = True
        safePrimOp Not{} = True
        safePrimOp Negate{} = True
        safePrimOp _ = False
safeExp (LoopOp _) = False
safeExp Apply{} = False
safeExp (If _ tbranch fbranch _) =
  all (safeExp . bindingExp) (bodyBindings tbranch) &&
  all (safeExp . bindingExp) (bodyBindings fbranch)
safeExp (Op op) = safeOp op

-- | Return the variable names used in 'Var' subexpressions.  May contain
-- duplicates.
subExpVars :: [SubExp] -> [VName]
subExpVars = mapMaybe subExpVar
  where subExpVar (Var v)    = Just v
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
  -- | A constant used to disambiguate method calls.  XXX, this is a
  -- hack to get around mising type application in Haskell, sometimes
  -- resulting in ambiguous types.
  representative :: lore

  -- | A loop returns not only the values indicated in the result list
  -- @res@, but may also have an existential context.  Thus,
  -- @loopResult res merge@ returns those variables in @merge@ that
  -- constitute the context.
  loopResultContext :: lore
                    -> [VName]
                    -> [FParam lore]
                    -> [VName]

  -- | As far as possible, determine the subexpression to which each
  -- context pattern element will be bound due to evaluation of the
  -- given expression.  The resulting list must have the same number
  -- of elements as there are context elements in the pattern.
  --
  -- The default method invokes 'expExtContext'.
  expContext :: (HasTypeEnv (NameType lore) m, Monad m) =>
                Pattern lore ->
                Exp lore ->
                m [Maybe SubExp]
  expContext = expExtContext
