{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
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
  , module Futhark.Representation.AST.Attributes.Aliases
  , module Futhark.Representation.AST.Attributes.Patterns
  , module Futhark.Representation.AST.Attributes.Names
  , module Futhark.Representation.AST.RetType
  , module Futhark.Representation.AST.Lore

  -- * Extra tools
  , funDecByName
  , asPrimOp
  , asLoopOp
  , asSegOp
  , safeExp
  , loopResultValues
  , getStreamAccums
  , getStreamOrder
  , subExpVars
  , shapeVars

  , IsOp (..)
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
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.AST.RetType
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Lore
import Futhark.Transform.Rename (Rename)
import Futhark.Transform.Substitute (Substitute)
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

-- | If the expression is a 'SegOp', return that 'SegOp', otherwise 'Nothing'.
asSegOp :: Exp lore -> Maybe (SegOp lore)
asSegOp (SegOp op) = Just op
asSegOp _          = Nothing

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
safeExp (SegOp _) = False
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
