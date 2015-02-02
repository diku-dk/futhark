{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
-- | This module provides various simple ways to query and manipulate
-- fundamental Futhark terms, such as types and values.  The intent is to
-- keep "Futhark.Reprsentation.AST.Syntax" simple, and put whatever
-- embellishments we need here.  This is an internal, desugared
-- representation.
module Futhark.Representation.AST.Attributes
  ( module Futhark.Representation.AST.Attributes.Types
  , module Futhark.Representation.AST.Attributes.Values
  , module Futhark.Representation.AST.Attributes.Constants
  , module Futhark.Representation.AST.Attributes.TypeOf
  , module Futhark.Representation.AST.Attributes.Patterns
  , module Futhark.Representation.AST.Attributes.Names
  , module Futhark.Representation.AST.RetType

  , representative
  , loopResultContext

  -- * Extra tools
  , funDecByName
  , reshapeOuter
  , reshapeInner
  , asPrimOp
  , asLoopOp
  , safeExp
  , loopResultValues
  )
  where

import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Values
import Futhark.Representation.AST.Attributes.Constants
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Attributes.TypeOf
import Futhark.Representation.AST.RetType
import Futhark.Representation.AST.Syntax
import qualified Futhark.Representation.AST.Lore as Lore

import Data.List

-- | A constant used to disambiguate method calls.  XXX, this is a
-- hack.
representative :: Lore.Lore l => l
representative = Lore.representative

-- | A loop returns not only the values indicated in the result list
-- @res@, but may also have an existential context.  Thus,
-- @loopResult res merge@ returns those variables in @merge@ that
-- constitute the context.
loopResultContext :: Lore.Lore l => l -> [Ident] -> [FParam l] -> [Ident]
loopResultContext = Lore.loopResultContext

-- | Figure out which parts of a loop body result correspond to which
-- value identifiers in the pattern.
--
-- The result of @loopResultValues patidents res mergeparams
-- loopresult@ is a mapping from elements of @loopresult@ to elements
-- of @patidents@.  Here, @patidents@ must be the identifiers of the
-- pattern storing the result of the loop (_including_ any existential
-- context), @res@ the names of the loop result list, @vname@ the
-- names of the merge parameters, and @loopresult@ the result of the
-- loop body - typically, a list of 'SubExp's.
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

shapeExps :: Ident -> [SubExp]
shapeExps = shapeDims . arrayShape . identType

-- | @reshapeOuter shape n src@ returns a 'Reshape' expression that
-- replaces the outer @n@ dimensions of @src@ with @shape@.
reshapeOuter :: [SubExp] -> Int -> Ident -> [SubExp]
reshapeOuter shape n src = shape ++ drop n (shapeExps src)

-- | @reshapeInner shape n src@ returns a 'Reshape' expression that
-- replaces the inner @m-n@ dimensions (where @m@ is the rank of
-- @src@) of @src@ with @shape@.
reshapeInner :: [SubExp] -> Int -> Ident -> [SubExp]
reshapeInner shape n src = take n (shapeExps src) ++ shape

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
safeExp :: Exp lore -> Bool
safeExp (PrimOp op) = safePrimOp op
  where safePrimOp (BinOp Divide _ (Constant (IntVal k)) _) = k /= 0
        safePrimOp (BinOp Divide _ (Constant (RealVal k)) _) = k /= 0
        safePrimOp (BinOp Divide _ _ _) = False
        safePrimOp (BinOp Mod _ (Constant (IntVal k)) _) = k /= 0
        safePrimOp (BinOp Mod _ (Constant (RealVal k)) _) = k /= 0
        safePrimOp (BinOp Mod _ _ _) = False
        safePrimOp (BinOp Pow _ _ _) = False
        safePrimOp (BinOp {}) = True
        safePrimOp (SubExp {}) = True
        safePrimOp (Not {}) = True
        safePrimOp (Negate {}) = True
        safePrimOp (Conjoin {}) = True
        safePrimOp _ = False
safeExp (LoopOp _) = False
safeExp (Apply {}) = False
safeExp (If _ tbranch fbranch _) =
  all (safeExp . bindingExp) (bodyBindings tbranch) &&
  all (safeExp . bindingExp) (bodyBindings fbranch)
