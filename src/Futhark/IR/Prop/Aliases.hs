{-# LANGUAGE TypeFamilies #-}
{-# Language FlexibleInstances, FlexibleContexts #-}
-- | The IR tracks aliases, mostly to ensure the soundness of in-place
-- updates, but it can also be used for other things (such as memory
-- optimisations).  This module contains the raw building blocks for
-- determining the aliases of the values produced by expressions.  It
-- also contains some building blocks for inspecting consumption.
--
-- One important caveat is that all aliases computed here are /local/.
-- Thus, they do not take aliases-of-aliases into account.  See
-- "Futhark.Analysis.Alias" if this is not what you want.
module Futhark.IR.Prop.Aliases
       ( subExpAliases
       , expAliases
       , patternAliases
       , Aliased (..)
       , AliasesOf (..)
         -- * Consumption
       , consumedInStm
       , consumedInExp
       , consumedByLambda
       -- * Extensibility
       , AliasedOp (..)
       , CanBeAliased (..)
       )
       where

import Control.Arrow (first)
import qualified Data.Kind

import Futhark.IR.Prop (IsOp)
import Futhark.IR.Syntax
import Futhark.IR.Prop.Patterns
import Futhark.IR.Prop.Types
import Futhark.IR.Prop.Names

-- | The class of lores that contain aliasing information.
class (Decorations lore, AliasedOp (Op lore),
       AliasesOf (LetDec lore)) => Aliased lore where
  -- | The aliases of the body results.
  bodyAliases :: Body lore -> [Names]
  -- | The variables consumed in the body.
  consumedInBody :: Body lore -> Names

vnameAliases :: VName -> Names
vnameAliases = oneName

-- | The alises of a subexpression.
subExpAliases :: SubExp -> Names
subExpAliases Constant{} = mempty
subExpAliases (Var v)    = vnameAliases v

basicOpAliases :: BasicOp -> [Names]
basicOpAliases (SubExp se) = [subExpAliases se]
basicOpAliases (Opaque se) = [subExpAliases se]
basicOpAliases (ArrayLit _ _) = [mempty]
basicOpAliases BinOp{} = [mempty]
basicOpAliases ConvOp{} = [mempty]
basicOpAliases CmpOp{} = [mempty]
basicOpAliases UnOp{} = [mempty]
basicOpAliases (Index ident _) = [vnameAliases ident]
basicOpAliases Update{} = [mempty]
basicOpAliases Iota{} = [mempty]
basicOpAliases Replicate{} = [mempty]
basicOpAliases Scratch{} = [mempty]
basicOpAliases (Reshape _ e) = [vnameAliases e]
basicOpAliases (Rearrange _ e) = [vnameAliases e]
basicOpAliases (Rotate _ e) = [vnameAliases e]
basicOpAliases Concat{} = [mempty]
basicOpAliases Copy{} = [mempty]
basicOpAliases Manifest{} = [mempty]
basicOpAliases Assert{} = [mempty]
basicOpAliases (UnAcc _ ts) = map (const mempty) ts
basicOpAliases UpdateAcc{} = [mempty]

ifAliases :: ([Names], Names) -> ([Names], Names) -> [Names]
ifAliases (als1,cons1) (als2,cons2) =
  map (`namesSubtract` cons) $ zipWith mappend als1 als2
  where cons = cons1 <> cons2

funcallAliases :: [(SubExp, Diet)] -> [TypeBase shape Uniqueness] -> [Names]
funcallAliases args t =
  returnAliases t [(subExpAliases se, d) | (se,d) <- args ]

-- | The aliases of an expression, one per non-context value returned.
expAliases :: (Aliased lore) => Exp lore -> [Names]
expAliases (If _ tb fb dec) =
  drop (length all_aliases - length ts) all_aliases
  where ts = ifReturns dec
        all_aliases = ifAliases
                      (bodyAliases tb, consumedInBody tb)
                      (bodyAliases fb, consumedInBody fb)
expAliases (BasicOp op) = basicOpAliases op
expAliases (DoLoop ctxmerge valmerge _ loopbody) =
  map (`namesSubtract` merge_names) val_aliases
  where (_ctx_aliases, val_aliases) =
          splitAt (length ctxmerge) $ bodyAliases loopbody
        merge_names = namesFromList $ map (paramName . fst) $ ctxmerge ++ valmerge
expAliases (Apply _ args t _) =
  funcallAliases args $ map declExtTypeOf t
expAliases MkAcc{} = [mempty]
expAliases (Op op) = opAliases op

returnAliases :: [TypeBase shape Uniqueness] -> [(Names, Diet)] -> [Names]
returnAliases rts args = map returnType' rts
  where returnType' (Array _ _ Nonunique) =
          mconcat $ map (uncurry maskAliases) args
        returnType' (Array _ _ Unique) =
          mempty
        returnType' (Prim _) =
          mempty
        returnType' Acc{} =
          error "returnAliases Acc"
        returnType' Mem{} =
          error "returnAliases Mem"

maskAliases :: Names -> Diet -> Names
maskAliases _   Consume = mempty
maskAliases _   ObservePrim = mempty
maskAliases als Observe = als

-- | The variables consumed in this statement.
consumedInStm :: Aliased lore => Stm lore -> Names
consumedInStm = consumedInExp . stmExp

-- | The variables consumed in this expression.
consumedInExp :: (Aliased lore) => Exp lore -> Names
consumedInExp (Apply _ args _ _) =
  mconcat (map (consumeArg . first subExpAliases) args)
  where consumeArg (als, Consume) = als
        consumeArg _              = mempty
consumedInExp (If _ tb fb _) =
  consumedInBody tb <> consumedInBody fb
consumedInExp (DoLoop _ merge _ _) =
  mconcat (map (subExpAliases . snd) $
           filter (unique . paramDeclType . fst) merge)
consumedInExp (BasicOp (Update src _ _)) = oneName src
consumedInExp (BasicOp (UpdateAcc acc _ _)) = oneName acc
consumedInExp (Op op) = consumedInOp op
consumedInExp _ = mempty

-- | The variables consumed by this lambda.
consumedByLambda :: Aliased lore => Lambda lore -> Names
consumedByLambda = consumedInBody . lambdaBody

-- | The aliases of each pattern element (including the context).
patternAliases :: AliasesOf dec => PatternT dec -> [Names]
patternAliases = map (aliasesOf . patElemDec) . patternElements

-- | Something that contains alias information.
class AliasesOf a where
  -- | The alias of the argument element.
  aliasesOf :: a -> Names

instance AliasesOf Names where
  aliasesOf = id

instance AliasesOf dec => AliasesOf (PatElemT dec) where
  aliasesOf = aliasesOf . patElemDec

-- | The class of operations that can produce aliasing and consumption
-- information.
class IsOp op => AliasedOp op where
  opAliases :: op -> [Names]
  consumedInOp :: op -> Names

instance AliasedOp () where
  opAliases () = []
  consumedInOp () = mempty

-- | The class of operations that can be given aliasing information.
-- This is a somewhat subtle concept that is only used in the
-- simplifier and when using "lore adapters".
class AliasedOp (OpWithAliases op) => CanBeAliased op where
  -- | The op that results when we add aliases to this op.
  type OpWithAliases op :: Data.Kind.Type

  -- | Remove aliases from this op.
  removeOpAliases :: OpWithAliases op -> op

  -- | Add aliases to this op.
  addOpAliases :: op -> OpWithAliases op

instance CanBeAliased () where
  type OpWithAliases () = ()
  removeOpAliases = id
  addOpAliases = id
