{-# LANGUAGE TypeFamilies #-}
{-# Language FlexibleInstances, FlexibleContexts #-}
module Futhark.Representation.AST.Attributes.Aliases
       ( vnameAliases
       , subExpAliases
       , primOpAliases
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

import Futhark.Representation.AST.Attributes (IsOp)
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Names

class (Annotations lore, AliasedOp (Op lore),
       AliasesOf (LetAttr lore)) => Aliased lore where
  bodyAliases :: Body lore -> [Names]
  consumedInBody :: Body lore -> Names

vnameAliases :: VName -> Names
vnameAliases = oneName

subExpAliases :: SubExp -> Names
subExpAliases Constant{} = mempty
subExpAliases (Var v)    = vnameAliases v

primOpAliases :: BasicOp lore -> [Names]
primOpAliases (SubExp se) = [subExpAliases se]
primOpAliases (Opaque se) = [subExpAliases se]
primOpAliases (ArrayLit _ _) = [mempty]
primOpAliases BinOp{} = [mempty]
primOpAliases ConvOp{} = [mempty]
primOpAliases CmpOp{} = [mempty]
primOpAliases UnOp{} = [mempty]

primOpAliases (Index ident _) =
  [vnameAliases ident]
primOpAliases Update{} =
  [mempty]
primOpAliases Iota{} =
  [mempty]
primOpAliases Replicate{} =
  [mempty]
primOpAliases (Repeat _ _ v) =
  [vnameAliases v]
primOpAliases Scratch{} =
  [mempty]
primOpAliases (Reshape _ e) =
  [vnameAliases e]
primOpAliases (Rearrange _ e) =
  [vnameAliases e]
primOpAliases (Rotate _ e) =
  [vnameAliases e]
primOpAliases Concat{} =
  [mempty]
primOpAliases Copy{} =
  [mempty]
primOpAliases Manifest{} =
  [mempty]
primOpAliases Assert{} =
  [mempty]

ifAliases :: ([Names], Names) -> ([Names], Names) -> [Names]
ifAliases (als1,cons1) (als2,cons2) =
  map (`namesSubtract` cons) $ zipWith mappend als1 als2
  where cons = cons1 <> cons2

funcallAliases :: [(SubExp, Diet)] -> [TypeBase shape Uniqueness] -> [Names]
funcallAliases args t =
  returnAliases t [(subExpAliases se, d) | (se,d) <- args ]

expAliases :: (Aliased lore) => Exp lore -> [Names]
expAliases (If _ tb fb attr) =
  drop (length all_aliases - length ts) all_aliases
  where ts = ifReturns attr
        all_aliases = ifAliases
                      (bodyAliases tb, consumedInBody tb)
                      (bodyAliases fb, consumedInBody fb)
expAliases (BasicOp op) = primOpAliases op
expAliases (DoLoop ctxmerge valmerge _ loopbody) =
  map (`namesSubtract` merge_names) val_aliases
  where (_ctx_aliases, val_aliases) =
          splitAt (length ctxmerge) $ bodyAliases loopbody
        merge_names = namesFromList $ map (paramName . fst) $ ctxmerge ++ valmerge
expAliases (Apply _ args t _) =
  funcallAliases args $ retTypeValues t
expAliases (Op op) = opAliases op

returnAliases :: [TypeBase shape Uniqueness] -> [(Names, Diet)] -> [Names]
returnAliases rts args = map returnType' rts
  where returnType' (Array _ _ Nonunique) =
          mconcat $ map (uncurry maskAliases) args
        returnType' (Array _ _ Unique) =
          mempty
        returnType' (Prim _) =
          mempty
        returnType' Mem{} =
          error "returnAliases Mem"

maskAliases :: Names -> Diet -> Names
maskAliases _   Consume = mempty
maskAliases _   ObservePrim = mempty
maskAliases als Observe = als

consumedInStm :: Aliased lore => Stm lore -> Names
consumedInStm = consumedInExp . stmExp

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
consumedInExp (Op op) = consumedInOp op
consumedInExp _ = mempty

consumedByLambda :: Aliased lore => Lambda lore -> Names
consumedByLambda = consumedInBody . lambdaBody

patternAliases :: AliasesOf attr => PatternT attr -> [Names]
patternAliases = map (aliasesOf . patElemAttr) . patternElements

-- | Something that contains alias information.
class AliasesOf a where
  -- | The alias of the argument element.
  aliasesOf :: a -> Names

instance AliasesOf Names where
  aliasesOf = id

instance AliasesOf attr => AliasesOf (PatElemT attr) where
  aliasesOf = aliasesOf . patElemAttr

class IsOp op => AliasedOp op where
  opAliases :: op -> [Names]
  consumedInOp :: op -> Names

instance AliasedOp () where
  opAliases () = []
  consumedInOp () = mempty

class AliasedOp (OpWithAliases op) => CanBeAliased op where
  type OpWithAliases op :: Data.Kind.Type
  removeOpAliases :: OpWithAliases op -> op
  addOpAliases :: op -> OpWithAliases op

instance CanBeAliased () where
  type OpWithAliases () = ()
  removeOpAliases = id
  addOpAliases = id
