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
       , consumedInPattern
       , consumedByLambda
       , consumedByExtLambda
       -- * Extensibility
       , AliasedOp (..)
       , CanBeAliased (..)
       )
       where

import Control.Arrow (first)
import Data.Monoid
import qualified Data.Set as S

import Futhark.Representation.AST.Attributes (IsOp)
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Types

class (Annotations lore, AliasedOp (Op lore),
       AliasesOf (LetAttr lore)) => Aliased lore where
  bodyAliases :: Body lore -> [Names]
  consumedInBody :: Body lore -> Names

vnameAliases :: VName -> Names
vnameAliases = S.singleton

subExpAliases :: SubExp -> Names
subExpAliases Constant{} = mempty
subExpAliases (Var v)    = vnameAliases v

primOpAliases :: BasicOp lore -> [Names]
primOpAliases (SubExp se) = [subExpAliases se]
primOpAliases (Opaque se) = [subExpAliases se]
primOpAliases (ArrayLit es _) = [mconcat $ map subExpAliases es]
primOpAliases BinOp{} = [mempty]
primOpAliases ConvOp{} = [mempty]
primOpAliases CmpOp{} = [mempty]
primOpAliases UnOp{} = [mempty]

primOpAliases (Index _ ident _) =
  [vnameAliases ident]
primOpAliases Iota{} =
  [mempty]
primOpAliases Replicate{} =
  [mempty]
primOpAliases (Repeat _ _ v) =
  [vnameAliases v]
primOpAliases Scratch{} =
  [mempty]
primOpAliases (Reshape _ _ e) =
  [vnameAliases e]
primOpAliases (Rearrange _ _ e) =
  [vnameAliases e]
primOpAliases (Rotate _ _ e) =
  [vnameAliases e]
primOpAliases (Split _ _ sizeexps e) =
  replicate (length sizeexps) (vnameAliases e)
primOpAliases Concat{} =
  [mempty]
primOpAliases Copy{} =
  [mempty]
primOpAliases Manifest{} =
  [mempty]
primOpAliases Assert{} =
  [mempty]
primOpAliases (Partition _ n _ arr) =
  replicate n mempty ++ map vnameAliases arr

ifAliases :: ([Names], Names) -> ([Names], Names) -> [Names]
ifAliases (als1,cons1) (als2,cons2) =
  map (S.filter notConsumed) $ zipWith mappend als1 als2
  where notConsumed = not . (`S.member` cons)
        cons = cons1 <> cons2

funcallAliases :: [(SubExp, Diet)] -> [TypeBase shape Uniqueness] -> [Names]
funcallAliases args t =
  returnAliases t [(subExpAliases se, d) | (se,d) <- args ]

expAliases :: (Aliased lore) => Exp lore -> [Names]
expAliases (If _ tb fb _) =
  ifAliases
  (bodyAliases tb, consumedInBody tb)
  (bodyAliases fb, consumedInBody fb)
expAliases (BasicOp op) = primOpAliases op
expAliases (DoLoop ctxmerge valmerge _ loopbody) =
  map (`S.difference` merge_names) val_aliases
  where (_ctx_aliases, val_aliases) =
          splitAt (length ctxmerge) $ bodyAliases loopbody
        merge_names = S.fromList $
                      map (paramName . fst) $ ctxmerge ++ valmerge
expAliases (Apply _ args t) =
  funcallAliases args $ retTypeValues t
expAliases (Op op) = opAliases op

returnAliases :: [TypeBase shaper Uniqueness] -> [(Names, Diet)] -> [Names]
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
maskAliases als Observe = als

consumedInStm :: Aliased lore => Stm lore -> Names
consumedInStm binding = consumedInPattern (bindingPattern binding) <>
                            consumedInExp (bindingExp binding)

consumedInExp :: (Aliased lore) => Exp lore -> Names
consumedInExp (Apply _ args _) =
  mconcat (map (consumeArg . first subExpAliases) args)
  where consumeArg (als, Consume) = als
        consumeArg (_,   Observe) = mempty
consumedInExp (If _ tb fb _) =
  consumedInBody tb <> consumedInBody fb
consumedInExp (DoLoop _ merge _ _) =
  mconcat (map (subExpAliases . snd) $
           filter (unique . paramDeclType . fst) merge)
consumedInExp (Op op) = consumedInOp op
consumedInExp _ = mempty

consumedByLambda :: Aliased lore => Lambda lore -> Names
consumedByLambda = consumedInBody . lambdaBody

consumedByExtLambda :: Aliased lore => ExtLambda lore -> Names
consumedByExtLambda = consumedInBody . extLambdaBody

patternAliases :: AliasesOf attr => PatternT attr -> [Names]
patternAliases = map (aliasesOf . patElemAttr) . patternElements

consumedInPattern :: PatternT attr -> Names
consumedInPattern pat =
  mconcat (map (consumedInBindage . patElemBindage) $
           patternContextElements pat ++ patternValueElements pat)
  where consumedInBindage BindVar = mempty
        consumedInBindage (BindInPlace _ src _) = vnameAliases src

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
  type OpWithAliases op :: *
  removeOpAliases :: OpWithAliases op -> op
  addOpAliases :: op -> OpWithAliases op

instance CanBeAliased () where
  type OpWithAliases () = ()
  removeOpAliases = id
  addOpAliases = id
