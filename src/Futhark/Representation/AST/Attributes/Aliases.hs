{-# Language FlexibleInstances, FlexibleContexts #-}
module Futhark.Representation.AST.Attributes.Aliases
       ( identAliases
       , subExpAliases
       , primOpAliases
       , loopOpAliases
       , aliasesOf
       , Aliased (..)
         -- * Consumption
       , consumedInExp
       )
       where

import Control.Arrow (first)
import Data.Monoid
import qualified Data.HashSet as HS

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Lore (Lore)
import Futhark.Representation.AST.RetType
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Patterns

class Lore lore => Aliased lore where
  bodyAliases :: Body lore -> [Names]
  consumedInBody :: Body lore -> Names
  patternAliases :: Pattern lore -> [Names]

identAliases :: Ident -> Names
identAliases = HS.singleton . identName

subExpAliases :: SubExp -> Names
subExpAliases (Constant {}) = mempty
subExpAliases (Var v)       = identAliases v

primOpAliases :: PrimOp lore -> [Names]
primOpAliases (SubExp se) = [subExpAliases se]
primOpAliases (ArrayLit es _ _) = map subExpAliases es
primOpAliases (BinOp {}) = [mempty]
primOpAliases (Not {}) = [mempty]
primOpAliases (Negate {}) = [mempty]
primOpAliases (Index _ ident _ _) =
  [identAliases ident]
primOpAliases (Update {}) =
  [mempty]
primOpAliases (Iota {}) =
  [mempty]
primOpAliases (Replicate _ e _) =
  [subExpAliases e]
primOpAliases (Reshape _ _ e _) =
  [identAliases e]
primOpAliases (Rearrange _ _ e _) =
  [identAliases e]
primOpAliases (Rotate _ _ e _) =
  [identAliases e]
primOpAliases (Split _ _ e _ _) =
  [identAliases e,identAliases e]
primOpAliases (Concat _ x y _ _) =
  [identAliases x <> identAliases y]
primOpAliases (Copy {}) =
  [mempty]
primOpAliases (Assert _ _) =
  [mempty]
primOpAliases (Conjoin _ _) =
  [mempty]
primOpAliases (Alloc _ _) =
  [mempty]

loopOpAliases :: (Aliased lore) => LoopOp lore -> [Names]
loopOpAliases (DoLoop res merge _ _ loopbody _) =
  map snd $ filter fst $ zip (map ((`elem` res) . bindeeIdent . fst) merge) (bodyAliases loopbody)
loopOpAliases (Map _ f _ _) =
  bodyAliases $ lambdaBody f
loopOpAliases (Reduce _ f _ _) =
  map (const mempty) $ lambdaReturnType f
loopOpAliases (Scan _ f _ _) =
  map (const mempty) $ lambdaReturnType f
loopOpAliases (Filter _ _ arrs _) =
  map identAliases arrs
loopOpAliases (Redomap _ outerfun _ _ _ _) =
  map (const mempty) $ lambdaReturnType outerfun

ifAliases :: ([Names], Names) -> ([Names], Names) -> [Names]
ifAliases (als1,cons1) (als2,cons2) =
  map (HS.filter notConsumed) $ zipWith mappend als1 als2
  where notConsumed = not . (`HS.member` cons)
        cons = cons1 <> cons2

funcallAliases :: [(SubExp, Diet)] -> [DeclType] -> [Names]
funcallAliases args t =
  returnAliases t [(subExpAliases se, d) | (se,d) <- args ]

aliasesOf :: Aliased lore => Exp lore -> [Names]
aliasesOf (If _ tb fb _ _) =
  ifAliases
  (bodyAliases tb, consumedInBody tb)
  (bodyAliases fb, consumedInBody fb)
aliasesOf (PrimOp op) = primOpAliases op
aliasesOf (LoopOp op) = loopOpAliases op
aliasesOf (Apply _ args t _) =
  funcallAliases args $ map toDecl $ retTypeValues t

returnAliases :: [TypeBase shape1] -> [(Names, Diet)] -> [Names]
returnAliases rts args = map returnType' rts
  where returnType' (Array _ _ Nonunique) =
          mconcat $ map (uncurry maskAliases) args
        returnType' (Array _ _ Unique) =
          mempty
        returnType' (Basic _) =
          mempty
        returnType' (Mem {}) =
          error "returnAliases Mem"

maskAliases :: Names -> Diet -> Names
maskAliases _   Consume = mempty
maskAliases als Observe = als

consumedInExp :: (Aliased lore) => Exp lore -> Names
consumedInExp (Apply _ args _ _) =
  mconcat $ map (consumeArg . first subExpAliases) args
  where consumeArg (als, Consume) = als
        consumeArg (_,   Observe) = mempty
consumedInExp (PrimOp (Update _ src _ _ _)) =
  identAliases src
consumedInExp (If _ tb fb _ _) =
  consumedInBody tb <> consumedInBody fb
consumedInExp (LoopOp (DoLoop _ merge _ _ _ _)) =
  mconcat $ map (subExpAliases . snd) $ filter (unique . bindeeType . fst) merge
consumedInExp _ = mempty
