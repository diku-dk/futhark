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
identAliases ident
  | Basic _ <- identType ident = mempty
  | otherwise = HS.singleton $ identName ident

subExpAliases :: SubExp -> Names
subExpAliases (Constant {}) = mempty
subExpAliases (Var v)       = identAliases v

primOpAliases :: PrimOp lore -> [Names]
primOpAliases (SubExp se) = [subExpAliases se]
primOpAliases (ArrayLit es _) = map subExpAliases es
primOpAliases (BinOp {}) = [mempty]
primOpAliases (Not {}) = [mempty]
primOpAliases (Negate {}) = [mempty]
primOpAliases (Index _ ident _) =
  [identAliases ident]
primOpAliases (Iota {}) =
  [mempty]
primOpAliases (Replicate _ e) =
  [subExpAliases e]
primOpAliases (Scratch {}) =
  [mempty]
primOpAliases (Reshape _ _ e) =
  [identAliases e]
primOpAliases (Rearrange _ _ e) =
  [identAliases e]
primOpAliases (Rotate _ _ e) =
  [identAliases e]
primOpAliases (Split _ sizeexps e) =
  replicate (length sizeexps) (identAliases e)
primOpAliases (Concat _ x y _) =
  [identAliases x <> identAliases y]
primOpAliases (Copy {}) =
  [mempty]
primOpAliases (Assert {}) =
  [mempty]
primOpAliases (Conjoin _) =
  [mempty]
primOpAliases (Alloc _) =
  [mempty]

loopOpAliases :: (Aliased lore) => LoopOp lore -> [Names]
loopOpAliases (DoLoop res merge _ _ loopbody) =
  map snd $ filter fst $
  zip (map ((`elem` res) . fparamIdent . fst) merge) (bodyAliases loopbody)
loopOpAliases (Map _ f _) =
  bodyAliases $ lambdaBody f
loopOpAliases (Reduce _ f _) =
  map (const mempty) $ lambdaReturnType f
loopOpAliases (Scan _ f _) =
  map (const mempty) $ lambdaReturnType f
loopOpAliases (Filter _ _ arrs) =
  map identAliases arrs
loopOpAliases (Redomap _ outerfun _ _ _) =
  map (const mempty) $ lambdaReturnType outerfun
loopOpAliases (ConcatMap {}) =
  [mempty]

ifAliases :: ([Names], Names) -> ([Names], Names) -> [Names]
ifAliases (als1,cons1) (als2,cons2) =
  map (HS.filter notConsumed) $ zipWith mappend als1 als2
  where notConsumed = not . (`HS.member` cons)
        cons = cons1 <> cons2

funcallAliases :: [(SubExp, Diet)] -> [DeclType] -> [Names]
funcallAliases args t =
  returnAliases t [(subExpAliases se, d) | (se,d) <- args ]

aliasesOf :: Aliased lore => Exp lore -> [Names]
aliasesOf (If _ tb fb _) =
  ifAliases
  (bodyAliases tb, consumedInBody tb)
  (bodyAliases fb, consumedInBody fb)
aliasesOf (PrimOp op) = primOpAliases op
aliasesOf (LoopOp op) = loopOpAliases op
aliasesOf (Apply _ args t) =
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

consumedInExp :: (Aliased lore) => Pattern anylore -> Exp lore -> Names
consumedInExp pat e =
  consumedInPattern pat <> consumedInExp' e
  where consumedInExp' (Apply _ args _) =
          mconcat (map (consumeArg . first subExpAliases) args)
          where consumeArg (als, Consume) = als
                consumeArg (_,   Observe) = mempty
        consumedInExp' (If _ tb fb _) =
          consumedInBody tb <> consumedInBody fb
        consumedInExp' (LoopOp (DoLoop _ merge _ _ _)) =
          consumedInPattern pat <>
          mconcat (map (subExpAliases . snd) $
                   filter (unique . fparamType . fst) merge)
        consumedInExp' _ = mempty

consumedInPattern :: Pattern lore -> Names
consumedInPattern pat =
  mconcat (map (consumedInBindage . patElemBindage) $
           patternElements pat)
  where consumedInBindage BindVar = mempty
        consumedInBindage (BindInPlace _ src _) = identAliases src
