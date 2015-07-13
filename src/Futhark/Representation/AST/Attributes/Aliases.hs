{-# Language FlexibleInstances, FlexibleContexts #-}
module Futhark.Representation.AST.Attributes.Aliases
       ( vnameAliases
       , subExpAliases
       , primOpAliases
       , loopOpAliases
       , aliasesOf
       , Aliased (..)
         -- * Consumption
       , consumedInBinding
       , consumedInExp
       , consumedInPattern
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

vnameAliases :: VName -> Names
vnameAliases = HS.singleton

subExpAliases :: SubExp -> Names
subExpAliases (Constant {}) = mempty
subExpAliases (Var v)       = vnameAliases v

primOpAliases :: PrimOp lore -> [Names]
primOpAliases (SubExp se) = [subExpAliases se]
primOpAliases (ArrayLit es _) = [mconcat $ map subExpAliases es]
primOpAliases (BinOp {}) = [mempty]
primOpAliases (Not {}) = [mempty]
primOpAliases (Complement {}) = [mempty]
primOpAliases (Negate {}) = [mempty]
primOpAliases (Abs {}) = [mempty]
primOpAliases (Signum {}) = [mempty]
primOpAliases (Index _ ident _) =
  [vnameAliases ident]
primOpAliases (Iota {}) =
  [mempty]
primOpAliases (Replicate _ e) =
  [subExpAliases e]
primOpAliases (Scratch {}) =
  [mempty]
primOpAliases (Reshape _ _ e) =
  [vnameAliases e]
primOpAliases (Rearrange _ _ e) =
  [vnameAliases e]
primOpAliases (Split _ sizeexps e) =
  replicate (length sizeexps) (vnameAliases e)
primOpAliases (Concat _ x ys _) =
  [vnameAliases x <> mconcat (map vnameAliases ys)]
primOpAliases (Copy {}) =
  [mempty]
primOpAliases (Assert {}) =
  [mempty]
primOpAliases (Alloc _) =
  [mempty]
primOpAliases (Partition _ n _ arr) =
  replicate n mempty ++ map vnameAliases arr

loopOpAliases :: (Aliased lore) => LoopOp lore -> [Names]
loopOpAliases (DoLoop res merge _ loopbody) =
  map snd $ filter fst $
  zip (map (((`elem` res) . identName) . paramIdent . fst) merge) (bodyAliases loopbody)
loopOpAliases (Map _ _ f _) =
  bodyAliases $ lambdaBody f
loopOpAliases (Reduce _ _ f _) =
  map (const mempty) $ lambdaReturnType f
loopOpAliases (Scan _ _ f _) =
  map (const mempty) $ lambdaReturnType f
loopOpAliases (Redomap _ _ _ innerfun _ _) =
  map (const mempty) $ lambdaReturnType innerfun
loopOpAliases (Stream _ _ form lam _ _) =
  let a1 = case form of
             MapLike _        -> []
             RedLike _ lam0 _ -> bodyAliases $ lambdaBody lam0
             Sequential _     -> []
  in  a1 ++ bodyAliases (extLambdaBody lam)
loopOpAliases (ConcatMap {}) =
  [mempty]

segOpAliases :: (Aliased lore) => SegOp lore -> [Names]
segOpAliases (SegReduce _ _ f _ _) =
  map (const mempty) $ lambdaReturnType f
segOpAliases (SegScan _ _ _ f _ _) =
  map (const mempty) $ lambdaReturnType f
segOpAliases (SegReplicate{}) =
  [mempty]
-- TODO: Troels, should this be vnameAliases ?

ifAliases :: ([Names], Names) -> ([Names], Names) -> [Names]
ifAliases (als1,cons1) (als2,cons2) =
  map (HS.filter notConsumed) $ zipWith mappend als1 als2
  where notConsumed = not . (`HS.member` cons)
        cons = cons1 <> cons2

funcallAliases :: [(SubExp, Diet)] -> [TypeBase shape] -> [Names]
funcallAliases args t =
  returnAliases t [(subExpAliases se, d) | (se,d) <- args ]

aliasesOf :: Aliased lore => Exp lore -> [Names]
aliasesOf (If _ tb fb _) =
  ifAliases
  (bodyAliases tb, consumedInBody tb)
  (bodyAliases fb, consumedInBody fb)
aliasesOf (PrimOp op) = primOpAliases op
aliasesOf (LoopOp op) = loopOpAliases op
aliasesOf (SegOp op) = segOpAliases op
aliasesOf (Apply _ args t) =
  funcallAliases args $ retTypeValues t

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

consumedInBinding :: Aliased lore => Binding lore -> Names
consumedInBinding binding = consumedInPattern (bindingPattern binding) <>
                            consumedInExp (bindingExp binding)

consumedInExp :: (Aliased lore) => Exp lore -> Names
consumedInExp (Apply _ args _) =
  mconcat (map (consumeArg . first subExpAliases) args)
  where consumeArg (als, Consume) = als
        consumeArg (_,   Observe) = mempty
consumedInExp (If _ tb fb _) =
  consumedInBody tb <> consumedInBody fb
consumedInExp (LoopOp (DoLoop _ merge _ _)) =
  mconcat (map (subExpAliases . snd) $
           filter (unique . paramType . fst) merge)
consumedInExp (LoopOp (Map _ _ lam arrs)) =
  consumedByLambda lam $ map vnameAliases arrs
consumedInExp (LoopOp (Reduce _ _ lam input)) =
  consumedByLambda lam $ map subExpAliases accs ++ map vnameAliases arrs
  where (accs, arrs) = unzip input
consumedInExp (LoopOp (Scan _ _ lam input)) =
  consumedByLambda lam $ map subExpAliases accs ++ map vnameAliases arrs
  where (accs, arrs) = unzip input
consumedInExp (LoopOp (Redomap _ _ _ lam accs arrs)) =
  consumedByLambda lam $ map subExpAliases accs ++ map vnameAliases arrs
consumedInExp _ = mempty

consumedByLambda :: Lambda lore -> [Names] -> Names
consumedByLambda lam param_als =
  mconcat [ als
          | (param, als) <- zip (lambdaParams lam) param_als,
            unique $ paramType param ]

consumedInPattern :: Pattern lore -> Names
consumedInPattern pat =
  mconcat (map (consumedInBindage . patElemBindage) $
           patternContextElements pat ++ patternValueElements pat)
  where consumedInBindage BindVar = mempty
        consumedInBindage (BindInPlace _ src _) = vnameAliases src
