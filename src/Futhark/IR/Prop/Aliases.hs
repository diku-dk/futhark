{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

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
  ( subExpAliases,
    expAliases,
    patAliases,
    lookupAliases,
    Aliased (..),
    AliasesOf (..),

    -- * Consumption
    consumedInStm,
    consumedInExp,
    consumedByLambda,

    -- * Extensibility
    AliasTable,
    AliasedOp (..),
    CanBeAliased (..),
  )
where

import Data.Bifunctor (first, second)
import qualified Data.Kind
import Data.List (find)
import qualified Data.Map as M
import Futhark.IR.Prop (IsOp, NameInfo (..), Scope)
import Futhark.IR.Prop.Names
import Futhark.IR.Prop.Patterns
import Futhark.IR.Prop.Types
import Futhark.IR.Syntax

-- | The class of representations that contain aliasing information.
class (RepTypes rep, AliasedOp (Op rep), AliasesOf (LetDec rep)) => Aliased rep where
  -- | The aliases of the body results.
  bodyAliases :: Body rep -> [Names]

  -- | The variables consumed in the body.
  consumedInBody :: Body rep -> Names

vnameAliases :: VName -> Names
vnameAliases = oneName

-- | The alises of a subexpression.
subExpAliases :: SubExp -> Names
subExpAliases Constant {} = mempty
subExpAliases (Var v) = vnameAliases v

basicOpAliases :: BasicOp -> [Names]
basicOpAliases (SubExp se) = [subExpAliases se]
basicOpAliases (Opaque _ se) = [subExpAliases se]
basicOpAliases (ArrayLit _ _) = [mempty]
basicOpAliases BinOp {} = [mempty]
basicOpAliases ConvOp {} = [mempty]
basicOpAliases CmpOp {} = [mempty]
basicOpAliases UnOp {} = [mempty]
basicOpAliases (Index ident _) = [vnameAliases ident]
basicOpAliases Update {} = [mempty]
basicOpAliases (FlatIndex ident _) = [vnameAliases ident]
basicOpAliases FlatUpdate {} = [mempty]
basicOpAliases Iota {} = [mempty]
basicOpAliases Replicate {} = [mempty]
basicOpAliases Scratch {} = [mempty]
basicOpAliases (Reshape _ e) = [vnameAliases e]
basicOpAliases (Rearrange _ e) = [vnameAliases e]
basicOpAliases (Rotate _ e) = [vnameAliases e]
basicOpAliases Concat {} = [mempty]
basicOpAliases Copy {} = [mempty]
basicOpAliases Manifest {} = [mempty]
basicOpAliases Assert {} = [mempty]
basicOpAliases UpdateAcc {} = [mempty]

ifAliases :: ([Names], Names) -> ([Names], Names) -> [Names]
ifAliases (als1, cons1) (als2, cons2) =
  map (`namesSubtract` cons) $ zipWith mappend als1 als2
  where
    cons = cons1 <> cons2

funcallAliases :: [(SubExp, Diet)] -> [TypeBase shape Uniqueness] -> [Names]
funcallAliases args t =
  returnAliases t [(subExpAliases se, d) | (se, d) <- args]

-- | The aliases of an expression, one per non-context value returned.
expAliases :: (Aliased rep) => Exp rep -> [Names]
expAliases (If _ tb fb dec) =
  drop (length all_aliases - length ts) all_aliases
  where
    ts = ifReturns dec
    all_aliases =
      ifAliases
        (bodyAliases tb, consumedInBody tb)
        (bodyAliases fb, consumedInBody fb)
expAliases (BasicOp op) = basicOpAliases op
expAliases (DoLoop merge _ loopbody) = do
  (p, als) <-
    transitive . zip params $ zipWith mappend arg_aliases (bodyAliases loopbody)
  let als' = als `namesSubtract` param_names
  if unique $ paramDeclType p
    then pure mempty
    else pure als'
  where
    arg_aliases = map (subExpAliases . snd) merge
    params = map fst merge
    param_names = namesFromList $ map paramName params
    transitive merge_and_als =
      let merge_and_als' = map (second expand) merge_and_als
       in if merge_and_als' == merge_and_als
            then merge_and_als
            else transitive merge_and_als'
      where
        look v = maybe mempty snd $ find ((== v) . paramName . fst) merge_and_als
        expand als = als <> foldMap look (namesToList als)
expAliases (Apply _ args t _) =
  funcallAliases args $ map declExtTypeOf t
expAliases (WithAcc inputs lam) =
  concatMap inputAliases inputs ++ drop num_accs (bodyAliases (lambdaBody lam))
  where
    inputAliases (_, arrs, _) = replicate (length arrs) mempty
    num_accs = length inputs
expAliases (Op op) = opAliases op

returnAliases :: [TypeBase shape Uniqueness] -> [(Names, Diet)] -> [Names]
returnAliases rts args = map returnType' rts
  where
    returnType' (Array _ _ Nonunique) =
      mconcat $ map (uncurry maskAliases) args
    returnType' (Array _ _ Unique) =
      mempty
    returnType' (Prim _) =
      mempty
    returnType' Acc {} =
      error "returnAliases Acc"
    returnType' Mem {} =
      mconcat $ map (uncurry maskAliases) args

maskAliases :: Names -> Diet -> Names
maskAliases _ Consume = mempty
maskAliases _ ObservePrim = mempty
maskAliases als Observe = als

-- | The variables consumed in this statement.
consumedInStm :: Aliased rep => Stm rep -> Names
consumedInStm = consumedInExp . stmExp

-- | The variables consumed in this expression.
consumedInExp :: (Aliased rep) => Exp rep -> Names
consumedInExp (Apply _ args _ _) =
  mconcat (map (consumeArg . first subExpAliases) args)
  where
    consumeArg (als, Consume) = als
    consumeArg _ = mempty
consumedInExp (If _ tb fb _) =
  consumedInBody tb <> consumedInBody fb
consumedInExp (DoLoop merge form body) =
  mconcat
    ( map (subExpAliases . snd) $
        filter (unique . paramDeclType . fst) merge
    )
    <> consumedInForm form
  where
    body_consumed = consumedInBody body
    varConsumed = (`nameIn` body_consumed) . paramName . fst
    consumedInForm (ForLoop _ _ _ loopvars) =
      namesFromList $ map snd $ filter varConsumed loopvars
    consumedInForm WhileLoop {} =
      mempty
consumedInExp (WithAcc inputs lam) =
  mconcat (map inputConsumed inputs)
    <> ( consumedByLambda lam
           `namesSubtract` namesFromList (map paramName (lambdaParams lam))
       )
  where
    inputConsumed (_, arrs, _) = namesFromList arrs
consumedInExp (BasicOp (Update _ src _ _)) = oneName src
consumedInExp (BasicOp (FlatUpdate src _ _)) = oneName src
consumedInExp (BasicOp (UpdateAcc acc _ _)) = oneName acc
consumedInExp (BasicOp _) = mempty
consumedInExp (Op op) = consumedInOp op

-- | The variables consumed by this lambda.
consumedByLambda :: Aliased rep => Lambda rep -> Names
consumedByLambda = consumedInBody . lambdaBody

-- | The aliases of each pattern element (including the context).
patAliases :: AliasesOf dec => Pat dec -> [Names]
patAliases = map (aliasesOf . patElemDec) . patElems

-- | Something that contains alias information.
class AliasesOf a where
  -- | The alias of the argument element.
  aliasesOf :: a -> Names

instance AliasesOf Names where
  aliasesOf = id

instance AliasesOf dec => AliasesOf (PatElem dec) where
  aliasesOf = aliasesOf . patElemDec

-- | Also includes the name itself.
lookupAliases :: AliasesOf (LetDec rep) => VName -> Scope rep -> Names
lookupAliases v scope =
  case M.lookup v scope of
    Just (LetName dec) ->
      oneName v <> foldMap (`lookupAliases` scope) (namesToList (aliasesOf dec))
    _ -> oneName v

-- | The class of operations that can produce aliasing and consumption
-- information.
class IsOp op => AliasedOp op where
  opAliases :: op -> [Names]
  consumedInOp :: op -> Names

instance AliasedOp () where
  opAliases () = []
  consumedInOp () = mempty

-- | Pre-existing aliases for variables.  Used to add transitive
-- aliases.
type AliasTable = M.Map VName Names

-- | The class of operations that can be given aliasing information.
-- This is a somewhat subtle concept that is only used in the
-- simplifier and when using "rep adapters".
class AliasedOp (OpWithAliases op) => CanBeAliased op where
  -- | The op that results when we add aliases to this op.
  type OpWithAliases op :: Data.Kind.Type

  -- | Remove aliases from this op.
  removeOpAliases :: OpWithAliases op -> op

  -- | Add aliases to this op.
  addOpAliases :: AliasTable -> op -> OpWithAliases op

instance CanBeAliased () where
  type OpWithAliases () = ()
  removeOpAliases = id
  addOpAliases = const id
