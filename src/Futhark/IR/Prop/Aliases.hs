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
  )
where

import Data.Bifunctor (first, second)
import Data.List (find, transpose)
import Data.Map qualified as M
import Futhark.IR.Prop (ASTRep, IsOp, NameInfo (..), Scope)
import Futhark.IR.Prop.Names
import Futhark.IR.Prop.Patterns
import Futhark.IR.Prop.Types
import Futhark.IR.Syntax

-- | The class of representations that contain aliasing information.
class (ASTRep rep, AliasedOp (Op rep), AliasesOf (LetDec rep)) => Aliased rep where
  -- | The aliases of the body results.  Note that this includes names
  -- bound in the body!
  bodyAliases :: Body rep -> [Names]

  -- | The variables consumed in the body.
  consumedInBody :: Body rep -> Names

vnameAliases :: VName -> Names
vnameAliases = oneName

-- | The aliases of a subexpression.
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
basicOpAliases (Reshape _ _ e) = [vnameAliases e]
basicOpAliases (Rearrange _ e) = [vnameAliases e]
basicOpAliases (Rotate _ e) = [vnameAliases e]
basicOpAliases Concat {} = [mempty]
basicOpAliases Copy {} = [mempty]
basicOpAliases Manifest {} = [mempty]
basicOpAliases Assert {} = [mempty]
basicOpAliases UpdateAcc {} = [mempty]

matchAliases :: [([Names], Names)] -> [Names]
matchAliases l =
  map ((`namesSubtract` mconcat conses) . mconcat) $ transpose alses
  where
    (alses, conses) = unzip l

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

funcallAliases :: [(SubExp, Diet)] -> [TypeBase shape Uniqueness] -> [Names]
funcallAliases args t =
  returnAliases t [(subExpAliases se, d) | (se, d) <- args]

-- | The aliases of an expression, one for each pattern element.
--
-- The pattern is important because some aliasing might be through
-- variables that are no longer in scope (consider the aliases for a
-- body that returns the same value multiple times).
expAliases :: (Aliased rep) => [PatElem dec] -> Exp rep -> [Names]
expAliases pes (Match _ cases defbody _) =
  -- Repeat mempty in case the pattern has more elements (this
  -- implies a type error).
  zipWith grow (map patElemName pes) $ als ++ repeat mempty
  where
    als = matchAliases $ onBody defbody : map (onBody . caseBody) cases
    onBody body = (bodyAliases body, consumedInBody body)
    bound = foldMap boundInBody $ defbody : map caseBody cases
    grow v names = (names <> pe_names) `namesSubtract` bound
      where
        pe_names =
          namesFromList
            . filter (/= v)
            . map (patElemName . fst)
            . filter (namesIntersect names . snd)
            $ zip pes als
expAliases _ (BasicOp op) = basicOpAliases op
expAliases _ (DoLoop merge _ loopbody) = do
  (p, als) <-
    transitive . zip params $ zipWith mappend arg_aliases (bodyAliases loopbody)
  let als' = als `namesSubtract` param_names
  if unique $ paramDeclType p
    then pure mempty
    else pure $ als' `namesSubtract` bound
  where
    bound = boundInBody loopbody
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
expAliases _ (Apply _ args t _) =
  funcallAliases args $ map declExtTypeOf t
expAliases _ (WithAcc inputs lam) =
  concatMap inputAliases inputs
    ++ drop num_accs (map (`namesSubtract` boundInBody body) $ bodyAliases body)
  where
    body = lambdaBody lam
    inputAliases (_, arrs, _) = replicate (length arrs) mempty
    num_accs = length inputs
expAliases _ (Op op) = opAliases op

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
consumedInExp (Match _ cases defbody _) =
  foldMap (consumedInBody . caseBody) cases <> consumedInBody defbody
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

-- | The aliases of each pattern element.
patAliases :: AliasesOf dec => Pat dec -> [Names]
patAliases = map aliasesOf . patElems

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

instance AliasedOp (NoOp rep) where
  opAliases NoOp = []
  consumedInOp NoOp = mempty

-- | Pre-existing aliases for variables.  Used to add transitive
-- aliases.
type AliasTable = M.Map VName Names
