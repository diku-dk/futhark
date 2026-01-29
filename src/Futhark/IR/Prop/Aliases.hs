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
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Futhark.IR.Prop (ASTRep, IsOp, NameInfo (..), Scope)
import Futhark.IR.Prop.Names
import Futhark.IR.Prop.Pat
import Futhark.IR.Prop.Types
import Futhark.IR.Syntax

-- | The class of representations that contain aliasing information.
class (ASTRep rep, AliasedOp (OpC rep), AliasesOf (LetDec rep)) => Aliased rep where
  -- | The aliases of the body results.  Note that this includes names
  -- bound in the body!
  bodyAliases :: Body rep -> [Names]

  -- | The variables consumed in the body.
  consumedInBody :: GBody rep res -> Names

vnameAliases :: VName -> Names
vnameAliases = oneName

-- | The aliases of a subexpression.
subExpAliases :: SubExp -> Names
subExpAliases Constant {} = mempty
subExpAliases (Var v) = vnameAliases v

basicOpAliases :: BasicOp -> [Names]
basicOpAliases (SubExp se) = [subExpAliases se]
basicOpAliases (Opaque _ se) = [subExpAliases se]
basicOpAliases (ArrayVal _ _) = [mempty]
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
basicOpAliases (Reshape v _) = [vnameAliases v]
basicOpAliases (Rearrange v _) = [vnameAliases v]
basicOpAliases Concat {} = [mempty]
basicOpAliases Manifest {} = [mempty]
basicOpAliases Assert {} = [mempty]
basicOpAliases UpdateAcc {} = [mempty]
basicOpAliases UserParam {} = [mempty]

matchAliases :: [([Names], Names)] -> [Names]
matchAliases l =
  map ((`namesSubtract` mconcat conses) . mconcat) $ transpose alses
  where
    (alses, conses) = unzip l

funcallAliases ::
  [PatElem dec] ->
  [(SubExp, Diet)] ->
  [(TypeBase shape Uniqueness, RetAls)] ->
  [Names]
funcallAliases pes args = map onType
  where
    -- We assumes that the pals/rals lists are sorted, as this allows
    -- us to compute the intersections much more efficiently.
    argAls (i, (Var v, Observe)) = Just (i, v)
    argAls _ = Nothing
    arg_als = mapMaybe argAls $ zip [0 ..] args
    res_als = zip [0 ..] $ map patElemName pes
    pick (i : is) ((j, v) : jvs)
      | i == j = v : pick is jvs
      | i > j = pick (i : is) jvs
      | otherwise = pick is ((j, v) : jvs)
    pick _ _ = []
    getAls als is = namesFromList $ pick is als
    onType (_t, RetAls pals rals) = getAls arg_als pals <> getAls res_als rals

mutualAliases :: Names -> [PatElem dec] -> [Names] -> [Names]
mutualAliases bound pes als = zipWith grow (map patElemName pes) als
  where
    bound_als = map (`namesIntersection` bound) als
    grow v names = (names <> pe_names) `namesSubtract` bound
      where
        pe_names =
          namesFromList
            . filter (/= v)
            . map (patElemName . fst)
            . filter (namesIntersect names . snd)
            $ zip pes bound_als

-- | The aliases of an expression, one for each pattern element.
--
-- The pattern is important because some aliasing might be through
-- variables that are no longer in scope (consider the aliases for a
-- body that returns the same value multiple times).
expAliases :: (Aliased rep) => [PatElem dec] -> Exp rep -> [Names]
expAliases pes (Match _ cases defbody _) =
  -- Repeat mempty in case the pattern has more elements (this
  -- implies a type error).
  mutualAliases bound pes $ als ++ repeat mempty
  where
    als = matchAliases $ onBody defbody : map (onBody . caseBody) cases
    onBody body = (bodyAliases body, consumedInBody body)
    bound = foldMap boundInBody $ defbody : map caseBody cases
expAliases _ (BasicOp op) = basicOpAliases op
expAliases pes (Loop merge _ loopbody) =
  mutualAliases (bound <> param_names) pes $ do
    (p, als) <-
      transitive . zip params $ zipWith (<>) arg_aliases (bodyAliases loopbody)
    if unique $ paramDeclType p
      then pure mempty
      else pure als
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
expAliases pes (Apply _ args t _) =
  funcallAliases pes args $ map (first declExtTypeOf) t
expAliases _ (WithAcc inputs lam) =
  concatMap inputAliases inputs
    ++ drop num_accs (map (`namesSubtract` boundInBody body) $ bodyAliases body)
  where
    body = lambdaBody lam
    inputAliases (_, arrs, _) = replicate (length arrs) mempty
    num_accs = length inputs
expAliases _ (Op op) = opAliases op

-- | The variables consumed in this statement.
consumedInStm :: (Aliased rep) => Stm rep -> Names
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
consumedInExp (Loop merge _ _) =
  mconcat
    ( map (subExpAliases . snd) $
        filter (unique . paramDeclType . fst) merge
    )
consumedInExp (WithAcc inputs lam) =
  mconcat (map inputConsumed inputs)
    <> ( consumedByLambda lam
           `namesSubtract` namesFromList (map paramName (lambdaParams lam))
       )
  where
    inputConsumed (_, arrs, _) = namesFromList arrs
consumedInExp (BasicOp (Update _ src _ _)) = oneName src
consumedInExp (BasicOp (FlatUpdate src _ _)) = oneName src
consumedInExp (BasicOp (UpdateAcc _ acc _ _)) = oneName acc
consumedInExp (BasicOp _) = mempty
consumedInExp (Op op) = consumedInOp op

-- | The variables consumed by this lambda.
consumedByLambda :: (Aliased rep) => Lambda rep -> Names
consumedByLambda = consumedInBody . lambdaBody

-- | The aliases of each pattern element.
patAliases :: (AliasesOf dec) => Pat dec -> [Names]
patAliases = map aliasesOf . patElems

-- | Something that contains alias information.
class AliasesOf a where
  -- | The alias of the argument element.
  aliasesOf :: a -> Names

instance AliasesOf Names where
  aliasesOf = id

instance (AliasesOf dec) => AliasesOf (PatElem dec) where
  aliasesOf = aliasesOf . patElemDec

-- | Also includes the name itself.
lookupAliases :: (AliasesOf (LetDec rep)) => VName -> Scope rep -> Names
lookupAliases root scope =
  -- We must be careful to handle circular aliasing properly (this
  -- can happen due to Match and Loop).
  expand mempty root
  where
    expand prev v =
      case M.lookup v scope of
        Just (LetName dec) ->
          oneName v
            <> foldMap
              (expand (oneName v <> prev))
              (filter (`notNameIn` prev) (namesToList (aliasesOf dec)))
        _ -> oneName v

-- | The class of operations that can produce aliasing and consumption
-- information.
class (IsOp op) => AliasedOp op where
  opAliases :: (Aliased rep) => op rep -> [Names]
  consumedInOp :: (Aliased rep) => op rep -> Names

instance AliasedOp NoOp where
  opAliases NoOp = []
  consumedInOp NoOp = mempty

-- | Pre-existing aliases for variables.  Used to add transitive
-- aliases.
type AliasTable = M.Map VName Names
