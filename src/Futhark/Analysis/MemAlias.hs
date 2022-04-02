{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Analysis.MemAlias
  ( analyzeSeqMem,
    analyzeGPUMem,
    canBeSameMemory,
    aliasesOf,
    MemAliases,
  )
where

import Control.Monad.Reader
import Data.Bifunctor
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Futhark.IR.GPUMem
import Futhark.IR.SeqMem
import Futhark.Util
import Futhark.Util.Pretty

-- For our purposes, memory aliases are a bijective function: If @a@ aliases
-- @b@, @b@ also aliases @a@. However, this relationship is not transitive. Consider for instance the following:
--
-- @
--   let xs@mem_1 =
--     if ... then
--       replicate i 0 @ mem_2
--     else
--       replicate j 1 @ mem_3
-- @
--
-- Here, @mem_1@ aliases both @mem_2@ and @mem_3@, each of which alias @mem_1@
-- but not each other.
newtype MemAliases = MemAliases (M.Map VName Names)
  deriving (Show, Eq)

instance Semigroup MemAliases where
  (MemAliases m1) <> (MemAliases m2) = MemAliases $ M.unionWith (<>) m1 m2

instance Monoid MemAliases where
  mempty = MemAliases mempty

instance Pretty MemAliases where
  ppr (MemAliases m) = ppr m

addAlias :: VName -> VName -> MemAliases -> MemAliases
addAlias v1 v2 m =
  m <> singleton v1 (oneName v2) <> singleton v2 mempty

singleton :: VName -> Names -> MemAliases
singleton v ns = MemAliases $ M.singleton v ns

canBeSameMemory :: MemAliases -> VName -> VName -> Bool
canBeSameMemory (MemAliases m) v1 v2 =
  case fmap (v2 `nameIn`) (M.lookup v1 m) of
    Just True -> True
    Just False -> case fmap (v1 `nameIn`) (M.lookup v2 m) of
      Just b -> b
      Nothing -> error $ "VName not found in MemAliases: " <> pretty v2
    Nothing -> error $ "VName not found in MemAliases: " <> pretty v1

aliasesOf :: MemAliases -> VName -> Names
aliasesOf (MemAliases m) v = fromMaybe mempty $ M.lookup v m

isIn :: VName -> MemAliases -> Bool
isIn v (MemAliases m) = v `S.member` M.keysSet m

newtype Env inner = Env {onInner :: MemAliases -> inner -> MemAliasesM inner MemAliases}

type MemAliasesM inner a = Reader (Env inner) a

analyzeHostOp :: MemAliases -> HostOp GPUMem () -> MemAliasesM (HostOp GPUMem ()) MemAliases
analyzeHostOp m (SegOp (SegMap _ _ _ kbody)) =
  analyzeStms (kernelBodyStms kbody) m
analyzeHostOp m (SegOp (SegRed _ _ _ _ kbody)) =
  analyzeStms (kernelBodyStms kbody) m
analyzeHostOp m (SegOp (SegScan _ _ _ _ kbody)) =
  analyzeStms (kernelBodyStms kbody) m
analyzeHostOp m (SegOp (SegHist _ _ _ _ kbody)) =
  analyzeStms (kernelBodyStms kbody) m
analyzeHostOp _ _ = pure mempty

analyzeStm :: (Mem rep inner, LetDec rep ~ LetDecMem) => MemAliases -> Stm rep -> MemAliasesM inner MemAliases
analyzeStm m (Let (Pat [PatElem vname _]) _ (Op (Alloc _ _))) =
  pure $ m <> singleton vname mempty
analyzeStm m (Let _ _ (Op (Inner inner))) = do
  on_inner <- asks onInner
  on_inner m inner
analyzeStm m (Let pat _ (If _ then_body else_body _)) = do
  m' <-
    analyzeStms (bodyStms then_body) m
      >>= analyzeStms (bodyStms else_body)
  zip (patNames pat) (map resSubExp $ bodyResult then_body)
    <> zip (patNames pat) (map resSubExp $ bodyResult else_body)
    & mapMaybe (filterFun m')
    & foldr (uncurry addAlias) m'
    & pure
analyzeStm m (Let pat _ (DoLoop params _ body)) = do
  let m_init =
        map snd params
          & zip (patNames pat)
          & mapMaybe (filterFun m)
          & foldr (uncurry addAlias) m
      m_params =
        mapMaybe (filterFun m_init . first paramName) params
          & foldr (uncurry addAlias) m_init
  m_body <- analyzeStms (bodyStms body) m_params
  zip (patNames pat) (map resSubExp $ bodyResult body)
    & mapMaybe (filterFun m_body)
    & foldr (uncurry addAlias) m_body
    & pure
analyzeStm m _ = pure m

filterFun :: MemAliases -> (VName, SubExp) -> Maybe (VName, VName)
filterFun m' (v, Var v') | v' `isIn` m' = Just (v, v')
filterFun _ _ = Nothing

analyzeStms :: (Mem rep inner, LetDec rep ~ LetDecMem) => Stms rep -> MemAliases -> MemAliasesM inner MemAliases
analyzeStms =
  flip $ foldM analyzeStm

analyzeFun :: (Mem rep inner, LetDec rep ~ LetDecMem) => FunDef rep -> MemAliasesM inner MemAliases
analyzeFun f =
  funDefParams f
    & mapMaybe justMem
    & mconcat
    & analyzeStms (bodyStms $ funDefBody f)
  where
    justMem (Param _ v (MemMem _)) = Just $ singleton v mempty
    justMem _ = Nothing

transitiveClosure :: MemAliases -> MemAliases
transitiveClosure ma@(MemAliases m) =
  M.foldMapWithKey
    ( \k ns ->
        namesToList ns
          & foldMap (aliasesOf ma)
          & singleton k
    )
    m
    <> ma

analyzeSeqMem :: Prog SeqMem -> MemAliases
analyzeSeqMem prog = completeBijection $ runReader (analyze prog) $ Env $ \x _ -> pure x

analyzeGPUMem :: Prog GPUMem -> MemAliases
analyzeGPUMem prog = completeBijection $ runReader (analyze prog) $ Env analyzeHostOp

analyze :: (Mem rep inner, LetDec rep ~ LetDecMem) => Prog rep -> MemAliasesM inner MemAliases
analyze prog =
  progFuns prog
    & foldM (\m f -> (<>) m <$> analyzeFun f) (MemAliases mempty)
    <&> fixPoint transitiveClosure

completeBijection :: MemAliases -> MemAliases
completeBijection ma@(MemAliases m) =
  M.foldMapWithKey (\k ns -> foldMap (`singleton` oneName k) (namesToList ns)) m <> ma
