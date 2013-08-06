{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
--
-- Perform simple hoisting of let-bindings, moving them out as far as
-- possible (particularly outside loops).
--
module L0C.EnablingOpts.Hoisting
  ( transformProg )
  where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Reader

import Data.Graph
import Data.Loc
import qualified Data.Map as M
import qualified Data.Set as S

import L0C.L0

data BindNeed = LoopBind TupIdent Exp Ident Exp Exp
              | LetBind TupIdent Exp [Exp]
              | LetWithBind Ident Ident [Exp] Exp
                deriving (Show, Eq, Ord)

asTail :: BindNeed -> Exp
asTail (LoopBind mergepat mergeexp i bound loopbody) =
  DoLoop mergepat mergeexp i bound loopbody (TupLit [] loc) loc
    where loc = srclocOf mergepat
asTail (LetBind pat e _) =
  LetPat pat e (TupLit [] loc) loc
    where loc = srclocOf pat
asTail (LetWithBind dest src is ve) =
  LetWith dest src is ve (Var dest) $ srclocOf dest

requires :: BindNeed -> S.Set VName
requires = S.map identName . freeInExp . asTail

provides :: BindNeed -> S.Set VName
provides (LoopBind mergepat _ _ _ _) = patNames mergepat
provides (LetBind pat _ _) = patNames pat
provides (LetWithBind dest _ _ _) = S.singleton $ identName dest

data Need = Need { needBindings :: S.Set BindNeed
                 }

instance Monoid Need where
  Need b1 `mappend` Need b2 = Need $ b1 <> b2
  mempty = Need S.empty

data Env = Env { envBindings :: M.Map TupIdent Exp
               }

emptyEnv :: Env
emptyEnv = Env {
             envBindings = M.empty
           }

newtype HoistM a = HoistM (WriterT Need (Reader Env) a)
  deriving (Applicative, Functor, Monad,
            MonadWriter Need, MonadReader Env)

runHoistM :: HoistM a -> (a, Need)
runHoistM (HoistM m) = runReader (runWriterT m) emptyEnv

bind :: BindNeed -> HoistM ()
bind = tell . Need . S.singleton

-- | Run the let-hoisting algorithm on the given program.  Even if the
-- output differs from the output, meaningful hoisting may not have
-- taken place - the order of bindings may simply have been
-- rearranged.  The function is idempotent, however.
transformProg :: Prog -> Prog
transformProg = Prog . map transformFun . progFunctions

transformFun :: FunDec -> FunDec
transformFun (fname, rettype, params, body, loc) =
  (fname, rettype, params, addBindings need body', loc)
  where (body', need) = runHoistM $ hoistInExp body

addBindings :: Need -> Exp -> Exp
addBindings need body =
  foldr comb body $ inDepOrder $ S.toList $ needBindings need
  where comb (LoopBind mergepat mergeexp loopvar
              boundexp loopbody) inner =
          DoLoop mergepat mergeexp loopvar boundexp loopbody inner
                   $ srclocOf inner
        comb (LetBind pat e _) inner =
          LetPat pat e inner $ srclocOf inner
        comb (LetWithBind dest src is ve) inner =
          LetWith dest src is ve inner $ srclocOf inner

inDepOrder :: [BindNeed] -> [BindNeed]
inDepOrder = flattenSCCs . stronglyConnComp . buildGraph
  where buildGraph bnds =
          [ (bnd, provides bnd, deps) |
            bnd <- bnds,
            let deps = [ provides dep | dep <- bnds, dep `mustPrecede` bnd ] ]

mustPrecede :: BindNeed -> BindNeed -> Bool
bnd1 `mustPrecede` bnd2 =
  not $ S.null $ (provides bnd1 `S.intersection` requires bnd2) `S.union`
                 (consumedInExp e2 `S.intersection` requires bnd1)
  where e2 = asTail bnd2

anyIsFreeIn :: S.Set VName -> Exp -> Bool
anyIsFreeIn ks = (ks `intersects`) . S.map identName . freeInExp

intersects :: Ord a => S.Set a -> S.Set a -> Bool
intersects a b = not $ S.null $ a `S.intersection` b

type BlockPred = Exp -> BindNeed -> Bool

orIf :: BlockPred -> BlockPred -> BlockPred
orIf p1 p2 body need = p1 body need || p2 body need

blockIfSeq :: [BlockPred] -> HoistM Exp -> HoistM Exp
blockIfSeq ps m = foldl (flip blockIf) m ps

blockIf :: BlockPred -> HoistM Exp -> HoistM Exp
blockIf block m = pass $ do
  (body, Need needs) <- listen m
  let (blocked, hoistable, _) =
        foldl (split body) (S.empty, S.empty, S.empty) $
        inDepOrder $ S.toList needs
  return (addBindings (Need blocked) body, const $ Need hoistable)
  where split body (blocked, hoistable, ks) need =
          case need of
            LetBind pat e es ->
              let bad e' = block body (LetBind pat e' []) || ks `anyIsFreeIn` e'
              in case (bad e, filter (not . bad) es) of
                   (True, [])     ->
                     (need `S.insert` blocked, hoistable,
                      patNames pat `S.union` ks)
                   (True, e':es') ->
                     (blocked, LetBind pat e' es' `S.insert` hoistable, ks)
                   (False, es')   ->
                     (blocked, LetBind pat e es' `S.insert` hoistable, ks)
            _ | requires need `intersects` ks || block body need ->
                (need `S.insert` blocked, hoistable, provides need `S.union` ks)
              | otherwise ->
                (blocked, need `S.insert` hoistable, ks)

blockAllHoisting :: HoistM Exp -> HoistM Exp
blockAllHoisting = blockIf $ \_ _ -> True

hasFree :: S.Set VName -> BlockPred
hasFree ks _ need = ks `intersects` requires need

uniqPat :: TupIdent -> Bool
uniqPat (Id k)         = unique $ identType k
uniqPat (TupId pats _) = any uniqPat pats

isUniqueBinding :: BlockPred
isUniqueBinding _ (LoopBind pat _ _ _ _)   = uniqPat pat
isUniqueBinding _ (LetBind pat _ _)        = uniqPat pat
isUniqueBinding _ (LetWithBind dest _ _ _) = unique $ identType dest

isConsumed :: BlockPred
isConsumed body need =
  provides need `intersects` consumedInExp body

hoistInExp :: Exp -> HoistM Exp
hoistInExp (If c e1 e2 loc) = do
  c' <- hoistInExp c
  e1' <- blockAllHoisting $ hoistInExp e1
  e2' <- blockAllHoisting $ hoistInExp e2
  return $ If c' e1' e2' loc
hoistInExp (LetPat pat e body _) = do
  e' <- hoistInExp e
  bind $ LetBind pat e' []
  hoistInExp body
hoistInExp (LetWith dest src idxs ve body _) = do
  idxs' <- mapM hoistInExp idxs
  ve' <- hoistInExp ve
  bind $ LetWithBind dest src idxs' ve'
  hoistInExp body
hoistInExp (DoLoop mergepat mergeexp loopvar boundexp loopbody letbody _) = do
  mergeexp' <- hoistInExp mergeexp
  boundexp' <- hoistInExp boundexp
  loopbody' <- blockIfSeq [hasFree boundnames, isConsumed] $
               hoistInExp loopbody
  bind $ LoopBind mergepat mergeexp' loopvar boundexp' loopbody'
  hoistInExp letbody
  where boundnames = identName loopvar `S.insert` patNames mergepat
hoistInExp e = mapExpM hoist e
  where hoist = identityMapper {
                  mapOnExp = hoistInExp
                , mapOnLambda = hoistInLambda
                }

hoistInLambda :: Lambda -> HoistM Lambda
hoistInLambda (CurryFun fname args rettype loc) = do
  args' <- mapM hoistInExp args
  return $ CurryFun fname args' rettype loc
hoistInLambda (AnonymFun params body rettype loc) = do
  body' <- blockIf (hasFree params' `orIf` isUniqueBinding) $ hoistInExp body
  return $ AnonymFun params body' rettype loc
  where params' = S.fromList $ map identName params
