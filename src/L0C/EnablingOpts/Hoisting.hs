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
import Control.Monad.State
import Control.Monad.RWS

import Data.Graph
import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.Map as M
import Data.Ord
import qualified Data.Set as S

import L0C.L0
import L0C.FreshNames

data BindNeed = LoopBind TupIdent Exp Ident Exp Exp
              | LetBind TupIdent Exp [(TupIdent, Exp)]
              | LetWithBind Ident Ident [Exp] Exp
                deriving (Show, Eq, Ord)

type NeedSet = S.Set BindNeed

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
requires (LetBind pat e ((altpat,alte):alts)) =
  requires (LetBind pat e []) <> requires (LetBind altpat alte alts)
requires bnd = S.map identName $ freeInExp $ asTail bnd

provides :: BindNeed -> S.Set VName
provides (LoopBind mergepat _ _ _ _) = patNames mergepat
provides (LetBind pat _ _) = patNames pat
provides (LetWithBind dest _ _ _) = S.singleton $ identName dest

data Need = Need { needBindings :: NeedSet
                 }

instance Monoid Need where
  Need b1 `mappend` Need b2 = Need $ b1 <> b2
  mempty = Need S.empty

data ShapeBinding = DimSizes [Maybe Exp]
                    deriving (Show, Eq)

type ShapeMap = M.Map Ident [ShapeBinding]

data Env = Env { envBindings :: ShapeMap
               }

emptyEnv :: Env
emptyEnv = Env {
             envBindings = M.empty
           }

cartesian :: [[a]] -> [[a]]
cartesian [] = []
cartesian [x] = map (:[]) x
cartesian (x:xs) = [ x' : xs' | xs' <- cartesian xs, x' <- x ]

varExp :: Exp -> Maybe Ident
varExp (Var k) = Just k
varExp _       = Nothing

vars :: [Exp] -> [Ident]
vars = mapMaybe varExp

lookupShapeBindings :: Ident -> ShapeMap -> [ShapeBinding]
lookupShapeBindings = delve S.empty
  where
    delve s k m | k `S.member` s = []
                | otherwise =
                  case M.lookup k m of
                    Nothing -> []
                    Just shs -> shs ++ concatMap (recurse s m) shs
    recurse s m (DimSizes sz) =
      map DimSizes $ filter (not . all (==Nothing)) $
          cartesian $ map inspect sz
      where inspect (Just (Size i (Var k') _)) =
              case delve (k' `S.insert` s) k' m of
                [] -> [Nothing]
                l  -> map (fill i) l
            inspect _ = [Nothing]
    fill i (DimSizes sz) = case drop i sz of
                             e:_ -> e
                             []  -> Nothing

sameShape :: ShapeMap -> Ident -> Ident -> Bool
sameShape m x y =
  any matches $ lookupShapeBindings x m
    where matches (DimSizes sz) =
            all ok $ zip sz [0..]
          ok (Just (Size i (Var v) _), j) =
            i == j && v == y
          ok _ = False

slices :: Ident -> ShapeMap -> [(Ident, Int)]
slices x m = mapMaybe isSlice $ lookupShapeBindings x m
  where isSlice (DimSizes (Just (Size i (Var y) _):sz))
          | all (isDimOf y) $ zip sz [i+1..] = Just (y, i)
          | otherwise = Nothing
        isSlice _ = Nothing
        isDimOf y (Just (Size i (Var z) _), j) =
          y == z && i == j
        isDimOf _ _ = False

sliceOf :: Ident -> ShapeMap -> [Ident]
sliceOf x m = map fst $ slices x m

copyOf :: Ident -> ShapeMap -> [Ident]
copyOf x m = mapMaybe isCopy $ slices x m
  where isCopy (y, 0) = Just y
        isCopy (_, _) = Nothing

newtype HoistM a = HoistM (RWS
                           Env                -- Reader
                           Need               -- Writer
                           (NameSource VName) -- State
                           a)
  deriving (Applicative, Functor, Monad,
            MonadWriter Need, MonadReader Env, MonadState (NameSource VName))

runHoistM :: HoistM a -> NameSource VName -> a
runHoistM (HoistM m) src = let (x, _, _) = runRWS m emptyEnv src
                           in x

new :: String -> HoistM VName
new k = do (name, src) <- gets $ flip newVName k
           put src
           return name

addNewBinding :: String -> Exp -> HoistM Ident
addNewBinding k e = do
  k' <- new k
  let ident = Ident { identName = k'
                    , identType = typeOf e
                    , identSrcLoc = srclocOf e }
  addBinding (Id ident) e
  return ident

addBinding :: TupIdent -> Exp -> HoistM ()
addBinding pat e@(Size i (Var x) _) = do
  let mkAlt (DimSizes ses) = case drop i ses of
                               Just se:_ -> [(pat, se)]
                               _        -> []
  alts <- concatMap mkAlt <$> asks (lookupShapeBindings x . envBindings)
  addSeveralBindings pat e alts

addBinding pat e@(Apply fname args t loc)
   | "assertZip" <- nameToString fname,
     Just args' <- mapM (varExp . fst) args = do
  bindings <- asks envBindings
  let cps = cartesian $ map (\a -> a : copyOf a bindings) args'
      sls = filter allSameShape $
            cartesian $ map (`sliceOf` bindings) args'
      allSameShape [] = True
      allSameShape (a:as) = all (sameShape bindings a) as
  case cps ++ sls of
    [] -> addSingleBinding pat e
    ls -> addSeveralBindings pat e
          [ (pat, Apply fname (map (\a -> (Var a, Observe)) l) t loc)
            | l <- ls ]

addBinding pat e = addSingleBinding pat e

addSingleBinding :: TupIdent -> Exp -> HoistM ()
addSingleBinding pat e = addSeveralBindings pat e []

addSeveralBindings :: TupIdent -> Exp -> [(TupIdent, Exp)] -> HoistM ()
addSeveralBindings pat e alts =
  tell $ Need $ S.singleton $ LetBind pat e alts

bindLet :: TupIdent -> Exp -> HoistM a -> HoistM a
bindLet (Id dest) (Var src) m = do
  addBinding (Id dest) (Var src)
  case identType src of Array {} -> withShape dest (slice 0 src) m
                        _        -> m

bindLet pat@(Id dest) e@(Iota (Var x) _) m = do
  addBinding pat e
  withShape dest [Var x] m

bindLet pat@(Id dest) e@(Replicate (Var x) _ _) m = do
  addBinding pat e
  withShape dest [Var x] m

bindLet pat@(TupId [dest1, dest2] _) e@(Split (Var n) (Var src) _ loc) m = do
  addBinding pat e
  src_sz <- addNewBinding "split_src_sz" $ Size 0 (Var src) loc
  split_sz <- addNewBinding "split_sz" $
              BinOp Minus (Var src_sz) (Var n) (Elem Int) loc
  withShapes [(dest1, Var n : rest),
              (dest2, Var split_sz : rest)] m
    where rest = [ Size i (Var src) loc
                   | i <- [1.. arrayDims (identType src) - 1]]

bindLet pat@(Id dest) e@(Concat (Var x) (Var y) loc) m = do
  concat_x <- addNewBinding "concat_x" $ Size 0 (Var x) loc
  concat_y <- addNewBinding "concat_y" $ Size 0 (Var y) loc
  concat_sz <- addNewBinding "concat_sz" $
               BinOp Plus (Var concat_x) (Var concat_y) (Elem Int) loc
  withShape dest (Var concat_sz :
                  [Size i (Var x) loc | i <- [1..arrayDims (identType x) - 1]]
                 ) $ addBinding pat e >> m

bindLet pat e@(Map2 _ srcs _ _) m = do
  addBinding pat e
  withShapes (sameOuterShapes $ S.toList (patIdents pat) ++ vars srcs) m

bindLet pat e@(Reduce2 _ _ srcs _ _) m = do
  addBinding pat e
  withShapes (sameOuterShapesExps srcs) m

bindLet pat e@(Scan2 _ _ srcs _ _) m = do
  addBinding pat e
  withShapes (sameOuterShapesExps srcs) m

bindLet pat e@(Filter2 _ srcs _) m = do
  addBinding pat e
  withShapes (sameOuterShapesExps srcs) m

bindLet pat e@(Redomap2 _ _ _ srcs _ _) m = do
  addBinding pat e
  withShapes (sameOuterShapesExps srcs) m

bindLet pat@(Id dest) e@(Index src idxs _ _) m = do
  addBinding pat e
  withShape dest (slice (length idxs) src) m

bindLet pat@(Id dest) e@(Transpose k n (Var src) loc) m = do
  addBinding pat e
  withShape dest dims m
    where dims = transposeIndex k n
                 [Size i (Var src) loc
                  | i <- [0..arrayDims (identType src) - 1]]

bindLet pat e m = do
  addBinding pat e
  m

bindLetWith :: Ident -> Ident -> [Exp] -> Exp -> HoistM a -> HoistM a
bindLetWith dest src is ve m = do
  tell $ Need $ S.singleton $ LetWithBind dest src is ve
  withShape dest (slice 0 src) m

bindLoop :: TupIdent -> Exp -> Ident -> Exp -> Exp -> HoistM a -> HoistM a
bindLoop pat e i bound body m = do
  tell $ Need $ S.singleton $ LoopBind pat e i bound body
  m

slice :: Int -> Ident -> [Exp]
slice d k = [ Size i (Var k) $ srclocOf k
              | i <- [d..arrayDims (identType k)-1]]

withShape :: Ident -> [Exp] -> HoistM a -> HoistM a
withShape dest src =
  local (\env -> env { envBindings =
                         M.insertWith (++) dest [DimSizes $ map Just src]
                            $ envBindings env })

withShapes :: [(TupIdent, [Exp])] -> HoistM a -> HoistM a
withShapes [] m =
  m
withShapes ((Id dest, es):rest) m =
  withShape dest es $ withShapes rest m
withShapes (_:rest) m =
  withShapes rest m

sameOuterShapesExps :: [Exp] -> [(TupIdent, [Exp])]
sameOuterShapesExps = sameOuterShapes . vars

sameOuterShapes :: [Ident] -> [(TupIdent, [Exp])]
sameOuterShapes = outer' []
  where outer' _ [] = []
        outer' prev (k:ks) =
          [ (Id k, [Size 0 (Var k') $ srclocOf k])
            | k' <- prev ++ ks ] ++
          outer' (k:prev) ks

-- | Run the let-hoisting algorithm on the given program.  Even if the
-- output differs from the output, meaningful hoisting may not have
-- taken place - the order of bindings may simply have been
-- rearranged.  The function is idempotent, however.
transformProg :: Prog -> Prog
transformProg prog =
  Prog $ runHoistM (mapM transformFun $ progFunctions prog) namesrc
  where namesrc = newNameSourceForProg prog

transformFun :: FunDec -> HoistM FunDec
transformFun (fname, rettype, params, body, loc) = do
  body' <- blockAllHoisting $ hoistInExp body
  return (fname, rettype, params, body', loc)

addBindings :: Need -> Exp -> Exp
addBindings need =
  snd $ foldl comb (M.empty, id) (inDepOrder $ S.toList $ needBindings need)
  where comb (m, outer) bind@(LoopBind mergepat mergeexp loopvar
                              boundexp loopbody) =
          (m `M.union` distances m bind,
           \inner -> outer $
                     DoLoop mergepat mergeexp loopvar boundexp
                     loopbody inner $ srclocOf inner)
        comb (m, outer) (LetBind pat e alts) =
          let add pat' e' =
                (m `M.union` distances m (LetBind pat' e' []),
                 \inner -> outer $ LetPat pat' e' inner $ srclocOf inner)
          in case map snd $ sortBy (comparing fst) $ map (score m) $ (pat,e):alts of
               (pat',e'):_ -> add pat' e'
               _           -> add pat e
        comb (m, outer) bind@(LetWithBind dest src is ve) =
          (m `M.union` distances m bind,
           \inner -> outer $ LetWith dest src is ve inner $ srclocOf inner)

score :: M.Map VName Int -> (TupIdent, Exp) -> (Int, (TupIdent, Exp))
score m (p, e) = (S.fold f 0 $ freeNamesInExp e, (p, e))
  where f k x = case M.lookup k m of
                  Just y  -> max x y
                  Nothing -> x

expCost :: Exp -> Int
expCost (Map {}) = 1
expCost (Map2 {}) = 1
expCost (Filter {}) = 1
expCost (Filter2 {}) = 1
expCost (Reduce {}) = 1
expCost (Reduce2 {}) = 1
expCost (Scan {}) = 1
expCost (Scan2 {}) = 1
expCost (Redomap {}) = 1
expCost (Redomap2 {}) = 1
expCost (Transpose {}) = 1
expCost (Copy {}) = 1
expCost (Concat {}) = 1
expCost (Split {}) = 1
expCost (Reshape {}) = 1
expCost (DoLoop {}) = 1
expCost _ = 0

distances :: M.Map VName Int -> BindNeed -> M.Map VName Int
distances m need = M.fromList [ (k, d+cost) | k <- S.toList outs ]
  where d = S.fold f 0 ins
        (outs, ins, cost) =
          case need of
            LetBind pat e _ ->
              (patNames pat, freeNamesInExp e, expCost e)
            LetWithBind dest src is ve ->
              (S.singleton $ identName dest,
               identName src `S.insert`
               mconcat (map freeNamesInExp (ve:is)),
               1)
            LoopBind pat mergeexp _ bound loopbody ->
              (patNames pat,
               mconcat $ map freeNamesInExp [mergeexp, bound, loopbody],
               1)
        f k x = case M.lookup k m of
                  Just y  -> max x y
                  Nothing -> x

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
                  badAlt (_, e') = bad e'
              in case (bad e, filter (not . badAlt) es) of
                   (True, [])     ->
                     (need `S.insert` blocked, hoistable,
                      patNames pat `S.union` ks)
                   (True, (pat', e'):es') ->
                     (blocked, LetBind pat' e' es' `S.insert` hoistable, ks)
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
uniqPat (Wildcard t _) = unique t
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
hoistInExp (If c e1 e2 t loc) = do
  c' <- hoistInExp c
  e1' <- blockAllHoisting $ hoistInExp e1
  e2' <- blockAllHoisting $ hoistInExp e2
  return $ If c' e1' e2' t loc
hoistInExp (LetPat pat e body _) = do
  e' <- hoistInExp e
  bindLet pat e' $ hoistInExp body
hoistInExp (LetWith dest src idxs ve body _) = do
  idxs' <- mapM hoistInExp idxs
  ve' <- hoistInExp ve
  bindLetWith dest src idxs' ve' $ hoistInExp body
hoistInExp (DoLoop mergepat mergeexp loopvar boundexp loopbody letbody _) = do
  mergeexp' <- hoistInExp mergeexp
  boundexp' <- hoistInExp boundexp
  loopbody' <- blockIfSeq [hasFree boundnames, isConsumed] $
               hoistInExp loopbody
  bindLoop mergepat mergeexp' loopvar boundexp' loopbody' $ hoistInExp letbody
  where boundnames = identName loopvar `S.insert` patNames mergepat
hoistInExp e@(Map2 (AnonymFun params _ _ _) arrexps _ _) =
  hoistInSOAC e arrexps $ \ks ->
    withShapes (zip (map (Id . fromParam) params) $ map (slice 1) ks) $
    withShapes (sameOuterShapesExps arrexps) $
    hoistInExpBase e
hoistInExp e@(Reduce2 (AnonymFun params _ _ _) accexps arrexps _ _) =
  hoistInSOAC e arrexps $ \ks ->
    withShapes (zip (map (Id . fromParam) $ drop (length accexps) params)
               $ map (slice 1) ks) $
    withShapes (sameOuterShapesExps arrexps) $
    hoistInExpBase e
hoistInExp e@(Scan2 (AnonymFun params _ _ _) accexps arrexps _ _) =
  hoistInSOAC e arrexps $ \ks ->
    withShapes (zip (map (Id . fromParam) $ drop (length accexps) params)
               $ map (slice 1) ks) $
    withShapes (sameOuterShapesExps arrexps) $
    hoistInExpBase e
hoistInExp e@(Redomap2 (AnonymFun redparams _ _ _) (AnonymFun mapparams _ _ _)
              accexps arrexps _ _) =
  hoistInSOAC e arrexps $ \ks ->
    withShapes (zip (map (Id . fromParam) mapparams) $ map (slice 1) ks) $
    withShapes (zip (map (Id . fromParam) $ drop (length accexps) redparams)
               $ map (slice 1) ks) $
    withShapes (sameOuterShapesExps arrexps) $
    hoistInExpBase e
hoistInExp e = hoistInExpBase e

hoistInExpBase :: Exp -> HoistM Exp
hoistInExpBase = mapExpM hoist
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

arrVars :: [Exp] -> Maybe [Ident]
arrVars = mapM arrVars'
  where arrVars' (Var k) = Just k
        arrVars' _       = Nothing

hoistInSOAC :: Exp -> [Exp] -> ([Ident] -> HoistM Exp) -> HoistM Exp
hoistInSOAC e arrexps m =
  case arrVars arrexps of
    Nothing -> hoistInExpBase e
    Just ks -> m ks
