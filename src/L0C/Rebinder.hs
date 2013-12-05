{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
--
-- Perform a range of loosely connected low-level transformations
-- based on data dependency information.  This module will:
--
--    * Perform common-subexpression elimination (CSE).
--
--    * Rewrite expressions such that the dependecy path from the root
--    variables (the arguments to the function containing the
--    expression) is as short as possible.  For example, @size(0,b)@
--    will be rewritten to @size(0,a)@ if @b@ is the result of a @map@
--    on @a@, as @a@ and @b@ will in that case have the same number of
--    rows.
--
--    * Hoist expressions out of loops (including lambdas) and
--    branches.  This is done as aggressively as possible.
--
-- For this module to work properly, the input program should be fully
-- normalised; this can be accomplished through use of
-- "L0C.FullNormalizer".
--
-- CSE (and other transformations) may also create many bindings of
-- the form @let a=b@, so it is recommended to run copy propagation
-- after the rebinder.
--
module L0C.Rebinder
  ( transformProg
  , transformProgAggr
  )
  where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS

import Data.Graph
import Data.Hashable
import Data.List
import Data.Loc
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import Data.Ord
import qualified Data.HashSet as HS
import qualified Data.Set as S

import L0C.L0
import L0C.FreshNames

import L0C.Rebinder.CSE
import qualified L0C.Rebinder.SizeTracking as SZ

data BindNeed = LoopBind TupIdent Exp Ident Exp Exp
              | LetBind TupIdent Exp [Exp]
              | LetWithBind Certificates Ident Ident [Exp] Exp
                deriving (Show, Eq)

type NeedSet = [BindNeed]

asTail :: BindNeed -> Exp
asTail (LoopBind mergepat mergeexp i bound loopbody) =
  DoLoop mergepat mergeexp i bound loopbody (TupLit [] loc) loc
    where loc = srclocOf mergepat
asTail (LetBind pat e _) =
  LetPat pat e (TupLit [] loc) loc
    where loc = srclocOf pat
asTail (LetWithBind cs dest src is ve) =
  LetWith cs dest src is ve (Var dest) $ srclocOf dest

requires :: BindNeed -> HS.HashSet VName
requires (LetBind pat e (alte:alts)) =
  requires (LetBind pat e []) <> requires (LetBind pat alte alts)
requires bnd = HS.map identName $ freeInExp $ asTail bnd

provides :: BindNeed -> HS.HashSet VName
provides (LoopBind mergepat _ _ _ _) = patNameSet mergepat
provides (LetBind pat _ _) = patNameSet pat
provides (LetWithBind _ dest _ _ _) = HS.singleton $ identName dest

data Need = Need { needBindings :: NeedSet
                 }

instance Monoid Need where
  Need b1 `mappend` Need b2 = Need $ b1 <> b2
  mempty = Need []

data Env = Env { envBindings :: SZ.ShapeMap
               , envDupeState :: DupeState
               , envAggressive :: Bool
               }

emptyEnv :: Env
emptyEnv = Env {
             envBindings = HM.empty
           , envDupeState = newDupeState
           , envAggressive = False
           }

varExp :: Exp -> Maybe Ident
varExp (Var k) = Just k
varExp _       = Nothing

vars :: [Exp] -> [Ident]
vars = mapMaybe varExp

isArrayIdent :: Ident -> Bool
isArrayIdent idd = case identType idd of
                     Array {} -> True
                     _        -> False



newtype HoistM a = HoistM (RWS
                           Env                -- Reader
                           Need               -- Writer
                           (NameSource VName) -- State
                           a)
  deriving (Applicative, Functor, Monad,
            MonadWriter Need, MonadReader Env, MonadState (NameSource VName))

runHoistM :: HoistM a -> NameSource VName -> Env -> a
runHoistM (HoistM m) src env = let (x, _, _) = runRWS m env src
                               in x

new :: String -> HoistM VName
new k = do (name, src) <- gets $ flip newVName k
           put src
           return name

needThis :: BindNeed -> HoistM ()
needThis need = tell $ Need [need]

withNewBinding :: String -> Exp -> (Ident -> HoistM a) -> HoistM a
withNewBinding k e m = do
  k' <- new k
  let ident = Ident { identName = k'
                    , identType = typeOf e
                    , identSrcLoc = srclocOf e }
  withBinding (Id ident) e $ m ident

withBinding :: TupIdent -> Exp -> HoistM a -> HoistM a
withBinding pat e@(Size _ i (Var x) _) m = do
  let mkAlt es = case drop i es of
                   des:_ -> S.toList des
                   _     -> []
  alts <- mkAlt <$> asks (SZ.lookup x . envBindings)
  withSeveralBindings pat e alts m

withBinding pat e m = withSingleBinding pat e m

withSingleBinding :: TupIdent -> Exp -> HoistM a -> HoistM a
withSingleBinding pat e = withSeveralBindings pat e []

withSeveralBindings :: TupIdent -> Exp -> [Exp]
                    -> HoistM a -> HoistM a
withSeveralBindings pat e alts m = do
  ds <- asks envDupeState
  let (e', ds') = performCSE ds pat e
      (es, ds'') = performMultipleCSE ds pat alts
  needThis $ LetBind pat e' es
  local (\env -> env { envDupeState = ds' <> ds''}) m

bindLet :: TupIdent -> Exp -> HoistM a -> HoistM a
bindLet (Id dest) (Var src) m =
  withBinding (Id dest) (Var src) $
  case identType src of Array {} -> withShape dest (slice [] 0 src) m
                        _        -> m

bindLet pat@(Id dest) e@(Iota (Var x) _) m =
  withBinding pat e $
  withShape dest [Var x] m

bindLet pat@(Id dest) e@(Replicate (Var x) (Var y) _) m =
  withBinding pat e $
  withShape dest (Var x:slice [] 0 y) m

bindLet pat@(TupId [dest1, dest2] _) e@(Split cs (Var n) (Var src) _ loc) m =
  withBinding pat e $
  withNewBinding "split_src_sz" (Size cs 0 (Var src) loc) $ \src_sz ->
  withNewBinding "split_sz" (BinOp Minus (Var src_sz) (Var n) (Elem Int) loc) $ \split_sz ->
  withShapes [(dest1, Var n : rest),
              (dest2, Var split_sz : rest)] m
    where rest = [ Size cs i (Var src) loc
                   | i <- [1.. arrayDims (identType src) - 1]]

bindLet pat@(Id dest) e@(Concat cs (Var x) (Var y) loc) m =
  withBinding pat e $
  withNewBinding "concat_x" (Size cs 0 (Var x) loc) $ \concat_x ->
  withNewBinding "concat_y" (Size cs 0 (Var y) loc) $ \concat_y ->
  withNewBinding "concat_sz" (BinOp Plus (Var concat_x) (Var concat_y) (Elem Int) loc) $ \concat_sz ->
  withShape dest (Var concat_sz :
                  [Size cs i (Var x) loc
                     | i <- [1..arrayDims (identType x) - 1]])
  m

bindLet pat e@(Map2 cs _ srcs _ _) m =
  withBinding pat e $
  withShapes (sameOuterShapes cs $ patIdents pat ++ vars srcs) m

bindLet pat e@(Reduce2 cs _ _ srcs _ _) m =
  withBinding pat e $
  withShapes (sameOuterShapesExps cs srcs) m

bindLet pat e@(Scan2 cs _ _ srcs _ _) m =
  withBinding pat e $
  withShapes (sameOuterShapesExps cs srcs) m

bindLet pat e@(Filter2 cs _ srcs _) m =
  withBinding pat e $
  withShapes (sameOuterShapesExps cs srcs) m

bindLet pat e@(Redomap2 cs _ _ _ srcs _ _) m =
  withBinding pat e $
  withShapes (sameOuterShapesExps cs srcs) m

bindLet pat@(Id dest) e@(Index cs src _ idxs _ _) m =
  withBinding pat e $
  withShape dest (slice cs (length idxs) src) m

bindLet pat@(Id dest) e@(Transpose cs k n (Var src) loc) m =
  withBinding pat e $
  withShape dest dims m
    where dims = transposeIndex k n
                 [Size cs i (Var src) loc
                  | i <- [0..arrayDims (identType src) - 1]]

bindLet pat e m = withBinding pat e m

bindLetWith :: Certificates -> Ident -> Ident -> [Exp] -> Exp -> HoistM a -> HoistM a
bindLetWith cs dest src is ve m = do
  needThis $ LetWithBind cs dest src is ve
  withShape dest (slice [] 0 src) m

bindLoop :: TupIdent -> Exp -> Ident -> Exp -> Exp -> HoistM a -> HoistM a
bindLoop pat e i bound body m = do
  needThis $ LoopBind pat e i bound body
  m

slice :: Certificates -> Int -> Ident -> [Exp]
slice cs d k = [ Size cs i (Var k) $ srclocOf k
                 | i <- [d..arrayDims (identType k)-1]]

withShape :: Ident -> [Exp] -> HoistM a -> HoistM a
withShape dest src =
  local (\env -> env { envBindings = SZ.insert dest src $ envBindings env })

withShapes :: [(TupIdent, [Exp])] -> HoistM a -> HoistM a
withShapes [] m =
  m
withShapes ((Id dest, es):rest) m =
  withShape dest es $ withShapes rest m
withShapes (_:rest) m =
  withShapes rest m

sameOuterShapesExps :: Certificates -> [Exp] -> [(TupIdent, [Exp])]
sameOuterShapesExps cs = sameOuterShapes cs . vars

sameOuterShapes :: Certificates -> [Ident] -> [(TupIdent, [Exp])]
sameOuterShapes cs = outer' []
  where outer' _ [] = []
        outer' prev (k:ks) =
          [ (Id k, [Size cs 0 (Var k') $ srclocOf k])
            | k' <- prev ++ ks ] ++
          outer' (k:prev) ks

-- | Run the let-hoisting algorithm on the given program.  Even if the
-- output differs from the output, meaningful hoisting may not have
-- taken place - the order of bindings may simply have been
-- rearranged.  The function is idempotent, however.
transformProg :: Prog -> Prog
transformProg prog =
  Prog $ runHoistM (mapM transformFun $ progFunctions prog) namesrc env
  where namesrc = newNameSourceForProg prog
        env = emptyEnv {
                envAggressive = False
              }

-- | Like 'transformProg', but hoists more aggressively, which may
-- create new nuisances to fusion.  Hence it is best to run it after
-- fusion has been performed.
transformProgAggr :: Prog -> Prog
transformProgAggr prog =
  Prog $ runHoistM (mapM transformFun $ progFunctions prog) namesrc env
  where namesrc = newNameSourceForProg prog
        env = emptyEnv {
                envAggressive = True
              }

transformFun :: FunDec -> HoistM FunDec
transformFun (fname, rettype, params, body, loc) = do
  body' <- blockAllHoisting $ hoistInExp body
  return (fname, rettype, params, body', loc)

addBindings :: DupeState -> [BindNeed] -> Exp -> Exp
addBindings dupes needs =
  foldl (.) id $ snd $ mapAccumL comb (HM.empty, dupes) needs
  where comb (m,ds) bind@(LoopBind mergepat mergeexp loopvar
                              boundexp loopbody) =
          ((m `HM.union` distances m bind,ds),
           \inner -> DoLoop mergepat mergeexp loopvar boundexp
                     loopbody inner $ srclocOf inner)
        comb (m,ds) (LetBind pat e alts) =
          let add pat' e' =
                let (e'',ds') = performCSE ds pat' e'
                in ((m `HM.union` distances m (LetBind pat' e' []),ds'),
                    \inner -> LetPat pat' e'' inner $ srclocOf inner)
          in case map snd $ sortBy (comparing fst) $ map (score m) $ e:alts of
               e':_ -> add pat e'
               _    -> add pat e
        comb (m,ds) bind@(LetWithBind cs dest src is ve) =
          ((m `HM.union` distances m bind,ds),
           \inner -> LetWith cs dest src is ve inner $ srclocOf inner)

score :: HM.HashMap VName Int -> Exp -> (Int, Exp)
score m (Var k) =
  (fromMaybe (-1) $ HM.lookup (identName k) m, Var k)
score m e =
  (HS.foldl' f 0 $ freeNamesInExp e, e)
  where f x k = case HM.lookup k m of
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
expCost (Replicate {}) = 1
expCost _ = 0

distances :: HM.HashMap VName Int -> BindNeed -> HM.HashMap VName Int
distances m need = HM.fromList [ (k, d+cost) | k <- HS.toList outs ]
  where d = HS.foldl' f 0 ins
        (outs, ins, cost) =
          case need of
            LetBind pat e _ ->
              (patNameSet pat, freeNamesInExp e, expCost e)
            LetWithBind _ dest src is ve ->
              (HS.singleton $ identName dest,
               identName src `HS.insert`
               mconcat (map freeNamesInExp (ve:is)),
               1)
            LoopBind pat mergeexp _ bound loopbody ->
              (patNameSet pat,
               mconcat $ map freeNamesInExp [mergeexp, bound, loopbody],
               1)
        f x k = case HM.lookup k m of
                  Just y  -> max x y
                  Nothing -> x

inDepOrder :: [BindNeed] -> [BindNeed]
inDepOrder = flattenSCCs . stronglyConnComp . buildGraph
  where buildGraph bnds =
          [ (bnd, representative $ provides bnd, deps) |
            bnd <- bnds,
            let deps = [ representative $ provides dep
                         | dep <- bnds, dep `mustPrecede` bnd ] ]

        -- As all names are unique, a pattern can be uniquely
        -- represented by any of its names.  If the pattern has no
        -- names, then it doesn't matter anyway.
        representative s = case HS.toList s of
                             x:_ -> Just x
                             []  -> Nothing

mustPrecede :: BindNeed -> BindNeed -> Bool
bnd1 `mustPrecede` bnd2 =
  not $ HS.null $ (provides bnd1 `HS.intersection` requires bnd2) `HS.union`
                 (consumedInExp e2 `HS.intersection` requires bnd1)
  where e2 = asTail bnd2

anyIsFreeIn :: HS.HashSet VName -> Exp -> Bool
anyIsFreeIn ks = (ks `intersects`) . HS.map identName . freeInExp

intersects :: (Eq a, Hashable a) => HS.HashSet a -> HS.HashSet a -> Bool
intersects a b = not $ HS.null $ a `HS.intersection` b

data BodyInfo = BodyInfo { bodyConsumes :: HS.HashSet VName
                         }

bodyInfo :: Exp -> BodyInfo
bodyInfo e = BodyInfo {
               bodyConsumes = consumedInExp e
             }

type BlockPred = BodyInfo -> BindNeed -> Bool

orIf :: BlockPred -> BlockPred -> BlockPred
orIf p1 p2 body need = p1 body need || p2 body need

splitHoistable :: BlockPred -> Exp -> Need -> ([BindNeed], Need)
splitHoistable block body needs =
  let (blocked, hoistable, _) =
        foldl split ([], [], HS.empty) $ inDepOrder $ needBindings needs
  in (reverse blocked, Need hoistable)
  where block' = block $ bodyInfo body
        split (blocked, hoistable, ks) need =
          case need of
            LetBind pat e es ->
              let bad e' = block' (LetBind pat e' []) || ks `anyIsFreeIn` e'
              in case (bad e, filter (not . bad) es) of
                   (True, [])     ->
                     (need : blocked, hoistable,
                      patNameSet pat `HS.union` ks)
                   (True, e':es') ->
                     (blocked, LetBind pat e' es' : hoistable, ks)
                   (False, es')   ->
                     (blocked, LetBind pat e es' : hoistable, ks)
            _ | requires need `intersects` ks || block' need ->
                (need : blocked, hoistable, provides need `HS.union` ks)
              | otherwise ->
                (blocked, need : hoistable, ks)

blockIfSeq :: [BlockPred] -> HoistM Exp -> HoistM Exp
blockIfSeq ps m = foldl (flip blockIf) m ps

blockIf :: BlockPred -> HoistM Exp -> HoistM Exp
blockIf block m = pass $ do
  (body, needs) <- listen m
  let (blocked, hoistable) = splitHoistable block body needs
  ds <- asks envDupeState
  return (addBindings ds blocked body, const hoistable)

blockAllHoisting :: HoistM Exp -> HoistM Exp
blockAllHoisting = blockIf $ \_ _ -> True

hasFree :: HS.HashSet VName -> BlockPred
hasFree ks _ need = ks `intersects` requires need

isNotSafe :: BlockPred
isNotSafe _ = not . safeExp . asTail

isNotCheap :: BlockPred
isNotCheap _ = not . cheap . asTail
  where cheap (Var _)      = True
        cheap (Literal {}) = True
        cheap (BinOp _ e1 e2 _ _) = cheap e1 && cheap e2
        cheap (TupLit es _) = all cheap es
        cheap (Not e _) = cheap e
        cheap (Negate e _ _) = cheap e
        cheap (LetPat _ e body _) = cheap e && cheap body
        cheap _ = False

uniqPat :: TupIdent -> Bool
uniqPat (Wildcard t _) = unique t
uniqPat (Id k)         = unique $ identType k
uniqPat (TupId pats _) = any uniqPat pats

isUniqueBinding :: BlockPred
isUniqueBinding _ (LoopBind pat _ _ _ _)     = uniqPat pat
isUniqueBinding _ (LetBind pat _ _)          = uniqPat pat
isUniqueBinding _ (LetWithBind _ dest _ _ _) = unique $ identType dest

isConsumed :: BlockPred
isConsumed body need =
  provides need `intersects` bodyConsumes body

hoistCommon :: HoistM Exp -> HoistM Exp -> HoistM (Exp, Exp)
hoistCommon m1 m2 = pass $ do
  (body1, needs1) <- listen m1
  (body2, needs2) <- listen m2
  let splitOK = splitHoistable $ isNotSafe `orIf` isNotCheap
      (needs1', safe1) = splitOK body1 needs1
      (needs2', safe2) = splitOK body2 needs2
  return ((addBindings newDupeState needs1' body1,
           addBindings newDupeState needs2' body2),
          const $ safe1 <> safe2)

hoistInExp :: Exp -> HoistM Exp
hoistInExp (If c e1 e2 t loc) = do
  c' <- hoistInExp c
  (e1',e2') <- hoistCommon (hoistInExp e1) (hoistInExp e2)
  return $ If c' e1' e2' t loc
hoistInExp (LetPat pat e body _) = do
  e' <- hoistInExp e
  bindLet pat e' $ hoistInExp body
hoistInExp (LetWith cs dest src idxs ve body _) = do
  idxs' <- mapM hoistInExp idxs
  ve' <- hoistInExp ve
  bindLetWith cs dest src idxs' ve' $ hoistInExp body
hoistInExp (DoLoop mergepat mergeexp loopvar boundexp loopbody letbody _) = do
  mergeexp' <- hoistInExp mergeexp
  boundexp' <- hoistInExp boundexp
  loopbody' <- blockIfSeq [hasFree boundnames, isConsumed] $
               hoistInExp loopbody
  bindLoop mergepat mergeexp' loopvar boundexp' loopbody' $ hoistInExp letbody
  where boundnames = identName loopvar `HS.insert` patNameSet mergepat
hoistInExp e@(Map2 cs (TupleLambda params _ _ _) arrexps _ _) =
  hoistInSOAC e arrexps $ \ks ->
    withSOACArrSlices cs params ks $
    withShapes (sameOuterShapes cs ks) $
    hoistInExpBase e
hoistInExp e@(Reduce2 cs (TupleLambda params _ _ _) accexps arrexps _ _) =
  hoistInSOAC e arrexps $ \ks ->
    withSOACArrSlices cs (drop (length accexps) params) ks $
    withShapes (sameOuterShapes cs ks) $
    hoistInExpBase e
hoistInExp e@(Scan2 cs (TupleLambda params _ _ _) accexps arrexps _ _) =
  hoistInSOAC e arrexps $ \arrks ->
  hoistInSOAC e accexps $ \accks ->
    let (accparams, arrparams) = splitAt (length accexps) params in
    withSOACArrSlices cs arrparams arrks $
    withShapes (map (first Id) $ filter (isArrayIdent . fst) $
                zip (map fromParam accparams) $ map (slice cs 0) accks) $
    withShapes (sameOuterShapes cs arrks) $
    hoistInExpBase e
hoistInExp e@(Redomap2 cs _ (TupleLambda innerparams _ _ _)
              accexps arrexps _ _) =
  hoistInSOAC e arrexps $ \ks ->
    withSOACArrSlices cs (drop (length accexps) innerparams) ks $
    withShapes (sameOuterShapes cs ks) $
    hoistInExpBase e
hoistInExp e = hoistInExpBase e

hoistInExpBase :: Exp -> HoistM Exp
hoistInExpBase = mapExpM hoist
  where hoist = identityMapper {
                  mapOnExp = hoistInExp
                , mapOnTupleLambda = hoistInTupleLambda
                }

hoistInTupleLambda :: TupleLambda -> HoistM TupleLambda
hoistInTupleLambda (TupleLambda params body rettype loc) = do
  body' <- blockIf (hasFree params' `orIf` isUniqueBinding) $ hoistInExp body
  return $ TupleLambda params body' rettype loc
  where params' = HS.fromList $ map identName params

arrVars :: [Exp] -> Maybe [Ident]
arrVars = mapM arrVars'
  where arrVars' (Var k) = Just k
        arrVars' _       = Nothing

hoistInSOAC :: Exp -> [Exp] -> ([Ident] -> HoistM Exp) -> HoistM Exp
hoistInSOAC e arrexps m =
  case arrVars arrexps of
    Nothing -> hoistInExpBase e
    Just ks -> m ks

arrSlices :: Certificates -> [Parameter] -> [Ident] -> [(TupIdent, [Exp])]
arrSlices cs params = zip (map (Id . fromParam) params) . map (slice cs 1)

withSOACArrSlices :: Certificates -> [Parameter] -> [Ident]
                  -> HoistM Exp -> HoistM Exp
withSOACArrSlices cs params ks m = do
  agg <- asks envAggressive
  if agg
  then withShapes (arrSlices cs params ks) m
  else m
