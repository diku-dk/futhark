{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
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

import L0C.InternalRep
import L0C.InternalRep.MonadFreshNames
import qualified L0C.HORepresentation.SOACNest as Nest

import L0C.Rebinder.CSE
import qualified L0C.Rebinder.SizeTracking as SZ

data BindNeed = LoopBind [(Ident,SubExp)] Ident SubExp Exp
              | LetBind [Ident] Exp [Exp]
              | LetWithBind Certificates Ident Ident (Maybe Certificates) [SubExp] SubExp
                deriving (Show, Eq)

type NeedSet = [BindNeed]

asTail :: BindNeed -> Exp
asTail (LoopBind merge i bound loopbody) =
  DoLoop merge i bound loopbody (TupLit [] loc) loc
    where loc = srclocOf loopbody
asTail (LetBind pat e _) =
  LetPat pat e (TupLit [] loc) loc
    where loc = srclocOf pat
asTail (LetWithBind cs dest src idxcs idxs ve) =
  LetWith cs dest src idxcs idxs ve (SubExp $ Var dest) $ srclocOf dest

requires :: BindNeed -> HS.HashSet VName
requires (LetBind pat e (alte:alts)) =
  requires (LetBind pat e []) <> requires (LetBind pat alte alts)
requires bnd = HS.map identName $ freeInExp $ asTail bnd

provides :: BindNeed -> HS.HashSet VName
provides (LoopBind merge _ _ _)       = patNameSet $ map fst merge
provides (LetBind pat _ _)            = patNameSet pat
provides (LetWithBind _ dest _ _ _ _) = HS.singleton $ identName dest

patNameSet :: [Ident] -> HS.HashSet VName
patNameSet = HS.fromList . map identName

data Need = Need { needBindings :: NeedSet
                 , freeInBound  :: HS.HashSet VName
                 }

instance Monoid Need where
  Need b1 f1 `mappend` Need b2 f2 = Need (b1 <> b2) (f1 <> f2)
  mempty = Need [] HS.empty

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

instance MonadFreshNames VName HoistM where
  getNameSource = get
  putNameSource = put

runHoistM :: HoistM a -> NameSource VName -> Env -> a
runHoistM (HoistM m) src env = let (x, _, _) = runRWS m env src
                               in x

needThis :: BindNeed -> HoistM ()
needThis need = tell $ Need [need] HS.empty

boundFree :: HS.HashSet VName -> HoistM ()
boundFree fs = tell $ Need [] fs

withNewBinding :: String -> Exp -> (Ident -> HoistM a) -> HoistM a
withNewBinding k e m = do
  k' <- newVName k
  case typeOf e of
    [t] ->
      let ident = Ident { identName = k'
                        , identType = t
                        , identSrcLoc = srclocOf e }
      in withBinding [ident] e $ m ident
    _   -> fail "Rebinder.withNewBinding: Cannot insert tuple-typed binding."

withBinding :: [Ident] -> Exp -> HoistM a -> HoistM a
withBinding pat e@(Size _ i (Var x) _) m = do
  let mkAlt es = case drop i es of
                   des:_ -> S.toList des
                   _     -> []
  alts <- mkAlt <$> asks (SZ.lookup x . envBindings)
  withSeveralBindings pat e alts m

withBinding pat e m = withSingleBinding pat e m

withSingleBinding :: [Ident] -> Exp -> HoistM a -> HoistM a
withSingleBinding pat e = withSeveralBindings pat e []

withSeveralBindings :: [Ident] -> Exp -> [Exp]
                    -> HoistM a -> HoistM a
withSeveralBindings pat e alts m = do
  ds <- asks envDupeState
  let (e', ds') = performCSE ds pat e
      (es, ds'') = performMultipleCSE ds pat alts
  needThis $ LetBind pat e' es
  local (\env -> env { envDupeState = ds' <> ds''}) m

bindLet :: [Ident] -> Exp -> HoistM a -> HoistM a
bindLet [dest] (SubExp (Var src)) m =
  withBinding [dest] (SubExp $ Var src) $
  case identType src of Array {} -> withShape dest (slice [] 0 src) m
                        _        -> m

bindLet pat@[dest] e@(Iota ne _) m =
  withBinding pat e $
  withShape dest [SubExp ne] m

bindLet pat@[dest] e@(Replicate ne (Var y) _) m =
  withBinding pat e $
  withShape dest (SubExp ne:slice [] 0 y) m

bindLet pat@[dest1, dest2] e@(Split cs ne srce loc) m =
  withBinding pat e $
  withNewBinding "split_src_sz" (Size cs 0 srce loc) $ \src_sz ->
  withNewBinding "split_sz" (BinOp Minus (Var src_sz) ne (Basic Int) loc) $ \split_sz ->
  withShapes [(dest1, SubExp ne : rest),
              (dest2, SubExp (Var split_sz) : rest)] m
    where rest = [ Size cs i srce loc
                   | i <- [1.. arrayRank (subExpType srce) - 1]]

bindLet pat@[dest] e@(Concat cs (Var x) (Var y) loc) m =
  withBinding pat e $
  withNewBinding "concat_x" (Size cs 0 (Var x) loc) $ \concat_x ->
  withNewBinding "concat_y" (Size cs 0 (Var y) loc) $ \concat_y ->
  withNewBinding "concat_sz" (BinOp Plus (Var concat_x) (Var concat_y) (Basic Int) loc) $ \concat_sz ->
  withShape dest (SubExp (Var concat_sz) :
                  [Size cs i (Var x) loc
                     | i <- [1..arrayRank (identType x) - 1]])
  m

bindLet pat e m
  | Right nest <- Nest.fromExp (const Nothing) e =
    withBinding pat e $
    withShapes (SZ.sizeRelations pat nest) m

bindLet pat@[dest] e@(Index cs src _ idxs _) m =
  withBinding pat e $
  withShape dest (slice cs (length idxs) src) m

bindLet pat@[dest] e@(Transpose cs k n (Var src) loc) m =
  withBinding pat e $
  withShape dest dims m
    where dims = transposeIndex k n
                 [Size cs i (Var src) loc
                  | i <- [0..arrayRank (identType src) - 1]]

bindLet pat e m = withBinding pat e m

bindLetWith :: Certificates -> Ident -> Ident
            -> Maybe Certificates -> [SubExp] -> SubExp
            -> HoistM a -> HoistM a
bindLetWith cs dest src idxcs idxs ve m = do
  needThis $ LetWithBind cs dest src idxcs idxs ve
  withShape dest (slice [] 0 src) m

bindLoop :: [(Ident,SubExp)] -> Ident -> SubExp -> Exp -> HoistM a -> HoistM a
bindLoop merge i bound body m = do
  needThis $ LoopBind merge i bound body
  m

slice :: Certificates -> Int -> Ident -> [Exp]
slice cs d k = [ Size cs i (Var k) $ srclocOf k
                 | i <- [d..arrayRank (identType k)-1]]

withShape :: Ident -> [Exp] -> HoistM a -> HoistM a
withShape dest src =
  local (\env -> env { envBindings = SZ.insert dest src $ envBindings env })

withShapes :: [(Ident, [Exp])] -> HoistM a -> HoistM a
withShapes [] m = m
withShapes ((dest, es):rest) m =
  withShape dest es $ withShapes rest m

sameOuterShapes :: Certificates -> [Ident] -> [(Ident, [Exp])]
sameOuterShapes cs = outer' []
  where outer' _ [] = []
        outer' prev (k:ks) =
          [ (k, [Size cs 0 (Var k') $ srclocOf k])
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

addBindings :: DupeState -> [BindNeed] -> Exp -> HS.HashSet VName
            -> (Exp, HS.HashSet VName)
addBindings dupes needs =
  curry $ foldl (.) id $ snd $
          mapAccumL comb (HM.empty, dupes) needs
  where bind bnd binder (inner, fs)
          | provides bnd `intersects` fs =
            (binder inner,
             (fs `HS.difference` provides bnd)
             `HS.union` requires bnd)
          | otherwise =
            (inner, fs)

        comb (m,ds) bnd@(LoopBind merge loopvar boundexp loopbody) =
          ((m `HM.union` distances m bnd, ds),
           bind bnd $
           \inner ->
             DoLoop merge loopvar boundexp
             loopbody inner $ srclocOf inner)

        comb (m,ds) (LetBind pat e alts) =
          let add e' =
                let (e'',ds') = performCSE ds pat e'
                    bnd       = LetBind pat e'' []
                in ((m `HM.union` distances m bnd, ds'),
                    bind bnd $
                    \inner ->
                      LetPat pat e'' inner $ srclocOf inner)
          in case map snd $ sortBy (comparing fst) $ map (score m) $ e:alts of
               e':_ -> add e'
               _    -> add e

        comb (m,ds) bnd@(LetWithBind cs dest src idxcs idxs ve) =
          ((m `HM.union` distances m bnd, ds),
           bind bnd $
           \inner ->
             LetWith cs dest src idxcs idxs ve inner $ srclocOf inner)

score :: HM.HashMap VName Int -> Exp -> (Int, Exp)
score m (SubExp (Var k)) =
  (fromMaybe (-1) $ HM.lookup (identName k) m, SubExp $ Var k)
score m e =
  (HS.foldl' f 0 $ freeNamesInExp e, e)
  where f x k = case HM.lookup k m of
                  Just y  -> max x y
                  Nothing -> x

expCost :: Exp -> Int
expCost (Map {}) = 1
expCost (Filter {}) = 1
expCost (Reduce {}) = 1
expCost (Scan {}) = 1
expCost (Redomap {}) = 1
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
            LetWithBind _ dest src _ idxs ve ->
              (HS.singleton $ identName dest,
               identName src `HS.insert`
               mconcat (map (freeNamesInExp . SubExp) $ ve:idxs),
               1)
            LoopBind merge _ bound loopbody ->
              (patNameSet $ map fst merge,
               mconcat $ map freeNamesInExp $
               [SubExp bound, loopbody] ++ map (SubExp . snd) merge,
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

splitHoistable :: BlockPred -> Exp -> NeedSet -> ([BindNeed], NeedSet)
splitHoistable block body needs =
  let (blocked, hoistable, _) =
        foldl split ([], [], HS.empty) $ inDepOrder needs
  in (reverse blocked, hoistable)
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
  ds <- asks envDupeState
  let (blocked, hoistable) = splitHoistable block body $ needBindings needs
      (e, fs) = addBindings ds blocked body $ freeInBound needs
  return (e,
          const Need { needBindings = hoistable
                     , freeInBound  = fs
                     })

blockAllHoisting :: HoistM Exp -> HoistM Exp
blockAllHoisting = blockIf $ \_ _ -> True

hasFree :: HS.HashSet VName -> BlockPred
hasFree ks _ need = ks `intersects` requires need

isNotSafe :: BlockPred
isNotSafe _ = not . safeExp . asTail

isNotCheap :: BlockPred
isNotCheap _ = not . cheap . asTail
  where cheap (SubExp _)   = True
        cheap (BinOp {})   = True
        cheap (TupLit {})  = True
        cheap (Not {})     = True
        cheap (Negate {})  = True
        cheap (LetPat _ e body _) = cheap e && cheap body
        cheap _ = False

uniqPat :: [Ident] -> Bool
uniqPat = any $ unique . identType

isUniqueBinding :: BlockPred
isUniqueBinding _ (LoopBind merge _ _ _)       = uniqPat $ map fst merge
isUniqueBinding _ (LetBind pat _ _)            = uniqPat pat
isUniqueBinding _ (LetWithBind _ dest _ _ _ _) = unique $ identType dest

isConsumed :: BlockPred
isConsumed body need =
  provides need `intersects` bodyConsumes body

hoistCommon :: HoistM Exp -> HoistM Exp -> HoistM (Exp, Exp)
hoistCommon m1 m2 = pass $ do
  (body1, needs1) <- listen m1
  (body2, needs2) <- listen m2
  let splitOK = splitHoistable $ isNotSafe `orIf` isNotCheap
      (needs1', safe1) = splitOK body1 $ needBindings needs1
      (needs2', safe2) = splitOK body2 $ needBindings needs2
      (e1, f1) = addBindings newDupeState needs1' body1 $ freeInBound needs1
      (e2, f2) = addBindings newDupeState needs2' body2 $ freeInBound needs2
  return ((e1, e2),
          const Need { needBindings = safe1 <> safe2
                     , freeInBound = f1 <> f2
                     })

hoistInExp :: Exp -> HoistM Exp
hoistInExp (If c e1 e2 t loc) = do
  (e1',e2') <- hoistCommon (hoistInExp e1) (hoistInExp e2)
  return $ If c e1' e2' t loc
hoistInExp (LetPat pat e body _) = do
  e' <- hoistInExp e
  bindLet pat e' $ hoistInExp body
hoistInExp (LetWith cs dest src idxcs idxs ve body _) =
  bindLetWith cs dest src idxcs idxs ve $ hoistInExp body
hoistInExp (DoLoop merge loopvar boundexp loopbody letbody _) = do
  loopbody' <- blockIfSeq [hasFree boundnames, isConsumed] $
               hoistInExp loopbody
  bindLoop merge loopvar boundexp loopbody' $ hoistInExp letbody
  where boundnames = identName loopvar `HS.insert` patNameSet (map fst merge)
hoistInExp e@(Map cs (Lambda params _ _ _) arrexps _) =
  hoistInSOAC e arrexps $ \ks ->
    withSOACArrSlices cs params ks $
    withShapes (sameOuterShapes cs ks) $
    hoistInExpBase e
hoistInExp e@(Reduce cs (Lambda params _ _ _) args _) =
  hoistInSOAC e arrexps $ \ks ->
    withSOACArrSlices cs (drop (length accexps) params) ks $
    withShapes (sameOuterShapes cs ks) $
    hoistInExpBase e
  where (accexps, arrexps) = unzip args
hoistInExp e@(Scan cs (Lambda params _ _ _) args _) =
  hoistInSOAC e arrexps $ \arrks ->
  hoistInSOAC e accexps $ \accks ->
    let (accparams, arrparams) = splitAt (length accexps) params in
    withSOACArrSlices cs arrparams arrks $
    withShapes (filter (isArrayIdent . fst) $
                zip (map fromParam accparams) $ map (slice cs 0) accks) $
    withShapes (sameOuterShapes cs arrks) $
    hoistInExpBase e
  where (accexps, arrexps) = unzip args
hoistInExp e@(Redomap cs _ (Lambda innerparams _ _ _)
              accexps arrexps _) =
  hoistInSOAC e arrexps $ \ks ->
    withSOACArrSlices cs (drop (length accexps) innerparams) ks $
    withShapes (sameOuterShapes cs ks) $
    hoistInExpBase e
hoistInExp e = do e' <- hoistInExpBase e
                  boundFree $ freeNamesInExp e'
                  return e'

hoistInExpBase :: Exp -> HoistM Exp
hoistInExpBase = mapExpM hoist
  where hoist = identityMapper {
                  mapOnExp = hoistInExp
                , mapOnLambda = hoistInLambda
                }

hoistInLambda :: Lambda -> HoistM Lambda
hoistInLambda (Lambda params body rettype loc) = do
  body' <- blockIf (hasFree params' `orIf` isUniqueBinding) $ hoistInExp body
  return $ Lambda params body' rettype loc
  where params' = patNameSet $ map fromParam params

arrVars :: [SubExp] -> Maybe [Ident]
arrVars = mapM arrVars'
  where arrVars' (Var k) = Just k
        arrVars' _       = Nothing

hoistInSOAC :: Exp -> [SubExp] -> ([Ident] -> HoistM Exp) -> HoistM Exp
hoistInSOAC e arrexps m =
  case arrVars arrexps of
    Nothing -> hoistInExpBase e
    Just ks -> m ks

arrSlices :: Certificates -> [Param] -> [Ident] -> [(Ident, [Exp])]
arrSlices cs params = zip (map fromParam params) . map (slice cs 1)

withSOACArrSlices :: Certificates -> [Param] -> [Ident]
                  -> HoistM Exp -> HoistM Exp
withSOACArrSlices cs params ks m = do
  agg <- asks envAggressive
  if agg
  then withShapes (arrSlices cs params ks) m
  else m
