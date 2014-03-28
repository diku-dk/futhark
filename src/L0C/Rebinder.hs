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

import L0C.InternalRep
import L0C.MonadFreshNames

import L0C.Rebinder.CSE

data BindNeed = LoopNeed [(Ident,SubExp)] Ident SubExp Body
              | LetNeed [Ident] Exp [Exp]
              | LetWithNeed Certificates Ident Ident [SubExp] SubExp
                deriving (Show, Eq)

type NeedSet = [BindNeed]

asTail :: BindNeed -> Body
asTail (LoopNeed merge i bound loopbody) =
  Body [DoLoop merge i bound loopbody] $ Result [] [] loc
    where loc = srclocOf loopbody
asTail (LetNeed pat e _) =
  Body [Let pat e] $ Result [] [] loc
    where loc = srclocOf pat
asTail (LetWithNeed cs dest src idxs ve) =
  Body [LetWith cs dest src idxs ve] $ Result [] [Var dest] loc
    where loc = srclocOf dest

requires :: BindNeed -> HS.HashSet VName
requires (LetNeed pat e alts) =
  freeInE `mappend` freeInPat
  where freeInE   = mconcat $ map freeNamesInExp $ e : alts
        freeInPat = mconcat $ map (freeInType . identType) pat
requires bnd = HS.map identName $ freeInBody $ asTail bnd

provides :: BindNeed -> HS.HashSet VName
provides (LoopNeed merge _ _ _)     = patNameSet $ map fst merge
provides (LetNeed pat _ _)          = patNameSet pat
provides (LetWithNeed _ dest _ _ _) = HS.singleton $ identName dest

patNameSet :: [Ident] -> HS.HashSet VName
patNameSet = HS.fromList . map identName

freeInType :: Type -> HS.HashSet VName
freeInType = mconcat . map (freeNamesInExp . subExp) . arrayDims

data Need = Need { needBindings :: NeedSet
                 , freeInBound  :: HS.HashSet VName
                 }

instance Monoid Need where
  Need b1 f1 `mappend` Need b2 f2 = Need (b1 <> b2) (f1 <> f2)
  mempty = Need [] HS.empty

data Env = Env { envDupeState :: DupeState
               }

emptyEnv :: Env
emptyEnv = Env {
             envDupeState = newDupeState
           }

newtype HoistM a = HoistM (RWS
                           Env                -- Reader
                           Need               -- Writer
                           (NameSource VName) -- State
                           a)
  deriving (Applicative, Functor, Monad,
            MonadWriter Need, MonadReader Env, MonadState (NameSource VName))

instance MonadFreshNames HoistM where
  getNameSource = get
  putNameSource = put

runHoistM :: HoistM a -> NameSource VName -> Env -> a
runHoistM (HoistM m) src env = let (x, _, _) = runRWS m env src
                               in x

needThis :: BindNeed -> HoistM ()
needThis need = tell $ Need [need] HS.empty

boundFree :: HS.HashSet VName -> HoistM ()
boundFree fs = tell $ Need [] fs

withBinding :: [Ident] -> Exp -> HoistM a -> HoistM a
withBinding = withSingleBinding

withSingleBinding :: [Ident] -> Exp -> HoistM a -> HoistM a
withSingleBinding pat e = withSeveralBindings pat e []

withSeveralBindings :: [Ident] -> Exp -> [Exp]
                    -> HoistM a -> HoistM a
withSeveralBindings pat e alts m = do
  ds <- asks envDupeState
  let (e', ds') = performCSE ds pat e
      (es, ds'') = performMultipleCSE ds pat alts
  needThis $ LetNeed pat e' es
  local (\env -> env { envDupeState = ds' <> ds''}) m

bindLet :: [Ident] -> Exp -> HoistM a -> HoistM a

bindLet = withBinding

bindLetWith :: Certificates -> Ident -> Ident
            -> [SubExp] -> SubExp
            -> HoistM a -> HoistM a
bindLetWith cs dest src idxs ve m = do
  needThis $ LetWithNeed cs dest src idxs ve
  m

bindLoop :: [(Ident,SubExp)] -> Ident -> SubExp -> Body -> HoistM a -> HoistM a
bindLoop merge i bound body m = do
  needThis $ LoopNeed merge i bound body
  m

-- | Run the let-hoisting algorithm on the given program.  Even if the
-- output differs from the output, meaningful hoisting may not have
-- taken place - the order of bindings may simply have been
-- rearranged.  The function is idempotent, however.
transformProg :: Prog -> Prog
transformProg prog =
  Prog $ runHoistM (mapM transformFun $ progFunctions prog) namesrc env
  where namesrc = newNameSourceForProg prog
        env = emptyEnv

transformFun :: FunDec -> HoistM FunDec
transformFun (fname, rettype, params, body, loc) = do
  body' <- blockAllHoisting $ hoistInBody body
  return (fname, rettype, params, body', loc)

addBindings :: DupeState -> [BindNeed] -> Body -> HS.HashSet VName
            -> (Body, HS.HashSet VName)
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

        comb (m,ds) bnd@(LoopNeed merge loopvar boundexp loopbody) =
          ((m `HM.union` distances m bnd, ds),
           bind bnd $
           \(Body bnds res) ->
             Body (DoLoop merge loopvar boundexp loopbody:bnds) res)

        comb (m,ds) (LetNeed pat e alts) =
          let add e' =
                let (e'',ds') = performCSE ds pat e'
                    bnd       = LetNeed pat e'' []
                in ((m `HM.union` distances m bnd, ds'),
                    bind bnd $
                    \(Body bnds res) -> Body (Let pat e'':bnds) res)
          in case map snd $ sortBy (comparing fst) $ map (score m) $ e:alts of
               e':_ -> add e'
               _    -> add e

        comb (m,ds) bnd@(LetWithNeed cs dest src idxs ve) =
          ((m `HM.union` distances m bnd, ds),
           bind bnd $
           \(Body bnds res) -> Body (LetWith cs dest src idxs ve:bnds) res)

score :: HM.HashMap VName Int -> Exp -> (Int, Exp)
score m (SubExps [Var k] _) =
  (fromMaybe (-1) $ HM.lookup (identName k) m, subExp $ Var k)
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
expCost (Rearrange {}) = 1
expCost (Copy {}) = 1
expCost (Concat {}) = 1
expCost (Split {}) = 1
expCost (Reshape {}) = 1
expCost (Replicate {}) = 1
expCost _ = 0

distances :: HM.HashMap VName Int -> BindNeed -> HM.HashMap VName Int
distances m need = HM.fromList [ (k, d+cost) | k <- HS.toList outs ]
  where d = HS.foldl' f 0 ins
        (outs, ins, cost) =
          case need of
            LetNeed pat e _ ->
              (patNameSet pat, freeNamesInExp e, expCost e)
            LetWithNeed _ dest src idxs ve ->
              (HS.singleton $ identName dest,
               identName src `HS.insert`
               mconcat (map (freeNamesInExp . subExp) $ ve:idxs),
               1)
            LoopNeed merge _ bound loopbody ->
              (patNameSet $ map fst merge,
               mconcat $ freeNamesInBody loopbody : map freeNamesInExp
               (subExp bound : map (subExp . snd) merge),
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
                  (consumedInBody e2 `HS.intersection` requires bnd1)
  where e2 = asTail bnd2

anyIsFreeIn :: HS.HashSet VName -> Exp -> Bool
anyIsFreeIn ks = (ks `intersects`) . HS.map identName . freeInExp

intersects :: (Eq a, Hashable a) => HS.HashSet a -> HS.HashSet a -> Bool
intersects a b = not $ HS.null $ a `HS.intersection` b

data BodyInfo = BodyInfo { bodyConsumes :: HS.HashSet VName
                         }

bodyInfo :: Body -> BodyInfo
bodyInfo b = BodyInfo {
               bodyConsumes = consumedInBody b
             }

type BlockPred = BodyInfo -> BindNeed -> Bool

orIf :: BlockPred -> BlockPred -> BlockPred
orIf p1 p2 body need = p1 body need || p2 body need

splitHoistable :: BlockPred -> Body -> NeedSet -> ([BindNeed], NeedSet)
splitHoistable block body needs =
  let (blocked, hoistable, _) =
        foldl split ([], [], HS.empty) $ inDepOrder needs
  in (reverse blocked, hoistable)
  where block' = block $ bodyInfo body
        split (blocked, hoistable, ks) need =
          case need of
            LetNeed pat e es ->
              let bad e' = block' (LetNeed pat e' []) || ks `anyIsFreeIn` e'
              in case (bad e, filter (not . bad) es) of
                   (True, [])     ->
                     (need : blocked, hoistable,
                      patNameSet pat `HS.union` ks)
                   (True, e':es') ->
                     (blocked, LetNeed pat e' es' : hoistable, ks)
                   (False, es')   ->
                     (blocked, LetNeed pat e es' : hoistable, ks)
            _ | requires need `intersects` ks || block' need ->
                (need : blocked, hoistable, provides need `HS.union` ks)
              | otherwise ->
                (blocked, need : hoistable, ks)

blockIfSeq :: [BlockPred] -> HoistM Body -> HoistM Body
blockIfSeq ps m = foldl (flip blockIf) m ps

blockIf :: BlockPred -> HoistM Body -> HoistM Body
blockIf block m = pass $ do
  (body, needs) <- listen m
  ds <- asks envDupeState
  let (blocked, hoistable) = splitHoistable block body $ needBindings needs
      (e, fs) = addBindings ds blocked body $ freeInBound needs
  return (e,
          const Need { needBindings = hoistable
                     , freeInBound  = fs
                     })

blockAllHoisting :: HoistM Body -> HoistM Body
blockAllHoisting = blockIf $ \_ _ -> True

hasFree :: HS.HashSet VName -> BlockPred
hasFree ks _ need = ks `intersects` requires need

isNotSafe :: BlockPred
isNotSafe _ = not . safeBnd
  where safeBnd (LetNeed _ e _) = safeExp e
        safeBnd _               = False

isNotCheap :: BlockPred
isNotCheap _ = not . cheapBnd
  where cheap (BinOp {})   = True
        cheap (SubExps {}) = True
        cheap (Not {})     = True
        cheap (Negate {})  = True
        cheap _            = False
        cheapBnd (LetNeed _ e _) = cheap e
        cheapBnd _               = False

uniqPat :: [Ident] -> Bool
uniqPat = any $ unique . identType

isUniqueBinding :: BlockPred
isUniqueBinding _ (LoopNeed merge _ _ _)     = uniqPat $ map fst merge
isUniqueBinding _ (LetNeed pat _ _)          = uniqPat pat
isUniqueBinding _ (LetWithNeed _ dest _ _ _) = unique $ identType dest

isConsumed :: BlockPred
isConsumed body need =
  provides need `intersects` bodyConsumes body

hoistCommon :: HoistM Body -> HoistM Body -> HoistM (Body, Body)
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

hoistInBody :: Body -> HoistM Body

hoistInBody (Body [] (Result cs es loc)) =
  resultBody <$> mapM hoistInIdent cs <*>
                 mapM hoistInSubExp es <*> pure loc

hoistInBody (Body (Let pat e:bnds) res) = do
  pat' <- mapM hoistInIdent pat
  e' <- hoistInExp e
  bindLet pat' e' $ hoistInBody $ Body bnds res
hoistInBody (Body (LetWith cs dest src idxs ve:bnds) res) = do
  cs'   <- mapM hoistInIdent cs
  dest' <- hoistInIdent dest
  src'  <- hoistInIdent src
  idxs' <- mapM hoistInSubExp idxs
  ve'   <- hoistInSubExp ve
  bindLetWith cs' dest' src' idxs' ve' $ hoistInBody $ Body bnds res
hoistInBody (Body (DoLoop merge loopvar boundexp loopbody:bnds) res) = do
  let (mergepat, mergeexp) = unzip merge
  loopbody' <- blockIfSeq [hasFree boundnames, isConsumed] $
               hoistInBody loopbody
  mergepat' <- mapM hoistInIdent mergepat
  mergeexp' <- mapM hoistInSubExp mergeexp
  bindLoop (zip mergepat' mergeexp') loopvar boundexp loopbody' $
           hoistInBody $ Body bnds res
  where boundnames = identName loopvar `HS.insert` patNameSet (map fst merge)

hoistInExp :: Exp -> HoistM Exp
hoistInExp (If c e1 e2 t loc) = do
  (e1',e2') <- hoistCommon (hoistInBody e1) (hoistInBody e2)
  c' <- hoistInSubExp c
  t' <- mapM hoistInType t
  return $ If c' e1' e2' t' loc
hoistInExp e = hoistInExpBase e

hoistInExpBase :: Exp -> HoistM Exp
hoistInExpBase = mapExpM hoist
  where hoist = Mapper {
                  mapOnExp = hoistInExp
                , mapOnBody = hoistInBody
                , mapOnSubExp = hoistInSubExp
                , mapOnLambda = hoistInLambda
                , mapOnIdent = hoistInIdent
                , mapOnType = hoistInType
                , mapOnValue = return
                , mapOnCertificates = mapM hoistInIdent
                }

hoistInSubExp :: SubExp -> HoistM SubExp
hoistInSubExp (Var v)          = Var <$> hoistInIdent v
hoistInSubExp (Constant v loc) = return $ Constant v loc

hoistInIdent :: Ident -> HoistM Ident
hoistInIdent v = do boundFree $ HS.singleton $ identName v
                    t' <- hoistInType $ identType v
                    return v { identType = t' }

hoistInType :: TypeBase als Shape -> HoistM (TypeBase als Shape)
hoistInType t = do dims <- mapM hoistInSubExp $ arrayDims t
                   return $ t `setArrayShape` Shape dims

hoistInLambda :: Lambda -> HoistM Lambda
hoistInLambda (Lambda params body rettype loc) = do
  body' <- blockIf (hasFree params' `orIf` isUniqueBinding) $ hoistInBody body
  rettype' <- mapM hoistInType rettype
  return $ Lambda params body' rettype' loc
  where params' = patNameSet $ map fromParam params
