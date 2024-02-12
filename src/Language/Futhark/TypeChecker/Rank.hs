module Language.Futhark.TypeChecker.Rank (rankAnalysis) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Vector.Unboxed qualified as V
import Debug.Trace
import Futhark.FreshNames qualified as FreshNames
import Futhark.MonadFreshNames hiding (newName)
import Futhark.Solve.BranchAndBound
import Futhark.Solve.LP hiding (Constraint, LSum, LinearProg)
import Futhark.Solve.LP qualified as LP
import Futhark.Solve.Simplex
import Language.Futhark hiding (ScalarType)
import Language.Futhark.TypeChecker.Constraints
import Language.Futhark.TypeChecker.Monad (mkTypeVarName)

type LSum = LP.LSum VName Double

type Constraint = LP.Constraint VName Double

type LinearProg = LP.LinearProg VName Double

type ScalarType = ScalarTypeBase SComp NoUniqueness

class Rank a where
  rank :: a -> LSum

instance Rank VName where
  rank = var

instance Rank SComp where
  rank SDim = constant 1
  rank (SVar v) = var v

instance Rank (Shape SComp) where
  rank = foldr (\d r -> rank d ~+~ r) (constant 0) . shapeDims

instance Rank ScalarType where
  rank Prim {} = constant 0
  rank (TypeVar _ (QualName [] v) []) = var v -- FIXME - might not be a type variable.
  rank (TypeVar {}) = constant 0
  rank (Arrow {}) = constant 0
  rank (Record {}) = constant 0
  rank (Sum {}) = constant 0

instance Rank Type where
  rank (Scalar t) = rank t
  rank (Array _ shape t) = rank shape ~+~ rank t

class Distribute a where
  distribute :: a -> a

instance Distribute Type where
  distribute = distributeOne
    where
      distributeOne (Array _ s (Arrow _ _ _ ta (RetType rd tr))) =
        Scalar $ Arrow NoUniqueness Unnamed mempty (arrayOf s ta) (RetType rd $ arrayOfWithAliases Nonunique s $ tr)
      distributeOne t = t

instance Distribute Ct where
  distribute (CtEq t1 t2) = distribute t1 `CtEq` distribute t2
  distribute c = c

data RankState = RankState
  { rankBinVars :: Map VName VName,
    rankCounter :: !Int,
    rankConstraints :: [Constraint]
  }

newtype RankM a = RankM {runRankM :: State RankState a}
  deriving (Functor, Applicative, Monad, MonadState RankState)

incCounter :: RankM Int
incCounter = do
  s <- get
  put s {rankCounter = rankCounter s + 1}
  pure $ rankCounter s

binVar :: VName -> RankM (VName)
binVar sv = do
  mbv <- (M.!? sv) <$> gets rankBinVars
  case mbv of
    Nothing -> do
      bv <- VName ("b_" <> baseName sv) <$> incCounter
      modify $ \s ->
        s
          { rankBinVars = M.insert sv bv $ rankBinVars s,
            rankConstraints = rankConstraints s ++ [bin bv]
          }
      pure bv
    Just bv -> pure bv

addConstraints :: [Constraint] -> RankM ()
addConstraints cs =
  modify $ \s -> s {rankConstraints = rankConstraints s ++ cs}

addConstraint :: Constraint -> RankM ()
addConstraint = addConstraints . pure

addCt :: Ct -> RankM ()
addCt (CtEq t1 t2) = addConstraint $ rank t1 ~==~ rank t2
addCt (CtAM r m) = do
  b_r <- binVar r
  b_m <- binVar m
  addConstraints $ oneIsZero (b_r, r) (b_m, m)

addTyVarInfo :: TyVar -> (Int, TyVarInfo) -> RankM ()
addTyVarInfo tv (_, TyVarFree) = pure ()
addTyVarInfo tv (_, TyVarPrim _) =
  addConstraint $ rank tv ~==~ constant 0
addTyVarInfo _ _ = error "Unhandled"

mkLinearProg :: Int -> [Ct] -> TyVars -> LinearProg
mkLinearProg counter cs tyVars =
  LP.LinearProg
    { optType = Minimize,
      objective =
        let shape_vars = M.keys $ rankBinVars finalState
         in foldr (\sv s -> var sv ~+~ s) (constant 0) shape_vars,
      constraints = rankConstraints finalState
    }
  where
    initState =
      RankState
        { rankBinVars = mempty,
          rankCounter = counter,
          rankConstraints = mempty
        }
    buildLP = do
      mapM_ addCt cs
      mapM_ (uncurry addTyVarInfo) $ M.toList tyVars
    finalState = flip execState initState $ runRankM buildLP

rankAnalysis :: VNameSource -> Int -> [Ct] -> TyVars -> Maybe ([Ct], TyVars, VNameSource, Int)
rankAnalysis vns counter cs tyVars = do
  traceM $ unlines ["## rankAnalysis prog", prettyString prog]
  (_size, ranks) <- branchAndBound lp
  let rank_map = (fromJust . (ranks V.!?)) <$> inv_var_map
  traceM $ unlines $ "## rank map" : map prettyString (M.toList rank_map)
  let initEnv =
        SubstEnv
          { envTyVars = tyVars,
            envRanks = rank_map
          }

      initState =
        SubstState
          { substTyVars = mempty,
            substNewVars = mempty,
            substNameSource = vns,
            substCounter = counter
          }
      (cs', state') =
        runSubstM initEnv initState $
          substRanks $
            filter (not . isCtAM) cs
  pure (cs', substTyVars state' <> tyVars, substNameSource state', substCounter state')
  where
    isCtAM (CtAM {}) = True
    isCtAM _ = False
    splitFuncs
      ( CtEq
          (Scalar (Arrow _ _ _ t1a (RetType _ t1r)))
          (Scalar (Arrow _ _ _ t2a (RetType _ t2r)))
        ) =
        splitFuncs (CtEq t1a t2a) ++ splitFuncs (CtEq t1r' t2r')
        where
          t1r' = t1r `setUniqueness` NoUniqueness
          t2r' = t2r `setUniqueness` NoUniqueness
    splitFuncs c = [c]
    cs' = foldMap splitFuncs cs
    prog = mkLinearProg counter cs' tyVars
    (lp, var_map) = linearProgToLP prog
    inv_var_map = M.fromListWith (error "oh no!") [(v, k) | (k, v) <- M.toList var_map]

newtype SubstM a = SubstM (StateT SubstState (Reader SubstEnv) a)
  deriving (Functor, Applicative, Monad, MonadState SubstState, MonadReader SubstEnv)

runSubstM :: SubstEnv -> SubstState -> SubstM a -> (a, SubstState)
runSubstM initEnv initState (SubstM m) =
  runReader (runStateT m initState) initEnv

data SubstEnv = SubstEnv
  { envTyVars :: TyVars,
    envRanks :: Map VName Int
  }

data SubstState = SubstState
  { substTyVars :: TyVars,
    substNewVars :: Map TyVar TyVar,
    substNameSource :: VNameSource,
    substCounter :: !Int
  }

substIncCounter :: SubstM Int
substIncCounter = do
  s <- get
  put s {substCounter = substCounter s + 1}
  pure $ substCounter s

newTyVar :: TyVar -> SubstM TyVar
newTyVar t = do
  i <- substIncCounter
  t' <- newID $ mkTypeVarName (baseName t) i
  modify $ \s -> s {substNewVars = M.insert t t' $ substNewVars s}
  pure t'
  where
    newID x = do
      s <- get
      let (v', src') = FreshNames.newName (substNameSource s) $ VName x 0
      put $ s {substNameSource = src'}
      pure v'

rankToShape :: VName -> SubstM (Shape SComp)
rankToShape x = do
  rs <- asks envRanks
  pure $ Shape $ replicate (rs M.! x) SDim

addRankInfo :: TyVar -> SubstM TyVar
addRankInfo t = do
  rs <- asks envRanks
  if rs M.! t == 0
    then pure t
    else do
      new_vars <- gets substNewVars
      maybe new_var pure $ new_vars M.!? t
  where
    lvl = 0 -- FIXME
    new_var = do
      t' <- newTyVar t
      old_tyvars <- asks envTyVars
      case old_tyvars M.!? t of
        Nothing -> pure t'
        Just info -> do
          modify $ \s -> s {substTyVars = M.insert t' info $ substTyVars s}
          modify $ \s -> s {substTyVars = M.insert t (fst info, TyVarFree) $ substTyVars s}
          pure t'

class SubstRanks a where
  substRanks :: a -> SubstM a

instance (SubstRanks a) => SubstRanks [a] where
  substRanks = mapM substRanks

instance SubstRanks (Shape SComp) where
  substRanks = foldM (\s d -> (s <>) <$> instDim d) mempty
    where
      instDim (SDim) = pure $ Shape $ pure SDim
      instDim (SVar x) = rankToShape x

instance SubstRanks (TypeBase SComp u) where
  substRanks t@(Scalar (TypeVar u (QualName [] x) [])) = do
    x' <- addRankInfo x
    pure $ (Scalar (TypeVar u (QualName [] x') []))
  substRanks (Scalar (Arrow u p d ta (RetType retdims tr))) = do
    ta' <- substRanks ta
    tr' <- substRanks tr
    pure $ Scalar (Arrow u p d ta' (RetType retdims tr'))
  substRanks (Array u shape t) = do
    shape' <- substRanks shape
    t' <- substRanks $ Scalar t
    pure $ arrayOfWithAliases u shape' t'
  substRanks t = pure t

instance SubstRanks Ct where
  substRanks (CtEq t1 t2) = CtEq <$> substRanks t1 <*> substRanks t2
  substRanks _ = error ""

-- data SubstState = SubstState
--  { substTyVars :: Map TyVar TyVarSol,
--    substNameSource :: VNameSource,
--    substCounter :: !Int
--  }
--
-- newtype SubstM a = SubstM {runSubstM :: State SubstState a}
--  deriving (Functor, Applicative, Monad, MonadState SubstState)
--
-- substIncCounter :: SubstM Int
-- substIncCounter = do
--  s <- get
--  put s {substCounter = substCounter s + 1}
--  pure $ substCounter s
--
-- newTyVar :: Name -> SubstM TyVar
-- newTyVar desc = do
--  i <- substIncCounter
--  newID $ mkTypeVarName desc i
--  where
--    newID x = do
--      s <- get
--      let (v', src') = FreshNames.newName (substNameSource s) $ VName x 0
--      put $ s {substNameSource = src'}
--      pure v'
--
-- addTyVarSol :: TyVar -> Shape SComp -> SubstM TyVar
-- addTyVarSol t shape = do
--  m <- subsTyVars gets
--  case m M.!? t of
--    Nothing -> do
--      t' <- newTyVar $ baseName t
--      modify $ \s -> s {substTyVars = M.insert t () $ substTyVars s}
--    Just t' -> pure t'
--
-- rankToShape :: Map VName Int -> VName -> Shape SComp
-- rankToShape rs x = Shape $ replicate (rs M.! x) SDim
--
-- class SubstRanks a where
--  substRanks :: Map VName Int -> a -> SubstM a
--
-- instance SubstRanks (Shape SComp) where
--  substRanks rs = pure . foldMap instDim
--    where
--      instDim (SDim) = Shape $ pure SDim
--      instDim (SVar x) = rankToShape rs x
--
-- instance SubstRanks (TypeBase SComp u) where
--  substRanks rs t@(Scalar (TypeVar u (QualName [] x) []))
--    | rs M.! x > 0 = do
--        t' <- newTyVar $ baseName t
--        t' <- addTyVarSol
--        arrayOfWithAliases u (rankToShape rs x) t
--  substRanks rs (Scalar (Arrow u p d ta (RetType retdims tr))) =
--    Scalar (Arrow u p d (substRanks rs ta) (RetType retdims (substRanks rs tr)))
--  substRanks _ t = t
--
-- instance SubstRanks Ct where
--  substRanks rs (CtEq t1 t2) = CtEq <$> substRanks rs t1 <*> substRanks rs t2
--  substRanks _ _ = error ""
