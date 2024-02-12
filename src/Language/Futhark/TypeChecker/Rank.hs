module Language.Futhark.TypeChecker.Rank (rankAnalysis) where

import Control.Monad.Reader
import Control.Monad.State
import Data.List qualified as L
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
import System.IO.Unsafe
import System.Process

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

rankAnalysis :: Bool -> VNameSource -> Int -> [Ct] -> TyVars -> Maybe ([Ct], TyVars, VNameSource, Int)
rankAnalysis use_python vns counter cs tyVars = do
  traceM $ unlines ["## rankAnalysis prog", prettyString prog]
  rank_map <-
    if use_python
      then do
        -- traceM $ linearProgToPulp prog
        parseRes $
          unsafePerformIO $
            readProcess "python" [] $
              linearProgToPulp prog
      else do
        (_size, ranks) <- branchAndBound lp
        pure $ (fromJust . (ranks V.!?)) <$> inv_var_map
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
            substCounter = counter,
            substNewCts = mempty
          }
      (cs', state') =
        runSubstM initEnv initState $
          substRanks $
            filter (not . isCtAM) cs
  pure (cs' <> substNewCts state', substTyVars state' <> tyVars, substNameSource state', substCounter state')
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

    rm_subscript x = fromMaybe x $ lookup x $ zip "₀₁₂₃₄₅₆₇₈₉" "0123456789"
    vname_to_pulp_var = M.mapWithKey (\k _ -> map rm_subscript $ show $ prettyName k) inv_var_map
    pulp_var_to_vname =
      M.fromListWith (error "oh no!") [(v, k) | (k, v) <- M.toList vname_to_pulp_var]

    parseRes :: String -> Maybe (Map VName Int)
    parseRes s = do
      (status : vars) <- trimToStart $ lines s
      if not (success status)
        then Nothing
        else do
          pure $ M.fromList $ catMaybes $ map readVar vars
      where
        trimToStart [] = Nothing
        trimToStart (l : ls)
          | "status" `L.isPrefixOf` l = Just (l : ls)
          | otherwise = trimToStart ls
        success l =
          (read $ drop (length ("status: " :: [Char])) l) == (1 :: Int)
        readVar xs =
          let (v, _ : value) = L.span (/= ':') xs
           in Just (fromJust $ pulp_var_to_vname M.!? v, read value)

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
    substCounter :: !Int,
    substNewCts :: [Ct]
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
  shape <- rankToShape t
  modify $ \s ->
    s
      { substNewVars = M.insert t t' $ substNewVars s,
        substNewCts =
          substNewCts s
            ++ [ CtEq
                   (Scalar (TypeVar mempty (QualName [] t) []))
                   (arrayOf shape (Scalar (TypeVar mempty (QualName [] t') [])))
               ]
      }
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
  pure $ Shape $ replicate (fromJust $ rs M.!? x) SDim

addRankInfo :: TyVar -> SubstM ()
addRankInfo t = do
  rs <- asks envRanks
  -- unless (fromMaybe (error $ prettyString t) (rs M.!? t) == 0) $ do
  unless (fromMaybe 0 (rs M.!? t) == 0) $ do
    new_vars <- gets substNewVars
    maybe new_var (const $ pure ()) $ new_vars M.!? t
  where
    new_var = do
      t' <- newTyVar t
      old_tyvars <- asks envTyVars
      let info = fromJust $ old_tyvars M.!? t
      modify $ \s -> s {substTyVars = M.insert t' info $ substTyVars s}
      modify $ \s -> s {substTyVars = M.insert t (fst info, TyVarFree) $ substTyVars s}

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
  substRanks t@(Scalar (TypeVar u (QualName [] x) [])) =
    addRankInfo x >> pure t
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
