module Language.Futhark.TypeChecker.Rank (rankAnalysis) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Functor.Identity
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Debug.Trace
import Futhark.Solve.GLPK
import Futhark.Solve.LP hiding (Constraint, LSum, LinearProg)
import Futhark.Solve.LP qualified as LP
import Language.Futhark hiding (ScalarType)
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Constraints
import Language.Futhark.TypeChecker.Monad
import System.IO.Unsafe

type LSum = LP.LSum VName Int

type Constraint = LP.Constraint VName Int

type LinearProg = LP.LinearProg VName Int

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
        Scalar $ Arrow NoUniqueness Unnamed mempty (arrayOf s ta) (RetType rd $ arrayOfWithAliases Nonunique s tr)
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

binVar :: VName -> RankM VName
binVar sv = do
  mbv <- gets ((M.!? sv) . rankBinVars)
  case mbv of
    Nothing -> do
      bv <- VName ("b_" <> baseName sv) <$> incCounter
      modify $ \s ->
        s
          { rankBinVars = M.insert sv bv $ rankBinVars s,
            rankConstraints = rankConstraints s ++ [bin bv, var bv ~<=~ var sv]
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
addTyVarInfo _ (_, TyVarFree) = pure ()
addTyVarInfo tv (_, TyVarPrim _) =
  addConstraint $ rank tv ~==~ constant 0
addTyVarInfo tv (_, TyVarRecord _) =
  addConstraint $ rank tv ~==~ constant 0
addTyVarInfo tv (_, TyVarSum _) =
  addConstraint $ rank tv ~==~ constant 0

mkLinearProg :: [Ct] -> TyVars -> LinearProg
mkLinearProg cs tyVars =
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
          rankCounter = 0,
          rankConstraints = mempty
        }
    buildLP = do
      mapM_ addCt cs
      mapM_ (uncurry addTyVarInfo) $ M.toList tyVars
    finalState = flip execState initState $ runRankM buildLP

ambigCheckLinearProg :: LinearProg -> (Int, Map VName Int) -> LinearProg
ambigCheckLinearProg prog (opt, ranks) =
  prog
    { constraints =
        constraints prog
          -- https://yetanothermathprogrammingconsultant.blogspot.com/2011/10/integer-cuts.html
          ++ [ lsum (var <$> M.keys one_bins)
                 ~-~ lsum (var <$> M.keys zero_bins)
                 ~<=~ constant (fromIntegral $ length one_bins)
                 ~-~ constant 1,
               objective prog ~==~ constant (fromIntegral opt)
             ]
    }
  where
    -- We really need to track which variables are binary in the LinearProg
    is_bin_var = ("b_" `L.isPrefixOf`) . baseString
    one_bins = M.filterWithKey (\k v -> is_bin_var k && v == 1) ranks
    zero_bins = M.filterWithKey (\k v -> is_bin_var k && v == 0) ranks
    lsum = foldr (~+~) (constant 0)

enumerateRankSols :: LinearProg -> [Map VName Int]
enumerateRankSols prog =
  take 5 $
    takeSolns $
      iterate next_sol $
        (prog,) <$> run_glpk prog
  where
    run_glpk = unsafePerformIO . glpk
    next_sol m = do
      (prog', sol') <- m
      let prog'' = ambigCheckLinearProg prog' sol'
      sol'' <- run_glpk prog''
      pure (prog'', sol'')
    takeSolns [] = []
    takeSolns (Nothing : _) = []
    takeSolns (Just (_, (_, r)) : xs) = r : takeSolns xs

solveRankILP :: (MonadTypeChecker m) => SrcLoc -> LinearProg -> m [Map VName Int]
solveRankILP loc prog = do
  traceM $
    unlines
      [ "## solveRankILP",
        prettyString prog
      ]
  case enumerateRankSols prog of
    [] -> typeError loc mempty "Rank ILP cannot be solved."
    rs -> do
      traceM "## rank maps"
      forM (zip [0 :: Int ..] rs) $ \(i, r) ->
        traceM $
          unlines $
            "\n## rank map " <> prettyString i
              : map prettyString (M.toList r)
      pure rs

rankAnalysis :: (MonadTypeChecker m) => SrcLoc -> [Ct] -> TyVars -> Exp -> m [(([Ct], TyVars), Exp)]
rankAnalysis _ [] tyVars body = pure [(([], tyVars), body)]
rankAnalysis loc cs tyVars body = do
  rank_maps <- solveRankILP loc (mkLinearProg (foldMap splitFuncs cs) tyVars)
  cts_tyvars' <- mapM (substRankInfo cs tyVars) rank_maps
  let bodys = map (flip updAM body) rank_maps
  pure $ zip cts_tyvars' bodys
  where
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

substRankInfo :: (MonadTypeChecker m) => [Ct] -> TyVars -> Map VName Int -> m ([Ct], TyVars)
substRankInfo cs tyVars rankmap = do
  (cs', new_cs, new_tyVars) <-
    runSubstT tyVars rankmap $
      substRanks $
        filter (not . isCtAM) cs
  pure (cs' <> new_cs, new_tyVars <> tyVars)
  where
    isCtAM (CtAM {}) = True
    isCtAM _ = False

runSubstT :: (MonadTypeChecker m) => TyVars -> Map VName Int -> SubstT m a -> m (a, [Ct], TyVars)
runSubstT tyVars rankmap (SubstT m) = do
  let env =
        SubstEnv
          { envTyVars = tyVars,
            envRanks = rankmap
          }

      s =
        SubstState
          { substTyVars = mempty,
            substNewVars = mempty,
            substNewCts = mempty
          }
  (a, s') <- runReaderT (runStateT m s) env
  pure (a, substNewCts s', substTyVars s')

newtype SubstT m a = SubstT (StateT SubstState (ReaderT SubstEnv m) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState SubstState,
      MonadReader SubstEnv
    )

data SubstEnv = SubstEnv
  { envTyVars :: TyVars,
    envRanks :: Map VName Int
  }

data SubstState = SubstState
  { substTyVars :: TyVars,
    substNewVars :: Map TyVar TyVar,
    substNewCts :: [Ct]
  }

instance MonadTrans SubstT where
  lift = SubstT . lift . lift

newTyVar :: (MonadTypeChecker m) => TyVar -> SubstT m TyVar
newTyVar t = do
  t' <- lift $ newTypeName (baseName t)
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

rankToShape :: (Monad m) => VName -> SubstT m (Shape SComp)
rankToShape x = do
  rs <- asks envRanks
  pure $ Shape $ replicate (fromJust $ rs M.!? x) SDim

addRankInfo :: (MonadTypeChecker m) => TyVar -> SubstT m ()
addRankInfo t = do
  rs <- asks envRanks
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
  substRanks :: (MonadTypeChecker m) => a -> SubstT m a

instance (SubstRanks a) => SubstRanks [a] where
  substRanks = mapM substRanks

instance SubstRanks (Shape SComp) where
  substRanks = foldM (\s d -> (s <>) <$> instDim d) mempty
    where
      instDim SDim = pure $ Shape $ pure SDim
      instDim (SVar x) = rankToShape x

instance SubstRanks (TypeBase SComp u) where
  substRanks t@(Scalar (TypeVar _ (QualName [] x) [])) =
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

updAM :: Map VName Int -> Exp -> Exp
updAM rank_map e =
  case e of
    AppExp (Apply f args loc) res ->
      let f' = updAM rank_map f
          args' =
            fmap
              ( bimap
                  (fmap $ bimap id upd)
                  (updAM rank_map)
              )
              args
       in AppExp (Apply f' args' loc) res
    AppExp (BinOp op t (x, Info (xv, xam)) (y, Info (yv, yam)) loc) res ->
      AppExp (BinOp op t (updAM rank_map x, Info (xv, upd xam)) (updAM rank_map y, Info (yv, upd yam)) loc) res
    _ -> runIdentity $ astMap m e
  where
    dimToRank (Var (QualName [] x) _ _) = rank_map M.! x
    dimToRank e = error $ prettyString e
    shapeToRank = sum . fmap dimToRank
    upd (AutoMap r m f) =
      AutoMapRank (shapeToRank r) (shapeToRank m) (shapeToRank f)
    m =
      identityMapper
        { mapOnExp = pure . updAM rank_map
        }
