module Language.Futhark.TypeChecker.Rank
  ( rankAnalysis,
    rankAnalysis1,
    CtAM (..),
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Functor.Identity
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Futhark.IR.Pretty ()
import Futhark.Solve.GLPK
import Futhark.Solve.LP hiding (Constraint, LSum, LinearProg)
import Futhark.Solve.LP qualified as LP
import Futhark.Util (debugTraceM)
import Futhark.Util.Pretty
import Language.Futhark hiding (ScalarType)
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Constraints
import Language.Futhark.TypeChecker.Monad
import System.IO.Unsafe

type LSum = LP.LSum VName Int

type Constraint = LP.Constraint VName Int

type LinearProg = LP.LinearProg VName Int

class Rank a where
  rank :: a -> LSum

instance Rank VName where
  rank = var

instance Rank SComp where
  rank SDim = constant 1
  rank (SVar v) = var v

instance Rank (Shape SComp) where
  rank = foldr (\d r -> rank d ~+~ r) (constant 0) . shapeDims

instance Rank (ScalarTypeBase SComp u) where
  rank Prim {} = constant 0
  rank (TypeVar _ (QualName [] v) []) = var v
  rank (TypeVar {}) = constant 0
  rank (Arrow {}) = constant 0
  rank (Record {}) = constant 0
  rank (Sum {}) = constant 0

instance Rank (TypeBase SComp u) where
  rank (Scalar t) = rank t
  rank (Array _ shape t) = rank shape ~+~ rank t

distribAndSplitArrows :: CtTy d -> [CtTy d]
distribAndSplitArrows (CtEq r t1 t2) =
  splitArrows $ CtEq r (distribute t1) (distribute t2)
  where
    distribute :: TypeBase dim as -> TypeBase dim as
    distribute (Array u s (Arrow _ _ _ ta (RetType rd tr))) =
      Scalar $
        Arrow
          u
          Unnamed
          mempty
          (arrayOf s ta)
          (RetType rd $ distribute $ arrayOfWithAliases Nonunique s tr)
    distribute t = t

    splitArrows
      ( CtEq
          reason
          (Scalar (Arrow _ _ _ t1a (RetType _ t1r)))
          (Scalar (Arrow _ _ _ t2a (RetType _ t2r)))
        ) =
        splitArrows (CtEq reason t1a t2a) ++ splitArrows (CtEq reason t1r' t2r')
        where
          t1r' = t1r `setUniqueness` NoUniqueness
          t2r' = t2r `setUniqueness` NoUniqueness
    splitArrows c = [c]

distribAndSplitCnstrs :: CtTy d -> [CtTy d]
distribAndSplitCnstrs ct@(CtEq r t1 t2) =
  ct : splitCnstrs (CtEq r (distribute1 t1) (distribute1 t2))
  where
    distribute1 :: TypeBase dim as -> TypeBase dim as
    distribute1 (Array u s (Record ts1)) =
      Scalar $ Record $ fmap (arrayOfWithAliases u s) ts1
    distribute1 (Array u s (Sum cs)) =
      Scalar $ Sum $ (fmap . fmap) (arrayOfWithAliases u s) cs
    distribute1 t = t

    -- FIXME. Should check for key set equality here.
    splitCnstrs (CtEq reason (Scalar (Record ts1)) (Scalar (Record ts2))) =
      concat $ zipWith (\x y -> distribAndSplitCnstrs $ CtEq reason x y) (M.elems ts1) (M.elems ts2)
    splitCnstrs (CtEq reason (Scalar (Sum cs1)) (Scalar (Sum cs2))) =
      concat $ concat $ (zipWith . zipWith) (\x y -> distribAndSplitCnstrs $ CtEq reason x y) (M.elems cs1) (M.elems cs2)
    splitCnstrs _ = []

data RankState = RankState
  { rankBinVars :: Map VName VName,
    rankCounter :: !Int,
    rankConstraints :: [Constraint],
    rankObj :: LSum
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
            rankConstraints = [bin bv, var bv ~<=~ var sv] <> rankConstraints s
          }
      pure bv
    Just bv -> pure bv

addConstraints :: [Constraint] -> RankM ()
addConstraints cs =
  modify $ \s -> s {rankConstraints = cs <> rankConstraints s}

addConstraint :: Constraint -> RankM ()
addConstraint = addConstraints . pure

addObj :: SVar -> RankM ()
addObj sv =
  modify $ \s -> s {rankObj = rankObj s ~+~ var sv}

addCt :: CtTy SComp -> RankM ()
addCt (CtEq _ t1 t2) = addConstraint $ rank t1 ~==~ rank t2

addCtAM :: CtAM -> RankM ()
addCtAM (CtAM _ r m f) = do
  b_r <- binVar r
  b_m <- binVar m
  b_max <- VName "c_max" <$> incCounter
  tr <- VName ("T_" <> baseName r) <$> incCounter
  addConstraints [bin b_max, var b_max ~<=~ var tr]
  addConstraints $ oneIsZero (b_r, r) (b_m, m)
  addConstraints $ LP.max b_max (constant 0) (rank r ~-~ rank f) (var tr)
  addObj m
  addObj tr

addTyVarInfo :: TyVar -> (Int, TyVarInfo d) -> RankM ()
addTyVarInfo _ (_, TyVarFree {}) = pure ()
addTyVarInfo tv (_, TyVarPrim {}) =
  addConstraint $ rank tv ~==~ constant 0
addTyVarInfo tv (_, TyVarRecord {}) =
  addConstraint $ rank tv ~==~ constant 0
addTyVarInfo tv (_, TyVarSum {}) =
  addConstraint $ rank tv ~==~ constant 0

mkLinearProg :: [CtTy SComp] -> [CtAM] -> TyVars d -> LinearProg
mkLinearProg cs cs_am tyVars =
  LP.LinearProg
    { optType = Minimize,
      objective = rankObj finalState,
      -- let shape_vars = M.keys $ rankBinVars finalState
      -- in foldr (\sv s -> var sv ~+~ s) (constant 0) shape_vars,
      constraints = rankConstraints finalState
    }
  where
    initState =
      RankState
        { rankBinVars = mempty,
          rankCounter = 0,
          rankConstraints = mempty,
          rankObj = constant 0
        }
    buildLP = do
      mapM_ addCt cs
      mapM_ addCtAM cs_am
      mapM_ (uncurry addTyVarInfo) $ M.toList tyVars
    finalState = flip execState initState $ runRankM buildLP

ambigCheckLinearProg :: LinearProg -> (Int, Map VName Int) -> LinearProg
ambigCheckLinearProg prog (opt, ranks) =
  prog
    { constraints =
        -- https://yetanothermathprogrammingconsultant.blogspot.com/2011/10/integer-cuts.html
        [ lsum (var <$> M.keys one_bins)
            ~-~ lsum (var <$> M.keys zero_bins)
            ~<=~ constant (fromIntegral $ length one_bins)
            ~-~ constant 1,
          objective prog ~==~ constant (fromIntegral opt)
        ]
          ++ constraints prog
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
      guard (fst sol' /= 0)
      let prog'' = ambigCheckLinearProg prog' sol'
      sol'' <- run_glpk prog''
      pure (prog'', sol'')
    takeSolns [] = []
    takeSolns (Nothing : _) = []
    takeSolns (Just (_, (_, r)) : xs) = r : takeSolns xs

solveRankILP :: (MonadTypeChecker m) => SrcLoc -> LinearProg -> m [Map VName Int]
solveRankILP loc prog = do
  debugTraceM 3 $
    unlines
      [ "## solveRankILP",
        prettyString prog
      ]
  case enumerateRankSols prog of
    [] -> typeError loc mempty "Rank ILP cannot be solved."
    rs -> do
      debugTraceM 3 "## rank maps"
      forM_ (zip [0 :: Int ..] rs) $ \(i, r) ->
        debugTraceM 3 $
          unlines $
            "\n## rank map " <> prettyString i
              : map prettyString (M.toList r)
      pure rs

rankAnalysis1 ::
  (MonadTypeChecker m) =>
  SrcLoc ->
  ([CtTy SComp], [CtAM]) ->
  TyVars SComp ->
  M.Map TyVar (CtType SComp) ->
  [Pat ParamType] ->
  Exp ->
  Maybe (TypeExp Exp VName) ->
  m
    ( ([CtTy ()], M.Map TyVar (CtType ()), TyVars ()),
      [Pat ParamType],
      Exp,
      Maybe (TypeExp Exp VName)
    )
rankAnalysis1 loc (cs, cs_am) tyVars artificial params body retdecl = do
  solutions <- rankAnalysis loc (cs, cs_am) tyVars artificial params body retdecl
  case solutions of
    [sol] -> pure sol
    sols -> do
      let (_, _, bodies', _) = L.unzip4 sols
      typeError loc mempty $
        stack $
          [ "Rank ILP is ambiguous.",
            "Choices:"
          ]
            ++ map pretty bodies'

rankAnalysis ::
  (MonadTypeChecker m) =>
  SrcLoc ->
  ([CtTy SComp], [CtAM]) ->
  TyVars SComp ->
  M.Map TyVar (CtType SComp) ->
  [Pat ParamType] ->
  Exp ->
  Maybe (TypeExp Exp VName) ->
  m
    [ ( ([CtTy ()], M.Map TyVar (CtType ()), TyVars ()),
        [Pat ParamType],
        Exp,
        Maybe (TypeExp Exp VName)
      )
    ]
rankAnalysis _ ([], []) tyVars artificial params body retdecl = do
  (_, artificial', tyVars') <- substRankInfo ([], []) artificial tyVars mempty
  pure [(([], artificial', tyVars'), params, body, retdecl)]
rankAnalysis loc (cs, cs_am) tyVars artificial params body retdecl = do
  debugTraceM 3 $
    unlines
      [ "##rankAnalysis",
        "cs:",
        unlines $ map prettyString cs,
        "cs':",
        unlines $ map prettyString cs'
      ]
  rank_maps <- solveRankILP loc (mkLinearProg cs' cs_am tyVars)
  cts_tyvars' <- mapM (substRankInfo (cs, cs_am) artificial tyVars) rank_maps
  let bodys = map (`updAM` body) rank_maps
      params' = map ((`map` params) . updAMPat) rank_maps
      retdecls = map ((<$> retdecl) . updAMTypeExp) rank_maps
  pure $ L.zip4 cts_tyvars' params' bodys retdecls
  where
    cs' =
      foldMap distribAndSplitCnstrs $
        foldMap distribAndSplitArrows cs

type RankMap = M.Map VName Int

substRankInfo ::
  (MonadTypeChecker m) =>
  ([CtTy SComp], [CtAM]) ->
  M.Map VName (CtType SComp) ->
  TyVars SComp ->
  RankMap ->
  m ([CtTy ()], M.Map VName (CtType ()), TyVars ())
substRankInfo (cs, _cs_am) artificial tyVars rankmap = do
  ((cs', artificial', tyVars'), new_cs, new_tyVars) <-
    runSubstT tyVars rankmap $
      (,,)
        <$> traverse substRanksCt cs
        <*> traverse substRanksType artificial
        <*> substRanksTyVars tyVars
  pure (cs' <> new_cs, artificial', new_tyVars <> tyVars')

runSubstT ::
  (MonadTypeChecker m) =>
  TyVars SComp ->
  RankMap ->
  SubstT m a ->
  m (a, [CtTy ()], TyVars ())
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
  { envTyVars :: TyVars SComp,
    envRanks :: RankMap
  }

data SubstState = SubstState
  { substTyVars :: TyVars (),
    substNewVars :: Map TyVar TyVar,
    substNewCts :: [CtTy ()]
  }

instance MonadTrans SubstT where
  lift = SubstT . lift . lift

rankToShape :: (Monad m) => VName -> SubstT m (Shape ())
rankToShape x = do
  rs <- asks envRanks
  pure $ Shape $ replicate (fromJust $ rs M.!? x) ()

newTyVar :: (MonadTypeChecker m) => TyVar -> SubstT m TyVar
newTyVar t = do
  t' <- lift $ newTypeName (baseName t)
  shape <- rankToShape t
  loc <- asks ((locOf . snd . fromJust . (M.!? t)) . envTyVars)
  modify $ \s ->
    s
      { substNewVars = M.insert t t' $ substNewVars s,
        substNewCts =
          CtEq
            (Reason loc)
            (Scalar (TypeVar mempty (QualName [] t) []))
            (arrayOf shape (Scalar (TypeVar mempty (QualName [] t') [])))
            : substNewCts s
      }
  pure t'

addRankInfo :: (MonadTypeChecker m) => TyVar -> SubstT m ()
addRankInfo t = do
  rs <- asks envRanks
  if fromMaybe 0 (rs M.!? t) == 0
    then pure ()
    else do
      new_vars <- gets substNewVars
      maybe new_var (const $ pure ()) $ new_vars M.!? t
  where
    new_var = do
      t' <- newTyVar t
      old_tyvars <- asks envTyVars
      let (level, tvinfo) = fromJust $ old_tyvars M.!? t
          l = case tvinfo of
            TyVarFree _ tvinfo_l -> tvinfo_l
            _ -> Unlifted
      tvinfo' <- substRanksTyVarInfo tvinfo
      modify $ \s -> s {substTyVars = M.insert t' (level, tvinfo') $ substTyVars s}
      modify $ \s -> s {substTyVars = M.insert t (level, TyVarFree (locOf tvinfo) l) $ substTyVars s}

substRanksShape :: (Monad m) => Shape SComp -> SubstT m (Shape ())
substRanksShape = foldM (\s d -> (s <>) <$> instDim d) mempty
  where
    instDim SDim = pure $ Shape [()]
    instDim (SVar x) = rankToShape x

substRanksType :: (MonadTypeChecker m) => TypeBase SComp u -> SubstT m (TypeBase () u)
substRanksType (Scalar (TypeVar vn (QualName qs x) targs)) = do
  when (null qs) $ addRankInfo x
  targs' <- mapM onTypeArg targs
  pure $ Scalar $ TypeVar vn (QualName qs x) targs'
  where
    onTypeArg (TypeArgType t) = TypeArgType <$> substRanksType t
    -- SVar cannot occur as argument to abstract ype.
    onTypeArg (TypeArgDim _) = pure $ TypeArgDim ()
substRanksType (Scalar (Arrow u p d ta (RetType retdims tr))) = do
  ta' <- substRanksType ta
  tr' <- substRanksType tr
  pure $ Scalar (Arrow u p d ta' (RetType retdims tr'))
substRanksType (Scalar (Record fs)) =
  Scalar . Record <$> traverse substRanksType fs
substRanksType (Scalar (Sum cs)) =
  Scalar . Sum <$> (traverse . traverse) substRanksType cs
substRanksType (Scalar (Prim pt)) = pure $ Scalar $ Prim pt
substRanksType (Array u shape t) = do
  shape' <- substRanksShape shape
  t' <- substRanksType $ Scalar t
  pure $ arrayOfWithAliases u shape' t'

substRanksCt :: (MonadTypeChecker m) => CtTy SComp -> SubstT m (CtTy ())
substRanksCt (CtEq r t1 t2) =
  CtEq
    <$> traverse substRanksType r
    <*> substRanksType t1
    <*> substRanksType t2

substRanksTyVarInfo :: (MonadTypeChecker m) => TyVarInfo SComp -> SubstT m (TyVarInfo ())
substRanksTyVarInfo (TyVarFree loc l) = pure $ TyVarFree loc l
substRanksTyVarInfo (TyVarPrim loc ts) = pure $ TyVarPrim loc ts
substRanksTyVarInfo (TyVarRecord loc fs) =
  TyVarRecord loc <$> traverse substRanksType fs
substRanksTyVarInfo (TyVarSum loc cs) =
  TyVarSum loc <$> traverse (traverse substRanksType) cs

substRanksTyVars :: (MonadTypeChecker m) => TyVars SComp -> SubstT m (TyVars ())
substRanksTyVars = traverse $ \(lvl, tv) -> (lvl,) <$> substRanksTyVarInfo tv

updAM :: RankMap -> Exp -> Exp
updAM rank_map e =
  case e of
    AppExp (Apply f args loc) res ->
      let f' = updAM rank_map f
          args' = fmap (bimap (fmap $ second upd) (updAM rank_map)) args
       in AppExp (Apply f' args' loc) res
    AppExp (BinOp op t (x, Info (xv, xam)) (y, Info (yv, yam)) loc) res ->
      AppExp (BinOp op t (updAM rank_map x, Info (xv, upd xam)) (updAM rank_map y, Info (yv, upd yam)) loc) res
    OpSectionRight name t arg (Info (pa, t1a), Info (pb, t1b, argext, am)) t2 loc ->
      OpSectionRight
        name
        t
        (updAM rank_map arg)
        (Info (pa, t1a), Info (pb, t1b, argext, upd am))
        t2
        loc
    OpSectionLeft name t arg (Info (pa, t1a, argext, am), Info (pb, t1b)) (ret, retext) loc ->
      OpSectionLeft
        name
        t
        (updAM rank_map arg)
        (Info (pa, t1a, argext, upd am), Info (pb, t1b))
        (ret, retext)
        loc
    _ -> runIdentity $ astMap mapper e
  where
    dimToRank (Var (QualName [] x) _ _) =
      replicate (rank_map M.! x) (TupLit mempty mempty)
    dimToRank e' = error $ prettyString e'
    shapeToRank = Shape . foldMap dimToRank
    upd (AutoMap r m f) =
      AutoMap (shapeToRank r) (shapeToRank m) (shapeToRank f)
    mapper = identityMapper {mapOnExp = pure . updAM rank_map}

updAMPat :: RankMap -> Pat ParamType -> Pat ParamType
updAMPat rank_map p = runIdentity $ astMap m p
  where
    m = identityMapper {mapOnExp = pure . updAM rank_map}

updAMTypeExp ::
  RankMap ->
  TypeExp Exp VName ->
  TypeExp Exp VName
updAMTypeExp rank_map te = runIdentity $ astMap m te
  where
    m = identityMapper {mapOnExp = pure . updAM rank_map}
