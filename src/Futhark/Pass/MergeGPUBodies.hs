module Futhark.Pass.MergeGPUBodies (mergeGPUBodies) where

import Control.Monad
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Foldable
import qualified Data.IntMap as IM
import Data.IntSet ((\\))
import qualified Data.IntSet as IS
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Sequence ((|>))
import qualified Data.Sequence as SQ
import Futhark.Analysis.Alias
import Futhark.Construct (sliceDim)
import Futhark.Error
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.MonadFreshNames hiding (newName)
import Futhark.Pass

mergeGPUBodies :: Pass GPU GPU
mergeGPUBodies =
  Pass
    "merge GPU bodies"
    "Reorder and merge GPUBody constructs to reduce kernels executions."
    $ intraproceduralTransformationWithConsts onStms onFunDef . aliasAnalysis
  where
    onFunDef _ (FunDef entry attrs name types params body) =
      FunDef entry attrs name types params . fst <$> transformBody body
    onStms stms =
      fst <$> transformStms stms

transformLambda :: LambdaT (Aliases GPU) -> PassM (LambdaT GPU, FreeVars)
transformLambda (Lambda params body types) = do
  (body', free) <- transformBody body
  pure (Lambda params body' types, free)

transformBody :: BodyT (Aliases GPU) -> PassM (BodyT GPU, FreeVars)
transformBody (Body _ stms res) = do
  (stms', free) <- transformStms stms
  pure (Body () stms' res, free)

transformStms :: Stms (Aliases GPU) -> PassM (Stms GPU, FreeVars)
transformStms stms = do
  let m = mapM_ handleStm stms >> collapse
  grp <- statePrelude . snd <$> runStateT m initialState

  let stms' = groupStms grp
  let free = groupFreeVars grp \\ groupBindings grp

  pure (stms', free)

handleStm :: Stm (Aliases GPU) -> MergeM ()
handleStm (Let pat (StmAux cs attrs _) e) = do
  (e', free) <- lift (transformExp e)
  let pat' = removePatAliases pat
  let stm' = Let pat' (StmAux cs attrs ()) e'

  -- Usage cannot pass site of binding.
  -- Consumption cannot pass site of usage by any alias.

  let consumed = namesToSet (consumedInExp e)
  let usage = Usage {
    usageBindings = IS.fromList $ map (baseTag . patElemName) (patElems pat'),
    usageFreeVars = free <> freeVars pat' <> freeVars cs,
    usageAliases = namesToSet (fold $ expAliases e)
  }

  case e' of
    Op (GPUBody _ (Body _ _ res)) -> do
      move <- canMergeGPUBodies usage consumed
      unless move collapse
      moveToInterlude stm' usage
      mapM_ (uncurry stores) (zip (patElems pat') (map resSubExp res))
    _ -> do
      move <- canMoveToPrelude usage consumed
      if move
        then moveToPrelude stm' usage
        else moveToPostlude stm' usage

transformExp :: Exp (Aliases GPU) -> PassM (Exp GPU, FreeVars)
transformExp e =
  case e of
    BasicOp {} -> pure (removeExpAliases e, freeVars e)
    Apply {} -> pure (removeExpAliases e, freeVars e)
    If c tbody fbody dec -> do
      (tbody', t_free) <- transformBody tbody
      (fbody', f_free) <- transformBody fbody
      let free = freeVars c <> t_free <> f_free <> freeVars dec
      pure (If c tbody' fbody' dec, free)
    DoLoop merge lform body -> do
      (body', body_free) <- transformBody body
      let (params, args) = unzip merge
      let free = body_free <> freeVars params <> freeVars args <> freeVars lform

      let scope = scopeOf lform <> scopeOfFParams params
      let bound = IS.fromList $ map baseTag (M.keys scope)
      let free' = free \\ bound

      let dummy = DoLoop merge lform (Body (bodyDec body) SQ.empty [])
      let DoLoop merge' lform' _ = removeExpAliases dummy

      pure (DoLoop merge' lform' body', free')
    WithAcc inputs lambda -> do
      (inputs', frees) <- unzip <$> mapM transformWithAccInput inputs
      (lambda', free) <- transformLambda lambda
      pure (WithAcc inputs' lambda', free <> fold frees)
    Op {} ->
      -- A GPUBody cannot be nested into other HostOp constructs.
      pure (removeExpAliases e, freeVars e)

transformWithAccInput :: WithAccInput (Aliases GPU)
                      -> PassM (WithAccInput GPU, FreeVars)
transformWithAccInput (shape, arrs, op) = do
  (op', free) <- case op of
    Nothing -> pure (Nothing, mempty)
    Just (f, nes) -> do
      (f', free) <- transformLambda f
      pure (Just (f', nes), free <> freeVars nes)
  let free' = free <> freeVars shape <> freeVars arrs
  pure ((shape, arrs, op'), free')

freeVars :: FreeIn a => a -> IS.IntSet
freeVars = namesToSet . freeIn

namesToSet :: Names -> IS.IntSet
namesToSet = IS.fromList . map baseTag . namesToList

stores :: PatElemT Type -> SubExp -> MergeM ()
stores (PatElem n t) se | isArray t =
  let row_t = fromJust (peelArray 1 t)
   in modify $ \st -> let stored = stateMemStored st
                          stored' = IM.insert (baseTag n) (se, row_t) stored
                       in st {stateMemStored = stored'}
stores pe se = pe `aliases` se

aliases :: PatElemT Type -> SubExp -> MergeM ()
aliases (PatElem n _) se =
  modify $ \st -> st {stateAliases = IM.insert (baseTag n) se (stateAliases st)}

isArray :: ArrayShape shape => TypeBase shape u -> Bool
isArray t = arrayRank t > 0

canMergeGPUBodies :: Usage -> Consumption -> MergeM Bool
canMergeGPUBodies usage consumed = do
  st <- get
  let stored = stateMemStored st
  let aliased = stateAliases st
  let isAlias i = IM.member i aliased || IM.member i stored

  -- Can use aliased value instead of its alias, allowing a statement to
  -- be moved before the alias given the alias would not be consumed.
  let usage' = usage {
    usageFreeVars = IS.filter (not . isAlias) (usageFreeVars usage)
  }

  canMoveBeforePostlude usage' consumed

canMoveBeforePostlude :: Usage -> Consumption -> MergeM Bool
canMoveBeforePostlude usage consumed = do
  postlude <- gets statePostlude
  let bound = groupBindings postlude
  let aliased = groupAliases postlude

  let used = usageFreeVars usage

  pure (IS.disjoint bound used && IS.disjoint aliased consumed)

canMoveToPrelude :: Usage -> Consumption -> MergeM Bool
canMoveToPrelude usage consumed = do
  canMove <- canMoveBeforePostlude usage consumed

  interlude <- gets stateInterlude
  let bound = groupBindings interlude
  let aliased = groupAliases interlude

  let used = usageFreeVars usage

  pure (canMove && IS.disjoint bound used && IS.disjoint aliased consumed)

moveToPrelude :: Stm GPU -> Usage -> MergeM ()
moveToPrelude stm usage =
  modify $ \st -> st {statePrelude = (stm, usage) `moveTo` statePrelude st}

moveToInterlude :: Stm GPU -> Usage -> MergeM ()
moveToInterlude stm usage =
  modify $ \st -> st {stateInterlude = (stm, usage) `moveTo` stateInterlude st}

moveToPostlude :: Stm GPU -> Usage -> MergeM ()
moveToPostlude stm usage = do
  stored <- gets stateMemStored
  aliased <- gets stateAliases

  -- Record aliases of GPUBody results to enable merging beyond those.
  case stm of
    Let (Pat [a]) _ (BasicOp (SubExp (Var n))) ->
      if isArray (patElemDec a)
        then case IM.lookup (baseTag n) stored of
          Nothing -> pure ()
          Just (se, _) -> a `stores` se
        else case IM.lookup (baseTag n) aliased of
          Nothing -> pure ()
          Just se -> a `aliases` se
    Let (Pat [a]) _ (BasicOp (Index arr slice))
      | Just (se, t) <- IM.lookup (baseTag arr) stored
      , DimFix idx : dims <- unSlice slice
      , idx == intConst Int64 0
      , and $ zipWith (\sd ad -> sd == sliceDim ad) dims (arrayDims t)
      -> a `aliases` se
    _ -> pure ()

  modify $ \st -> st {statePostlude = (stm, usage) `moveTo` statePostlude st}

moveTo :: (Stm GPU, Usage) -> Group -> Group
moveTo (stm, usage) grp =
  grp {
        groupStms = groupStms grp |> stm,
        groupUsage = groupUsage grp <> usage
      }

collapse :: MergeM ()
collapse = do
  mergeInterlude
  modify $ \st -> st {
      statePrelude = statePrelude st <> stateInterlude st <> statePostlude st,
      stateInterlude = mempty,
      statePostlude = mempty,
      stateMemStored = mempty,
      stateAliases = mempty
    }

mergeInterlude :: MergeM ()
mergeInterlude = do
  stms <- gets (groupStms . stateInterlude)
  info <- AliasInfo <$> gets stateMemStored <*> gets stateAliases

  stms' <- if SQ.length stms < 2
              then pure stms
              else SQ.singleton <$> foldrM (merge info) empty stms

  modify $ \st -> st {stateInterlude = (stateInterlude st) {groupStms = stms'}}
  where
    empty = Let mempty (StmAux mempty mempty ()) noop
    noop = Op (GPUBody [] (Body () SQ.empty []))

    merge :: AliasInfo -> Stm GPU -> Stm GPU -> MergeM (Stm GPU)
    merge info stm0 stm1
      | Let pat0 (StmAux cs0 attrs0 _) (Op (GPUBody types0 body)) <- stm0
      , Let pat1 (StmAux cs1 attrs1 _) (Op (GPUBody types1 body1)) <- stm1
      = do
        Body _ stms0 res0 <- execRewrite (rewriteBody body) info
        let Body _ stms1 res1 = body1

            pat' = pat0 <> pat1
            aux' = StmAux (cs0 <> cs1) (attrs0 <> attrs1) ()
            types' = types0 ++ types1
            body' = Body () (stms0 <> stms1) (res0 <> res1)

         in pure (Let pat' aux' (Op (GPUBody types' body')))
    merge _ _ _ =
      compilerBugS "mergeInterlude: cannot merge non-GPUBody statements"

type FreeVars = IS.IntSet
type Consumption = IS.IntSet

data Usage = Usage {
    usageBindings :: IS.IntSet,
    usageFreeVars :: FreeVars,
    usageAliases :: IS.IntSet
  }

instance Semigroup Usage where
  a <> b =
    Usage {
      usageBindings = usageBindings a <> usageBindings b,
      usageFreeVars = usageFreeVars a <> usageFreeVars b,
      usageAliases = usageAliases a <> usageAliases b
    }

instance Monoid Usage where
  mempty = Usage {
      usageBindings = mempty,
      usageFreeVars = mempty,
      usageAliases = mempty
    }

data Group = Group {
    groupStms :: Stms GPU,
    groupUsage :: Usage
  }

groupBindings :: Group -> IS.IntSet
groupBindings = usageBindings . groupUsage

groupFreeVars :: Group -> FreeVars
groupFreeVars = usageFreeVars . groupUsage

groupAliases :: Group -> IS.IntSet
groupAliases = usageAliases . groupUsage

instance Semigroup Group where
  a <> b =
    Group {
      groupStms = groupStms a <> groupStms b,
      groupUsage = groupUsage a <> groupUsage b
    }

instance Monoid Group where
  mempty = Group {
      groupStms = mempty,
      groupUsage = mempty
    }

data State = State {
    statePrelude :: Group,
    stateInterlude :: Group,
    statePostlude :: Group,
    stateMemStored :: IM.IntMap (SubExp, Type),
    stateAliases :: IM.IntMap SubExp
  }

initialState :: State
initialState = State {
    statePrelude = mempty,
    stateInterlude = mempty,
    statePostlude = mempty,
    stateMemStored = mempty,
    stateAliases = mempty
  }

type MergeM = StateT State PassM

type RewriteM = StateT (Stms GPU) (R.ReaderT AliasInfo MergeM)

data AliasInfo = AliasInfo {
    infoStored :: IM.IntMap (SubExp, Type),
    infoAliased :: IM.IntMap SubExp
  }

-- | Retrieve a function of the current environment.
asks :: (AliasInfo -> a) -> RewriteM a
asks = lift . R.asks

execRewrite :: RewriteM (BodyT GPU) -> AliasInfo -> MergeM (BodyT GPU)
execRewrite m info = fst <$> R.runReaderT (runStateT m' SQ.empty) info
  where
    m' = do Body _ stms res <- m
            prelude <- get
            pure (Body () (prelude <> stms) res)

rewriteBody :: BodyT GPU -> RewriteM (BodyT GPU)
rewriteBody (Body _ stms res) =
  Body () <$> rewriteStms stms <*> rewriteResult res

rewriteStms :: Stms GPU -> RewriteM (Stms GPU)
rewriteStms = mapM rewriteStm

rewriteStm :: Stm GPU -> RewriteM (Stm GPU)
rewriteStm (Let (Pat pes) (StmAux cs attrs _) e) = do
  pat' <- Pat <$> mapM rewritePatElem pes
  cs' <- rewriteCerts cs
  e' <- rewriteExp e
  pure $ Let pat' (StmAux cs' attrs ()) e'

rewritePatElem :: PatElemT Type -> RewriteM (PatElemT Type)
rewritePatElem (PatElem n t) =
  PatElem n <$> rewriteType t

rewriteExp :: Exp GPU -> RewriteM (Exp GPU)
rewriteExp e = do
  stored <- asks infoStored
  case e of
    BasicOp (Index arr slice)
      | Just (se, _) <- IM.lookup (baseTag arr) stored
      , [DimFix idx] <- unSlice slice
      , idx == intConst Int64 0
      -> pure $ BasicOp (SubExp se)

    BasicOp (Index arr slice)
      | Just (Var src, _) <- IM.lookup (baseTag arr) stored
      , DimFix idx : dims <- unSlice slice
      , idx == intConst Int64 0
      -> pure $ BasicOp $ Index src (Slice dims)

    _ -> mapExpM rewriter e
  where
    rewriter = Mapper {
          mapOnSubExp = rewriteSubExp,
          mapOnBody = const rewriteBody,
          mapOnVName = rewriteName,
          mapOnRetType = rewriteExtType,
          mapOnBranchType = rewriteExtType,
          mapOnFParam = rewriteParam,
          mapOnLParam = rewriteParam,
          mapOnOp = const opError
      }

    opError = compilerBugS "rewriteExp: unhandled HostOp in GPUBody"

rewriteResult :: Result -> RewriteM Result
rewriteResult = mapM rewriteSubExpRes

rewriteSubExpRes :: SubExpRes -> RewriteM SubExpRes
rewriteSubExpRes (SubExpRes cs se) =
  SubExpRes <$> rewriteCerts cs <*> rewriteSubExp se

rewriteCerts :: Certs -> RewriteM Certs
rewriteCerts (Certs cs) =
  Certs <$> mapM rewriteName cs

rewriteType :: TypeBase Shape u -> RewriteM (TypeBase Shape u)
-- Note: mapOnType also maps the VName token of accumulators
rewriteType = mapOnType rewriteSubExp

rewriteExtType :: TypeBase ExtShape u -> RewriteM (TypeBase ExtShape u)
-- Note: mapOnExtType also maps the VName token of accumulators
rewriteExtType = mapOnExtType rewriteSubExp

rewriteParam :: Param (TypeBase Shape u) -> RewriteM (Param (TypeBase Shape u))
rewriteParam (Param attrs n t) =
  Param attrs n <$> rewriteType t

rewriteSubExp :: SubExp -> RewriteM SubExp
rewriteSubExp (Constant c) = pure (Constant c)
rewriteSubExp (Var n) = do
  aliased <- asks infoAliased
  case IM.lookup (baseTag n) aliased of
    Just se -> pure se
    Nothing -> do
      stored <- asks infoStored
      case IM.lookup (baseTag n) stored of
        Just (se, t) -> Var <$> addArray se t
        Nothing -> pure (Var n)
  where
    addArray se row_t = do
      name <- newName "arr"
      let t = row_t `arrayOfRow` intConst Int64 1

      let pat = Pat [PatElem name t]
      let aux = StmAux mempty mempty ()
      let e = BasicOp (ArrayLit [se] row_t)

      modify (|> Let pat aux e)
      pure name

rewriteName :: VName -> RewriteM VName
rewriteName n = do
  se <- rewriteSubExp (Var n)
  case se of
    Var n' -> pure n'
    Constant c -> do
      name <- newName "cnst"
      let t = Prim (primValueType c)

      let pat = Pat [PatElem name t]
      let aux = StmAux mempty mempty ()
      let e = BasicOp (SubExp se)

      modify (|> Let pat aux e)
      pure name

newName :: String -> RewriteM VName
newName s = lift $ lift $ lift (newNameFromString s)
