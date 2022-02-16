module Futhark.Pass.MergeGPUBodies (mergeGPUBodies) where

import Control.Monad
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Foldable
import qualified Data.IntMap as IM
import Data.IntSet ((\\))
import qualified Data.IntSet as IS
import Data.Maybe (fromJust)
import Data.Sequence ((|>))
import qualified Data.Sequence as SQ
import Futhark.Analysis.Alias
import Futhark.Construct (sliceDim)
import Futhark.Error
import Futhark.IR.Prop.Aliases
import Futhark.IR.GPU hiding (Names)
import Futhark.MonadFreshNames hiding (newName)
import Futhark.Pass

mergeGPUBodies :: Pass GPU GPU
mergeGPUBodies =
  Pass
    "merge GPU bodies"
    "Reorder and merge GPUBody constructs to reduce kernels executions."
    $ intraproceduralTransformationWithConsts onStms onFunDef
  where
    onFunDef _ fd = do
      body' <- onBody (funDefBody fd)
      pure fd {funDefBody = body'}
    onBody body = do
      stms' <- onStms (bodyStms body)
      pure body {bodyStms = stms'}
    onStms stms = do
      (stms', _, _) <- transformStms stms
      pure stms'

transformBody :: BodyT GPU -> PassM (BodyT GPU, Usage, Consumption)
transformBody (Body dec stms res) = do
  (stms', usage, consumed) <- transformStms stms
  pure (Body dec stms' res, usage, consumed)

transformStms :: Stms GPU -> PassM (Stms GPU, Usage, Consumption)
transformStms stms = do
  let m = mapM_ handleStm stms >> collapse
  st <- snd <$> runStateT m initialState

  let prelude = statePrelude st
  let stms' = groupStms prelude
  let usage = groupUsage prelude \\ groupBindings prelude
  let consumed = IS.intersection usage (stateConsumed st)

  pure (stms', usage, consumed)

transformExp :: Exp GPU -> PassM (Exp GPU, Usage, Consumption)
transformExp e =
  case e of
    BasicOp {} -> pure (e, used, consumed)
    Apply {} -> pure (e, used, consumed)
    -- TODO
  where
    used = namesToSet (freeIn e)

    -- TODO: Verify that this consumption analysis is sufficient.
    -- TODO: Not sufficient! Postlude might alias a prelude value
    -- which can be consumed without being in the Postlude usage
    -- set. Bummer! Need to rework aliasing tracking...
    consumed = namesToSet $ consumedInExp (analyseExp mempty e)

    namesToSet = IS.fromList . map baseTag . namesToList

type MergeM = StateT State PassM

type Bindings = IS.IntSet
type Usage = IS.IntSet
type Consumption = IS.IntSet

data State = State {
    stateConsumed :: Consumption,
    statePrelude :: Group,
    stateInterlude :: Group,
    statePostlude :: Group,
    stateMemStored :: IM.IntMap (SubExp, Type),
    stateAliases :: IM.IntMap SubExp
  }

data Group = Group {
    groupStms :: Stms GPU,
    groupBindings :: Bindings,
    groupUsage :: Usage
  }

instance Semigroup Group where
  a <> b =
    Group {
      groupStms = groupStms a <> groupStms b,
      groupBindings = groupBindings a <> groupBindings b,
      groupUsage = groupUsage a <> groupUsage b
    }

instance Monoid Group where
  mempty = Group {
      groupStms = mempty,
      groupBindings = mempty,
      groupUsage = mempty
    }

initialState :: State
initialState = State {
    stateConsumed = mempty,
    statePrelude = mempty,
    stateInterlude = mempty,
    statePostlude = mempty,
    stateMemStored = mempty,
    stateAliases = mempty
  }

handleStm :: Stm GPU -> MergeM ()
handleStm (Let pat aux e) = do
  (e', usage, consumed) <- lift (transformExp e)
  let usage' = usage <> certUsage (stmAuxCerts aux)
  tellConsumed consumed

  let stm' = Let pat aux e'
  case e' of
    Op (GPUBody _ (Body _ _ res)) -> do
      move <- canMergeGPUBodies usage' consumed
      unless move collapse
      moveToInterlude stm' usage'
      mapM_ (uncurry stores) (zip (patElems pat) (map resSubExp res))
    _ -> do
      move <- canMoveToPrelude usage' consumed
      if move
        then moveToPrelude stm' usage'
        else moveToPostlude stm' usage'
  where
    certUsage = IS.fromList . (map baseTag) . unCerts

    tellConsumed consumed =
      modify $ \st -> st {stateConsumed = (stateConsumed st) <> consumed}

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
canMergeGPUBodies operands consumes = do
  st <- get
  let stored = stateMemStored st
  let aliased = stateAliases st
  let isAlias i = IM.member i aliased || IM.member i stored

  -- Can use aliased value instead of its alias, allowing a statement to
  -- be moved before the alias unless the alias would be consumed.
  let operands' = IS.filter isAlias operands
  canMoveBeforePostlude operands' consumes

canMoveBeforePostlude :: Usage -> Consumption -> MergeM Bool
canMoveBeforePostlude operands consumes = do
  postlude <- gets statePostlude
  let bindings = groupBindings postlude 
  let usage = groupUsage postlude

  pure (IS.disjoint bindings operands && IS.disjoint usage consumes)

canMoveToPrelude :: Usage -> Consumption -> MergeM Bool
canMoveToPrelude operands consumes = do
  canMove <- canMoveBeforePostlude operands consumes

  interlude <- gets stateInterlude
  let bindings = groupBindings interlude 
  let usage = groupUsage interlude

  pure (canMove && IS.disjoint bindings operands && IS.disjoint usage consumes)

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
  let bindings = map (baseTag . patElemName) $ patElems (stmPat stm)
   in grp {
        groupStms = groupStms grp |> stm,
        groupBindings = groupBindings grp <> IS.fromList bindings,
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
  names <- Names <$> gets stateMemStored <*> gets stateAliases

  stms' <- if SQ.length stms < 2
              then pure stms
              else SQ.singleton <$> foldrM (merge names) empty stms

  modify $ \st -> st {stateInterlude = (stateInterlude st) {groupStms = stms'}}
  where
    empty = Let mempty (StmAux mempty mempty ()) noop
    noop = Op (GPUBody [] (Body () SQ.empty []))

    merge :: Names -> Stm GPU -> Stm GPU -> MergeM (Stm GPU)
    merge names stm0 stm1
      | Let pat0 (StmAux cs0 attrs0 _) (Op (GPUBody types0 body)) <- stm0
      , Let pat1 (StmAux cs1 attrs1 _) (Op (GPUBody types1 body1)) <- stm1
      = do
        Body _ stms0 res0 <- execRewrite (rewriteBody body) names
        let Body _ stms1 res1 = body1

            pat' = pat0 <> pat1
            aux' = StmAux (cs0 <> cs1) (attrs0 <> attrs1) ()
            types' = types0 ++ types1
            body' = Body () (stms0 <> stms1) (res0 <> res1)

         in pure (Let pat' aux' (Op (GPUBody types' body')))
    merge _ _ _ =
      compilerBugS "mergeInterlude: cannot merge non-GPUBody statements"

type RewriteM = StateT (Stms GPU) (R.ReaderT Names MergeM)

data Names = Names {
    namesStored :: IM.IntMap (SubExp, Type),
    namesAliased :: IM.IntMap SubExp
  }

-- | Retrieve a function of the current environment.
asks :: (Names -> a) -> RewriteM a
asks = lift . R.asks

execRewrite :: RewriteM (BodyT GPU) -> Names -> MergeM (BodyT GPU)
execRewrite m names = fst <$> R.runReaderT (runStateT m' SQ.empty) names
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
  stored <- asks namesStored
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
  aliased <- asks namesAliased
  case IM.lookup (baseTag n) aliased of
    Just se -> pure se
    Nothing -> do
      stored <- asks namesStored
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
