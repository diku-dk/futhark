module Futhark.Pass.MergeGPUBodies (mergeGPUBodies) where

import Control.Monad
import Control.Monad.Trans.Class
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
      FunDef entry attrs name types params . fst <$> transformBody mempty body
    onStms stms =
      fst <$> transformStms mempty stms

type Dependencies = IS.IntSet

transformLambda ::
  AliasTable ->
  Lambda (Aliases GPU) ->
  PassM (Lambda GPU, Dependencies)
transformLambda aliases (Lambda params body types) = do
  (body', deps) <- transformBody aliases body
  pure (Lambda params body' types, deps)

transformBody ::
  AliasTable ->
  Body (Aliases GPU) ->
  PassM (Body GPU, Dependencies)
transformBody aliases (Body _ stms res) = do
  (stms', deps) <- transformStms aliases stms
  pure (Body () stms' res, deps)

transformStms ::
  AliasTable ->
  Stms (Aliases GPU) ->
  PassM (Stms GPU, Dependencies)
transformStms aliases stms = do
  let m = foldM_ handleStm aliases stms >> collapse
  grp <- statePrelude . snd <$> runStateT m initialState

  let stms' = groupStms grp
  let deps = groupDependencies grp \\ groupBindings grp

  pure (stms', deps)

transformExp ::
  AliasTable ->
  Exp (Aliases GPU) ->
  PassM (Exp GPU, Dependencies)
transformExp aliases e =
  case e of
    BasicOp {} -> pure (removeExpAliases e, depsOf e)
    Apply {} -> pure (removeExpAliases e, depsOf e)
    If c tbody fbody dec -> do
      -- TODO: Transform into a single GPUBody if both transformed branches
      --       only contain single GPUBody statements and all return values
      --       (1) originate from the branch GPUBody statements (not a
      --           dependency to its branch body).
      --       (2) originate from other GPUBody statements. (Only if a merge
      --           with that GPUBody is guaranteed or just if the asymptotical
      --           cost remains the same? For the prior: Inspect stateMemStored)
      --       (3) are scalar arrays of size 1, whatever their rank.
      (tbody', t_deps) <- transformBody aliases tbody
      (fbody', f_deps) <- transformBody aliases fbody
      let deps = depsOf c <> t_deps <> f_deps <> depsOf dec
      pure (If c tbody' fbody' dec, deps)
    DoLoop merge lform body -> do
      -- TODO: Transform into a single GPUBody if body only contains a single
      --       GPUBody statement and all return values
      --       (1) originate from that GPUBody statement (not a dependency to
      --           its body).
      --       (2) originate from other GPUBody statements. (Only if a merge
      --           with that GPUBody is guaranteed or just if the asymptotical
      --           cost remains the same? For the prior: Inspect stateMemStored)
      --       (3) are scalar arrays of size 1, whatever their rank.
      --
      -- What merge and lform aliases outside the loop is irrelevant as those
      -- cannot be consumed within the loop.
      (body', body_deps) <- transformBody aliases body
      let (params, args) = unzip merge
      let deps = body_deps <> depsOf params <> depsOf args <> depsOf lform

      let scope = scopeOf lform <> scopeOfFParams params
      let bound = IS.fromList $ map baseTag (M.keys scope)
      let deps' = deps \\ bound

      let dummy = DoLoop merge lform (Body (bodyDec body) SQ.empty [])
      let DoLoop merge' lform' _ = removeExpAliases dummy

      pure (DoLoop merge' lform' body', deps')
    WithAcc inputs lambda -> do
      accs <- mapM (transformWithAccInput aliases) inputs
      let (inputs', input_deps) = unzip accs
      -- Lambda parameters are all unique and thus have no aliases.
      (lambda', deps) <- transformLambda aliases lambda
      pure (WithAcc inputs' lambda', deps <> fold input_deps)
    Op {} ->
      -- A GPUBody cannot be nested within other HostOp constructs.
      pure (removeExpAliases e, depsOf e)

transformWithAccInput ::
  AliasTable ->
  WithAccInput (Aliases GPU) ->
  PassM (WithAccInput GPU, Dependencies)
transformWithAccInput aliases (shape, arrs, op) = do
  (op', deps) <- case op of
    Nothing -> pure (Nothing, mempty)
    Just (f, nes) -> do
      -- Lambda parameters have no aliases.
      (f', deps) <- transformLambda aliases f
      pure (Just (f', nes), deps <> depsOf nes)
  let deps' = deps <> depsOf shape <> depsOf arrs
  pure ((shape, arrs, op'), deps')

depsOf :: FreeIn a => a -> Dependencies
depsOf = namesToSet . freeIn

namesToSet :: Names -> IS.IntSet
namesToSet = IS.fromList . map baseTag . namesToList

handleStm :: AliasTable -> Stm (Aliases GPU) -> MergeM AliasTable
handleStm aliases (Let pat (StmAux cs attrs _) e) = do
  (e', deps) <- lift (transformExp aliases e)
  let pat' = removePatAliases pat
  let stm' = Let pat' (StmAux cs attrs ()) e'
  let pes' = patElems pat'

  let observed = namesToSet $ rootAliasesOf (fold $ expAliases e) aliases
  let consumed = namesToSet $ rootAliasesOf (consumedInExp e) aliases
  let usage =
        Usage
          { usageBindings = IS.fromList $ map (baseTag . patElemName) pes',
            usageDependencies = observed <> deps <> depsOf pat' <> depsOf cs
          }

  case e' of
    Op (GPUBody _ (Body _ _ res)) -> do
      move <- canMergeGPUBodies usage consumed
      unless move collapse
      moveToInterlude stm' usage
      mapM_ (uncurry stores) (zip pes' (map resSubExp res))
    _ -> do
      move <- canMoveToPrelude usage consumed
      if move
        then moveToPrelude stm' usage
        else moveToPostlude stm' usage

  pure $ foldl recordAliases aliases (patElems pat)
  where
    rootAliasesOf names atable =
      let look n = M.findWithDefault (oneName n) n atable
       in foldMap look (namesToList names)

    recordAliases atable pe
      | aliasesOf pe == mempty =
        atable
      | otherwise =
        let root_aliases = rootAliasesOf (aliasesOf pe) atable
         in M.insert (patElemName pe) root_aliases atable

canMergeGPUBodies :: Usage -> Consumption -> MergeM Bool
canMergeGPUBodies usage consumed = do
  st <- get
  let stored = stateMemStored st
  let bound = stateHostBound st
  let onDevice tag = IM.member tag bound || IM.member tag stored

  -- A dependency returned from a GPUBody can be ignored as that dependency
  -- still will be available after a potential merge, albeit under a different
  -- name.
  let deps = usageDependencies usage
  let usage' = usage {usageDependencies = IS.filter (not . onDevice) deps}

  canMoveBeforePostlude usage' consumed

canMoveBeforePostlude :: Usage -> Consumption -> MergeM Bool
canMoveBeforePostlude usage consumed = do
  postlude <- gets statePostlude
  let bound = groupBindings postlude
  let deps = groupDependencies postlude

  let used = usageDependencies usage

  pure (IS.disjoint bound used && IS.disjoint deps consumed)

canMoveToPrelude :: Usage -> Consumption -> MergeM Bool
canMoveToPrelude usage consumed = do
  canMove <- canMoveBeforePostlude usage consumed

  interlude <- gets stateInterlude
  let bound = groupBindings interlude
  let deps = groupDependencies interlude

  let used = usageDependencies usage

  pure (canMove && IS.disjoint bound used && IS.disjoint deps consumed)

moveToPrelude :: Stm GPU -> Usage -> MergeM ()
moveToPrelude stm usage =
  modify $ \st -> st {statePrelude = (stm, usage) `moveTo` statePrelude st}

moveToInterlude :: Stm GPU -> Usage -> MergeM ()
moveToInterlude stm usage =
  modify $ \st -> st {stateInterlude = (stm, usage) `moveTo` stateInterlude st}

moveToPostlude :: Stm GPU -> Usage -> MergeM ()
moveToPostlude stm usage = do
  recordResultAliases stm
  modify $ \st -> st {statePostlude = (stm, usage) `moveTo` statePostlude st}

moveTo :: (Stm GPU, Usage) -> Group -> Group
moveTo (stm, usage) grp =
  grp
    { groupStms = groupStms grp |> stm,
      groupUsage = groupUsage grp <> usage
    }

type Consumption = IS.IntSet

data Usage = Usage
  { usageBindings :: IS.IntSet,
    usageDependencies :: Dependencies
  }

instance Semigroup Usage where
  a <> b =
    Usage
      { usageBindings = usageBindings a <> usageBindings b,
        usageDependencies = usageDependencies a <> usageDependencies b
      }

instance Monoid Usage where
  mempty =
    Usage
      { usageBindings = mempty,
        usageDependencies = mempty
      }

data Group = Group
  { groupStms :: Stms GPU,
    groupUsage :: Usage
  }

groupBindings :: Group -> IS.IntSet
groupBindings = usageBindings . groupUsage

groupDependencies :: Group -> Dependencies
groupDependencies = usageDependencies . groupUsage

instance Semigroup Group where
  a <> b =
    Group
      { groupStms = groupStms a <> groupStms b,
        groupUsage = groupUsage a <> groupUsage b
      }

instance Monoid Group where
  mempty =
    Group
      { groupStms = mempty,
        groupUsage = mempty
      }

data State = State
  { statePrelude :: Group,
    stateInterlude :: Group,
    statePostlude :: Group,
    stateMemStored :: IM.IntMap (SubExp, Type),
    stateHostBound :: IM.IntMap SubExp
  }

initialState :: State
initialState =
  State
    { statePrelude = mempty,
      stateInterlude = mempty,
      statePostlude = mempty,
      stateMemStored = mempty,
      stateHostBound = mempty
    }

type MergeM = StateT State PassM

stores :: PatElem Type -> SubExp -> MergeM ()
stores (PatElem n t) se
  | isArray t =
    let row_t = fromJust (peelArray 1 t)
     in modify $ \st ->
          let stored = stateMemStored st
              stored' = IM.insert (baseTag n) (se, row_t) stored
           in st {stateMemStored = stored'}
stores pe se = pe `binds` se

binds :: PatElem Type -> SubExp -> MergeM ()
binds (PatElem n _) se =
  modify $ \st ->
    let bound = stateHostBound st
     in st {stateHostBound = IM.insert (baseTag n) se bound}

isArray :: ArrayShape shape => TypeBase shape u -> Bool
isArray t = arrayRank t > 0

-- | Record direct aliases of GPUBody results and their contents. Any GPUBody
-- can have its dependencies to those substituted if they merge with the source
-- GPUBody. Hence alias dependencies can be ignored when determing whether a
-- GPUBody can be moved across the postlude.
recordResultAliases :: Stm GPU -> MergeM ()
recordResultAliases stm = do
  stored <- gets stateMemStored
  bound <- gets stateHostBound
  case stm of
    Let (Pat [a]) _ (BasicOp (SubExp (Var n))) ->
      if isArray (patElemType a)
        then case IM.lookup (baseTag n) stored of
          Nothing -> pure ()
          Just (se, _) -> a `stores` se
        else case IM.lookup (baseTag n) bound of
          Nothing -> pure ()
          Just se -> a `binds` se
    Let (Pat [a]) _ (BasicOp (Index arr slice))
      | Just (se, t) <- IM.lookup (baseTag arr) stored,
        DimFix idx : dims <- unSlice slice,
        idx == intConst Int64 0,
        and $ zipWith (\sd ad -> sd == sliceDim ad) dims (arrayDims t) ->
        a `binds` se
    _ -> pure ()

collapse :: MergeM ()
collapse = do
  mergeInterlude
  modify $ \st ->
    st
      { statePrelude = statePrelude st <> stateInterlude st <> statePostlude st,
        stateInterlude = mempty,
        statePostlude = mempty,
        stateMemStored = mempty,
        stateHostBound = mempty
      }

mergeInterlude :: MergeM ()
mergeInterlude = do
  stms <- gets (groupStms . stateInterlude)

  stms' <-
    if SQ.length stms < 2
      then pure stms
      else SQ.singleton <$> foldrM merge empty stms

  modify $ \st -> st {stateInterlude = (stateInterlude st) {groupStms = stms'}}
  where
    empty = Let mempty (StmAux mempty mempty ()) noop
    noop = Op (GPUBody [] (Body () SQ.empty []))

    merge :: Stm GPU -> Stm GPU -> MergeM (Stm GPU)
    merge stm0 stm1
      | Let pat0 (StmAux cs0 attrs0 _) (Op (GPUBody types0 body)) <- stm0,
        Let pat1 (StmAux cs1 attrs1 _) (Op (GPUBody types1 body1)) <- stm1 =
        do
          Body _ stms0 res0 <- execRewrite (rewriteBody body)
          let Body _ stms1 res1 = body1

              pat' = pat0 <> pat1
              aux' = StmAux (cs0 <> cs1) (attrs0 <> attrs1) ()
              types' = types0 ++ types1
              body' = Body () (stms0 <> stms1) (res0 <> res1)
           in pure (Let pat' aux' (Op (GPUBody types' body')))
    merge _ _ =
      compilerBugS "mergeInterlude: cannot merge non-GPUBody statements"

type RewriteM = StateT (Stms GPU) MergeM

arrayContents :: RewriteM (IM.IntMap (SubExp, Type))
arrayContents = lift (gets stateMemStored)

returnedValues :: RewriteM (IM.IntMap SubExp)
returnedValues = lift (gets stateHostBound)

execRewrite :: RewriteM (Body GPU) -> MergeM (Body GPU)
execRewrite m = fst <$> runStateT m' SQ.empty
  where
    m' = do
      Body _ stms res <- m
      prelude <- get
      pure (Body () (prelude <> stms) res)

rewriteBody :: Body GPU -> RewriteM (Body GPU)
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

rewritePatElem :: PatElem Type -> RewriteM (PatElem Type)
rewritePatElem (PatElem n t) =
  PatElem n <$> rewriteType t

rewriteExp :: Exp GPU -> RewriteM (Exp GPU)
rewriteExp e = do
  stored <- arrayContents
  case e of
    BasicOp (Index arr slice)
      | Just (se, _) <- IM.lookup (baseTag arr) stored,
        [DimFix idx] <- unSlice slice,
        idx == intConst Int64 0 ->
        pure $ BasicOp (SubExp se)
    BasicOp (Index arr slice)
      | Just (Var src, _) <- IM.lookup (baseTag arr) stored,
        DimFix idx : dims <- unSlice slice,
        idx == intConst Int64 0 ->
        pure $ BasicOp $ Index src (Slice dims)
    _ -> mapExpM rewriter e
  where
    rewriter =
      Mapper
        { mapOnSubExp = rewriteSubExp,
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
  as_value <- returnedValues
  case IM.lookup (baseTag n) as_value of
    Just se -> pure se
    Nothing -> do
      as_array <- arrayContents
      case IM.lookup (baseTag n) as_array of
        Just (se, t) -> Var <$> asArray se t
        Nothing -> pure (Var n)

rewriteName :: VName -> RewriteM VName
rewriteName n = do
  se <- rewriteSubExp (Var n)
  case se of
    Var n' -> pure n'
    Constant c -> referConst c

asArray :: SubExp -> Type -> RewriteM VName
asArray se row_t = do
  name <- newName "arr"
  let t = row_t `arrayOfRow` intConst Int64 1

  let pat = Pat [PatElem name t]
  let aux = StmAux mempty mempty ()
  let e = BasicOp (ArrayLit [se] row_t)

  modify (|> Let pat aux e)
  pure name

referConst :: PrimValue -> RewriteM VName
referConst c = do
  name <- newName "cnst"
  let t = Prim (primValueType c)

  let pat = Pat [PatElem name t]
  let aux = StmAux mempty mempty ()
  let e = BasicOp (SubExp $ Constant c)

  modify (|> Let pat aux e)
  pure name

newName :: String -> RewriteM VName
newName s = lift $ lift (newNameFromString s)
