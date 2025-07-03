-- |
-- This module implements an optimization pass that merges 'GPUBody' kernels to
-- eliminate memory transactions and reduce the number of kernel launches.
-- This is useful because the "Futhark.Optimise.ReduceDeviceSyncs" pass introduces
-- 'GPUBody' kernels that only execute single statements.
--
-- To merge as many 'GPUBody' kernels as possible, this pass reorders statements
-- with the goal of bringing as many 'GPUBody' statements next to each other in
-- a sequence. Such sequence can then trivially be merged.
module Futhark.Optimise.MergeGPUBodies (mergeGPUBodies) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Bifunctor (first)
import Data.Foldable
import Data.IntMap qualified as IM
import Data.IntSet ((\\))
import Data.IntSet qualified as IS
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence ((|>))
import Data.Sequence qualified as SQ
import Futhark.Analysis.Alias
import Futhark.Construct (sliceDim)
import Futhark.Error
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.MonadFreshNames hiding (newName)
import Futhark.Pass

-- | An optimization pass that reorders and merges 'GPUBody' statements to
-- eliminate memory transactions and reduce the number of kernel launches.
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

--------------------------------------------------------------------------------
--                               COMMON - TYPES                               --
--------------------------------------------------------------------------------

-- | A set of 'VName' tags that denote all variables that some group of
-- statements depend upon. Those must be computed before the group statements.
type Dependencies = IS.IntSet

-- | A set of 'VName' tags that denote all variables that some group of
-- statements binds.
type Bindings = IS.IntSet

-- | A set of 'VName' tags that denote the root aliases of all arrays that some
-- statement consumes.
type Consumption = IS.IntSet

--------------------------------------------------------------------------------
--                              COMMON - HELPERS                              --
--------------------------------------------------------------------------------

-- | All free variables of a construct as 'Dependencies'.
depsOf :: (FreeIn a) => a -> Dependencies
depsOf = namesToSet . freeIn

-- | Convert 'Names' to an integer set of name tags.
namesToSet :: Names -> IS.IntSet
namesToSet = IS.fromList . map baseTag . namesToList

--------------------------------------------------------------------------------
--                            AD HOC OPTIMIZATION                             --
--------------------------------------------------------------------------------

-- | Optimize a lambda and determine its dependencies.
transformLambda ::
  AliasTable ->
  Lambda (Aliases GPU) ->
  PassM (Lambda GPU, Dependencies)
transformLambda aliases (Lambda params types body) = do
  (body', deps) <- transformBody aliases body
  pure (Lambda params types body', deps)

-- | Optimize a body and determine its dependencies.
transformBody ::
  AliasTable ->
  Body (Aliases GPU) ->
  PassM (Body GPU, Dependencies)
transformBody aliases (Body _ stms res) = do
  grp <- evalStateT (foldM_ reorderStm aliases stms >> collapse) initialState

  let stms' = groupStms grp
  let deps = (groupDependencies grp <> depsOf res) \\ groupBindings grp

  pure (Body () stms' res, deps)

-- | Optimize a sequence of statements and determine their dependencies.
transformStms ::
  AliasTable ->
  Stms (Aliases GPU) ->
  PassM (Stms GPU, Dependencies)
transformStms aliases stms = do
  (Body _ stms' _, deps) <- transformBody aliases (Body mempty stms [])
  pure (stms', deps)

-- | Optimizes and reorders a single statement within a sequence while tracking
-- the declaration, observation, and consumption of its dependencies.
-- This creates sequences of GPUBody statements that can be merged into single
-- kernels.
reorderStm :: AliasTable -> Stm (Aliases GPU) -> ReorderM AliasTable
reorderStm aliases (Let pat (StmAux cs attrs loc _) e) = do
  (e', deps) <- lift (transformExp aliases e)
  let pat' = removePatAliases pat
  let stm' = Let pat' (StmAux cs attrs loc ()) e'
  let pes' = patElems pat'

  -- Array aliases can be seen as a directed graph where vertices are arrays
  -- (or the names that bind them) and an edge x -> y denotes that x aliases y.
  -- The root aliases of some array A is then the set of arrays that can be
  -- reached from A in graph and which have no edges themselves.
  --
  -- All arrays that share a root alias are considered aliases of each other
  -- and will be consumed if either of them is consumed.
  -- When reordering statements we must ensure that no statement that consumes
  -- an array is moved before any statement that observes one of its aliases.
  --
  -- That is to move statement X before statement Y the set of root aliases of
  -- arrays consumed by X must not overlap with the root aliases of arrays
  -- observed by Y.
  --
  -- We consider the root aliases of Y's observed arrays as part of Y's
  -- dependencies and simply say that the root aliases of arrays consumed by X
  -- must not overlap those.
  --
  -- To move X before Y then the dependencies of X must also not overlap with
  -- the variables bound by Y.

  let observed = namesToSet $ rootAliasesOf (fold $ patAliases pat) aliases
  let consumed = namesToSet $ rootAliasesOf (consumedInExp e) aliases
  let usage =
        Usage
          { usageBindings = IS.fromList $ map (baseTag . patElemName) pes',
            usageDependencies = observed <> deps <> depsOf pat' <> depsOf cs
          }

  case e' of
    Op GPUBody {} ->
      moveGPUBody stm' usage consumed
    _ ->
      moveOther stm' usage consumed

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

-- | Optimize a single expression and determine its dependencies.
transformExp ::
  AliasTable ->
  Exp (Aliases GPU) ->
  PassM (Exp GPU, Dependencies)
transformExp aliases e =
  case e of
    BasicOp {} -> pure (removeExpAliases e, depsOf e)
    Apply {} -> pure (removeExpAliases e, depsOf e)
    Match ses cases defbody dec -> do
      let transformCase (Case vs body) =
            first (Case vs) <$> transformBody aliases body
      (cases', cases_deps) <- mapAndUnzipM transformCase cases
      (defbody', defbody_deps) <- transformBody aliases defbody
      let deps = depsOf ses <> mconcat cases_deps <> defbody_deps <> depsOf dec
      pure (Match ses cases' defbody' dec, deps)
    Loop merge lform body -> do
      -- What merge and lform aliases outside the loop is irrelevant as those
      -- cannot be consumed within the loop.
      (body', body_deps) <- transformBody aliases body
      let (params, args) = unzip merge
      let deps = body_deps <> depsOf params <> depsOf args <> depsOf lform

      let scope =
            scopeOfLoopForm lform <> scopeOfFParams params ::
              Scope (Aliases GPU)
      let bound = IS.fromList $ map baseTag (M.keys scope)
      let deps' = deps \\ bound

      let dummy =
            Loop merge lform (Body (bodyDec body) SQ.empty []) ::
              Exp (Aliases GPU)
      let Loop merge' lform' _ = removeExpAliases dummy

      pure (Loop merge' lform' body', deps')
    WithAcc inputs lambda -> do
      accs <- mapM (transformWithAccInput aliases) inputs
      let (inputs', input_deps) = unzip accs
      -- The lambda parameters are all unique and thus have no aliases.
      (lambda', deps) <- transformLambda aliases lambda
      pure (WithAcc inputs' lambda', deps <> fold input_deps)
    Op {} ->
      -- A GPUBody cannot be nested within other HostOp constructs.
      pure (removeExpAliases e, depsOf e)

-- | Optimize a single WithAcc input and determine its dependencies.
transformWithAccInput ::
  AliasTable ->
  WithAccInput (Aliases GPU) ->
  PassM (WithAccInput GPU, Dependencies)
transformWithAccInput aliases (shape, arrs, op) = do
  (op', deps) <- case op of
    Nothing -> pure (Nothing, mempty)
    Just (f, nes) -> do
      -- The lambda parameters have no aliases.
      (f', deps) <- transformLambda aliases f
      pure (Just (f', nes), deps <> depsOf nes)
  let deps' = deps <> depsOf shape <> depsOf arrs
  pure ((shape, arrs, op'), deps')

--------------------------------------------------------------------------------
--                             REORDERING - TYPES                             --
--------------------------------------------------------------------------------

-- | The monad used to reorder statements within a sequence such that its
-- GPUBody statements can be merged into as few possible kernels.
type ReorderM = StateT State PassM

-- | The state used by a 'ReorderM' monad.
data State = State
  { -- | All statements that already have been processed from the sequence,
    -- divided into alternating groups of non-GPUBody and GPUBody statements.
    -- Blocks at even indices only contain non-GPUBody statements. Blocks at
    -- odd indices only contain GPUBody statements.
    stateBlocks :: Blocks,
    stateEquivalents :: EquivalenceTable
  }

-- | A map from variable tags to t'SubExp's returned from within GPUBodies.
type EquivalenceTable = IM.IntMap Entry

-- | An entry in an 'EquivalenceTable'.
data Entry = Entry
  { -- | A value returned from within a GPUBody kernel.
    -- In @let res = gpu { x }@ this is @x@.
    entryValue :: SubExp,
    -- | The type of the 'entryValue'.
    entryType :: Type,
    -- | The name of the variable that binds the return value for 'entryValue'.
    -- In @let res = gpu { x }@ this is @res@.
    entryResult :: VName,
    -- | The index of the group that `entryResult` is bound in.
    entryBlockIdx :: Int,
    -- | If 'False' then the entry key is a variable that binds the same value
    -- as the 'entryValue'. Otherwise it binds an array with an outer dimension
    -- of one whose row equals that value.
    entryStored :: Bool
  }

type Blocks = SQ.Seq Group

-- | A group is a subsequence of statements, usually either only GPUBody
-- statements or only non-GPUBody statements. The 'Usage' statistics of those
-- statements are also stored.
data Group = Group
  { -- | The statements of the group.
    groupStms :: Stms GPU,
    -- | The usage statistics of the statements within the group.
    groupUsage :: Usage
  }

-- | Usage statistics for some set of statements.
data Usage = Usage
  { -- | The variables that the statements bind.
    usageBindings :: Bindings,
    -- | The variables that the statements depend upon, i.e. the free variables
    -- of each statement and the root aliases of every array that they observe.
    usageDependencies :: Dependencies
  }

instance Semigroup Group where
  (Group s1 u1) <> (Group s2 u2) = Group (s1 <> s2) (u1 <> u2)

instance Monoid Group where
  mempty = Group {groupStms = mempty, groupUsage = mempty}

instance Semigroup Usage where
  (Usage b1 d1) <> (Usage b2 d2) = Usage (b1 <> b2) (d1 <> d2)

instance Monoid Usage where
  mempty = Usage {usageBindings = mempty, usageDependencies = mempty}

--------------------------------------------------------------------------------
--                           REORDERING - FUNCTIONS                           --
--------------------------------------------------------------------------------

-- | Return the usage bindings of the group.
groupBindings :: Group -> Bindings
groupBindings = usageBindings . groupUsage

-- | Return the usage dependencies of the group.
groupDependencies :: Group -> Dependencies
groupDependencies = usageDependencies . groupUsage

-- | An initial state to use when running a 'ReorderM' monad.
initialState :: State
initialState =
  State
    { stateBlocks = SQ.singleton mempty,
      stateEquivalents = mempty
    }

-- | Modify the groups that the sequence has been split into so far.
modifyBlocks :: (Blocks -> Blocks) -> ReorderM ()
modifyBlocks f =
  modify $ \st -> st {stateBlocks = f (stateBlocks st)}

-- | Remove these keys from the equivalence table.
removeEquivalents :: IS.IntSet -> ReorderM ()
removeEquivalents keys =
  modify $ \st ->
    let eqs' = stateEquivalents st `IM.withoutKeys` keys
     in st {stateEquivalents = eqs'}

-- | Add an entry to the equivalence table.
recordEquivalent :: VName -> Entry -> ReorderM ()
recordEquivalent n entry =
  modify $ \st ->
    let eqs = stateEquivalents st
        eqs' = IM.insert (baseTag n) entry eqs
     in st {stateEquivalents = eqs'}

-- | Moves a GPUBody statement to the furthest possible group of the statement
-- sequence, possibly a new group at the end of sequence.
--
-- To simplify consumption handling a GPUBody is not allowed to merge with a
-- kernel whose result it consumes. Such GPUBody may therefore not be moved
-- into the same group as such kernel.
moveGPUBody :: Stm GPU -> Usage -> Consumption -> ReorderM ()
moveGPUBody stm usage consumed = do
  -- Replace dependencies with their GPUBody result equivalents.
  eqs <- gets stateEquivalents
  let g i = maybe i (baseTag . entryResult) (IM.lookup i eqs)
  let deps' = IS.map g (usageDependencies usage)
  let usage' = usage {usageDependencies = deps'}

  -- Move the GPUBody.
  grps <- gets stateBlocks
  let f = groupBlocks usage' consumed
  let idx = fromMaybe 1 (SQ.findIndexR f grps)
  let idx' = case idx `mod` 2 of
        0 -> idx + 1
        _ | consumes idx grps -> idx + 2
        _ -> idx
  modifyBlocks $ moveToGrp (stm, usage) idx'

  -- Record the kernel equivalents of the bound results.
  let pes = patElems (stmPat stm)
  let Op (GPUBody _ (Body _ _ res)) = stmExp stm
  mapM_ (stores idx') (zip pes (map resSubExp res))
  where
    consumes idx grps
      | Just grp <- SQ.lookup idx grps =
          not $ IS.disjoint (groupBindings grp) consumed
      | otherwise =
          False

    stores idx (PatElem n t, se)
      | Just row_t <- peelArray 1 t =
          recordEquivalent n $ Entry se row_t n idx True
      | otherwise =
          recordEquivalent n $ Entry se t n idx False

-- | Moves a non-GPUBody statement to the furthest possible groups of the
-- statement sequence, possibly a new group at the end of sequence.
moveOther :: Stm GPU -> Usage -> Consumption -> ReorderM ()
moveOther stm usage consumed = do
  grps <- gets stateBlocks
  let f = groupBlocks usage consumed
  let idx = fromMaybe 0 (SQ.findIndexR f grps)
  let idx' = ((idx + 1) `div` 2) * 2
  modifyBlocks $ moveToGrp (stm, usage) idx'
  recordEquivalentsOf stm idx'

-- | @recordEquivalentsOf stm idx@ records the GPUBody result and/or return
-- value that @stm@ is equivalent to. @idx@ is the index of the group that @stm@
-- belongs to.
--
-- A GPUBody can have a dependency substituted with a result equivalent if it
-- merges with the source GPUBody, allowing it to be moved beyond the binding
-- site of that dependency.
--
-- To guarantee that a GPUBody which moves beyond a dependency also merges with
-- its source GPUBody, equivalents are only allowed to be recorded for results
-- bound within the group at index @idx-1@.
recordEquivalentsOf :: Stm GPU -> Int -> ReorderM ()
recordEquivalentsOf stm idx = do
  eqs <- gets stateEquivalents
  case stm of
    Let (Pat [PatElem x _]) _ (BasicOp (SubExp (Var n)))
      | Just entry <- IM.lookup (baseTag n) eqs,
        entryBlockIdx entry == idx - 1 ->
          recordEquivalent x entry
    Let (Pat [PatElem x _]) _ (BasicOp (Index arr slice))
      | Just entry <- IM.lookup (baseTag arr) eqs,
        entryBlockIdx entry == idx - 1,
        Slice (DimFix i : dims) <- slice,
        i == intConst Int64 0,
        dims == map sliceDim (arrayDims $ entryType entry) ->
          recordEquivalent x (entry {entryStored = False})
    _ -> pure ()

-- | Does this group block a statement with this usage/consumption statistics
-- from being moved past it?
groupBlocks :: Usage -> Consumption -> Group -> Bool
groupBlocks usage consumed grp =
  let bound = groupBindings grp
      deps = groupDependencies grp

      used = usageDependencies usage
   in not (IS.disjoint bound used && IS.disjoint deps consumed)

-- | @moveToGrp stm idx grps@ moves @stm@ into the group at index @idx@ of
-- @grps@.
moveToGrp :: (Stm GPU, Usage) -> Int -> Blocks -> Blocks
moveToGrp stm idx grps
  | idx >= SQ.length grps =
      moveToGrp stm idx (grps |> mempty)
  | otherwise =
      SQ.adjust' (stm `moveTo`) idx grps

-- | Adds the statement and its usage statistics to the group.
moveTo :: (Stm GPU, Usage) -> Group -> Group
moveTo (stm, usage) grp =
  grp
    { groupStms = groupStms grp |> stm,
      groupUsage = groupUsage grp <> usage
    }

--------------------------------------------------------------------------------
--                         MERGING GPU BODIES - TYPES                         --
--------------------------------------------------------------------------------

-- | The monad used for rewriting a GPUBody to use the t'SubExp's that are
-- returned from kernels it is merged with rather than the results that they
-- bind.
--
-- The state is a prologue of statements to be added at the beginning of the
-- rewritten kernel body.
type RewriteM = StateT (Stms GPU) ReorderM

--------------------------------------------------------------------------------
--                       MERGING GPU BODIES - FUNCTIONS                       --
--------------------------------------------------------------------------------

-- | Collapses the processed sequence of groups into a single group and returns
-- it, merging GPUBody groups into single kernels in the process.
collapse :: ReorderM Group
collapse = do
  grps <- zip (cycle [False, True]) . toList <$> gets stateBlocks
  grp <- foldM clps mempty grps

  modify $ \st -> st {stateBlocks = SQ.singleton grp}
  pure grp
  where
    clps grp0 (gpu_bodies, Group stms usage) = do
      grp1 <-
        if gpu_bodies
          then Group <$> mergeKernels stms <*> pure usage
          else pure (Group stms usage)
      -- Remove equivalents that no longer are relevant for rewriting GPUBody
      -- kernels. This ensures that they are not substituted in later kernels
      -- where the replacement variables might not be in scope.
      removeEquivalents (groupBindings grp1)
      pure (grp0 <> grp1)

-- | Merges a sequence of GPUBody statements into a single kernel.
mergeKernels :: Stms GPU -> ReorderM (Stms GPU)
mergeKernels stms
  | SQ.length stms < 2 =
      pure stms
  | otherwise =
      SQ.singleton <$> foldrM merge empty stms
  where
    empty = Let mempty (defAux ()) noop
    noop = Op (GPUBody [] (Body () SQ.empty []))

    merge :: Stm GPU -> Stm GPU -> ReorderM (Stm GPU)
    merge stm0 stm1
      | Let pat0 (StmAux cs0 attrs0 _ _) (Op (GPUBody types0 body)) <- stm0,
        Let pat1 (StmAux cs1 attrs1 _ _) (Op (GPUBody types1 body1)) <- stm1 =
          do
            Body _ stms0 res0 <- execRewrite (rewriteBody body)
            let Body _ stms1 res1 = body1

                pat' = pat0 <> pat1
                aux' = StmAux (cs0 <> cs1) (attrs0 <> attrs1) mempty ()
                types' = types0 ++ types1
                body' = Body () (stms0 <> stms1) (res0 <> res1)
             in pure (Let pat' aux' (Op (GPUBody types' body')))
    merge _ _ =
      compilerBugS "mergeGPUBodies: cannot merge non-GPUBody statements"

-- | Perform a rewrite and finish it by adding the rewrite prologue to the start
-- of the body.
execRewrite :: RewriteM (Body GPU) -> ReorderM (Body GPU)
execRewrite m = evalStateT m' SQ.empty
  where
    m' = do
      Body _ stms res <- m
      prologue <- get
      pure (Body () (prologue <> stms) res)

-- | Return the equivalence table.
equivalents :: RewriteM EquivalenceTable
equivalents = lift (gets stateEquivalents)

rewriteBody :: Body GPU -> RewriteM (Body GPU)
rewriteBody (Body _ stms res) =
  Body () <$> rewriteStms stms <*> rewriteResult res

rewriteStms :: Stms GPU -> RewriteM (Stms GPU)
rewriteStms = mapM rewriteStm

rewriteStm :: Stm GPU -> RewriteM (Stm GPU)
rewriteStm (Let (Pat pes) (StmAux cs attrs loc _) e) = do
  pat' <- Pat <$> mapM rewritePatElem pes
  cs' <- rewriteCerts cs
  e' <- rewriteExp e
  pure $ Let pat' (StmAux cs' attrs loc ()) e'

rewritePatElem :: PatElem Type -> RewriteM (PatElem Type)
rewritePatElem (PatElem n t) =
  PatElem n <$> rewriteType t

rewriteExp :: Exp GPU -> RewriteM (Exp GPU)
rewriteExp e = do
  eqs <- equivalents
  case e of
    BasicOp (Index arr slice)
      | Just entry <- IM.lookup (baseTag arr) eqs,
        DimFix idx : dims <- unSlice slice,
        idx == intConst Int64 0 ->
          let se = entryValue entry
           in pure . BasicOp $ case (dims, se) of
                ([], _) -> SubExp se
                (_, Var src) -> Index src (Slice dims)
                _ -> compilerBugS "rewriteExp: bad equivalence entry"
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
  eqs <- equivalents
  case IM.lookup (baseTag n) eqs of
    Nothing -> pure (Var n)
    Just (Entry se _ _ _ False) -> pure se
    Just (Entry se t _ _ True) -> Var <$> asArray se t

rewriteName :: VName -> RewriteM VName
rewriteName n = do
  se <- rewriteSubExp (Var n)
  case se of
    Var n' -> pure n'
    Constant c -> referConst c

-- | @asArray se t@ adds @let x = [se]@ to the rewrite prologue and returns the
-- name of @x@. @t@ is the type of @se@.
asArray :: SubExp -> Type -> RewriteM VName
asArray se row_t = do
  name <- newName "arr"
  let t = row_t `arrayOfRow` intConst Int64 1

  let pat = Pat [PatElem name t]
  let e = BasicOp (ArrayLit [se] row_t)

  modify (|> Let pat (defAux ()) e)
  pure name

-- | @referConst c@ adds @let x = c@ to the rewrite prologue and returns the
-- name of @x@.
referConst :: PrimValue -> RewriteM VName
referConst c = do
  name <- newName "cnst"
  let t = Prim (primValueType c)

  let pat = Pat [PatElem name t]
  let e = BasicOp (SubExp $ Constant c)

  modify (|> Let pat (defAux ()) e)
  pure name

-- | Produce a fresh name, using the given string as a template.
newName :: String -> RewriteM VName
newName s = lift $ lift (newNameFromString s)
