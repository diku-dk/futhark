-- | This module implements a compiler pass for inlining functions,
-- then removing those that have become dead.
module Futhark.Optimise.InliningDeadFun
  ( inlineAggressively,
    inlineConservatively,
    removeDeadFunctions,
  )
where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Parallel.Strategies
import Data.Functor (($>))
import Data.List (partition)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Analysis.CallGraph
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Builder
import Futhark.IR.SOACS
import Futhark.IR.SOACS.Simplify
  ( simpleSOACS,
    simplifyConsts,
    simplifyFun,
  )
import Futhark.Optimise.CSE
import Futhark.Optimise.Simplify.Rep (addScopeWisdom, informStms)
import Futhark.Pass
import Futhark.Transform.CopyPropagate
  ( copyPropagateInFun,
    copyPropagateInProg,
  )
import Futhark.Transform.Rename

parMapM :: (MonadFreshNames m) => (a -> State VNameSource b) -> [a] -> m [b]
-- The special-casing of [] is quite important here!  If 'as' is
-- empty, then we might otherwise create an empty name source below,
-- which can wreak all kinds of havoc.
parMapM _ [] = pure []
parMapM f as =
  modifyNameSource $ \src ->
    let f' a = runState (f a) src
        (bs, srcs) = unzip $ parMap rpar f' as
     in (bs, mconcat srcs)

-- It is more efficient to shrink the program as soon as possible,
-- rather than wait until it has balooned after full inlining.  This
-- is the inverse rate at which we perform full simplification after
-- inlining.  For the other steps we just do copy propagation.  The
-- simplification rates used have been determined heuristically and
-- are probably not optimal for any given program.
inlineFunctions ::
  (MonadFreshNames m) =>
  Int ->
  CallGraph ->
  S.Set Name ->
  Prog SOACS ->
  m (Prog SOACS)
inlineFunctions simplify_rate cg what_should_be_inlined prog = do
  let consts = progConsts prog
      funs = progFuns prog
      vtable = ST.fromScope (addScopeWisdom (scopeOf consts))

  (consts', funs') <- recurse (1, vtable) (consts, funs) what_should_be_inlined
  pure $ prog {progConsts = consts', progFuns = funs'}
  where
    fdmap fds = M.fromList $ zip (map funDefName fds) fds

    noCallsTo which from = S.null $ allCalledBy from cg `S.intersection` which

    recurse (i, vtable) (consts, funs) to_inline = do
      let (to_inline_now, to_inline_later) =
            S.partition (noCallsTo to_inline) to_inline
          (dont_inline_in, to_inline_in) =
            partition (noCallsTo to_inline_now . funDefName) funs

      if null to_inline_now
        then pure (consts, funs)
        else do
          let inlinemap =
                fdmap $ filter ((`S.member` to_inline_now) . funDefName) dont_inline_in
          (vtable', consts') <-
            if any (`calledByConsts` cg) to_inline_now
              then do
                consts' <-
                  simplifyConsts . performCSEOnStms
                    =<< inlineInStms inlinemap consts
                pure (ST.insertStms (informStms consts') mempty, consts')
              else pure (vtable, consts)

          let simplifyFun' fd
                | i `rem` simplify_rate == 0 =
                    copyPropagateInFun simpleSOACS vtable'
                      . performCSEOnFunDef True
                      =<< simplifyFun vtable' fd
                | otherwise =
                    copyPropagateInFun simpleSOACS vtable' fd

              onFun = simplifyFun' <=< inlineInFunDef inlinemap

          to_inline_in' <- parMapM onFun to_inline_in

          recurse
            (i + 1, vtable')
            (consts', dont_inline_in <> to_inline_in')
            to_inline_later

calledOnce :: CallGraph -> S.Set Name
calledOnce =
  S.fromList . map fst . filter ((== 1) . snd) . M.toList . numOccurences

inlineBecauseTiny :: Prog SOACS -> S.Set Name
inlineBecauseTiny = foldMap onFunDef . progFuns
  where
    onFunDef fd
      | (length (bodyStms (funDefBody fd)) <= k)
          || ("inline" `inAttrs` funDefAttrs fd) =
          S.singleton (funDefName fd)
      | otherwise = mempty
      where
        k = length (funDefRetType fd) + length (funDefParams fd)

progStms :: Prog SOACS -> Stms SOACS
progStms prog =
  progConsts prog <> foldMap (bodyStms . funDefBody) (progFuns prog)

data Used = InSOAC | InAD deriving (Eq, Ord, Show)

directlyCalledInSOACs :: Prog SOACS -> M.Map Name Used
directlyCalledInSOACs = flip execState mempty . mapM_ (onStm Nothing) . progStms
  where
    onBody :: Maybe Used -> Body SOACS -> State (M.Map Name Used) ()
    onBody u = mapM_ (onStm u) . bodyStms
    onStm u stm = onExp u (stmExp stm) $> stm
    onExp (Just u) (Apply fname _ _ _) = modify $ M.insertWith max fname u
    onExp Nothing Apply {} = pure ()
    onExp u e = walkExpM (walker u) e
    onSOAC u soac = void $ traverseSOACStms (const (traverse (onStm u'))) soac
      where
        u' = max u $ Just $ usage soac
    usage JVP {} = InAD
    usage VJP {} = InAD
    usage _ = InSOAC
    walker u =
      (identityWalker :: Walker SOACS (State (M.Map Name Used)))
        { walkOnBody = const (onBody u),
          walkOnOp = onSOAC u
        }

-- Expand set of function names with all reachable functions.
withTransitiveCalls :: CallGraph -> M.Map Name Used -> M.Map Name Used
withTransitiveCalls cg fs
  | fs == fs' = fs
  | otherwise = withTransitiveCalls cg fs'
  where
    look :: (Name, Used) -> M.Map Name Used
    look (f, u) = M.fromList $ map (,u) (S.toList (allCalledBy f cg))
    fs' = foldr (M.unionWith max . look) fs $ M.toList fs

calledInSOACs :: CallGraph -> Prog SOACS -> M.Map Name Used
calledInSOACs cg prog = withTransitiveCalls cg $ directlyCalledInSOACs prog

-- Inline those functions that are used in SOACs, and which involve
-- arrays of any kind, as well as any functions used in AD.
inlineBecauseSOACs :: CallGraph -> Prog SOACS -> S.Set Name
inlineBecauseSOACs cg prog =
  S.fromList $ mapMaybe onFunDef (progFuns prog)
  where
    called = calledInSOACs cg prog
    isArray = not . primType
    inline _ InAD = True
    inline fd InSOAC =
      any (isArray . paramType) (funDefParams fd)
        || any (isArray . fst) (funDefRetType fd)
        || length (funDefRetType fd) /= 1 -- FIXME: webgpu hack
        || arrayInBody (funDefBody fd)
    onFunDef fd = do
      guard $ maybe False (inline fd) $ M.lookup (funDefName fd) called
      Just $ funDefName fd
    arrayInBody = any arrayInStm . bodyStms
    arrayInStm stm =
      any isArray (patTypes (stmPat stm)) || arrayInExp (stmExp stm)
    arrayInExp (Match _ cases defbody _) =
      any arrayInBody $ defbody : map caseBody cases
    arrayInExp (Loop _ _ body) =
      arrayInBody body
    arrayInExp _ = False

-- Conservative inlining of functions that are called just once, or
-- have #[inline] on them.
consInlineFunctions :: (MonadFreshNames m) => Prog SOACS -> m (Prog SOACS)
consInlineFunctions prog =
  inlineFunctions 4 cg (calledOnce cg <> inlineBecauseTiny prog) prog
  where
    cg = buildCallGraph prog

-- Inline aggressively; in particular most things called from a SOAC.
aggInlineFunctions :: (MonadFreshNames m) => Prog SOACS -> m (Prog SOACS)
aggInlineFunctions prog =
  inlineFunctions 3 cg (inlineBecauseTiny prog <> inlineBecauseSOACs cg prog) prog
  where
    cg = buildCallGraph prog

-- | @inlineInFunDef constf fdmap caller@ inlines in @calleer@ the
-- functions in @fdmap@ that are called as @constf@. At this point the
-- preconditions are that if @fdmap@ is not empty, and, more
-- importantly, the functions in @fdmap@ do not call any other
-- functions.
inlineInFunDef ::
  (MonadFreshNames m) =>
  M.Map Name (FunDef SOACS) ->
  FunDef SOACS ->
  m (FunDef SOACS)
inlineInFunDef fdmap (FunDef entry attrs name rtp args body) =
  FunDef entry attrs name rtp args <$> inlineInBody fdmap body

inlineFunction ::
  (MonadFreshNames m) =>
  Pat Type ->
  StmAux dec ->
  [(SubExp, Diet)] ->
  (Safety, SrcLoc, [SrcLoc]) ->
  FunDef SOACS ->
  m (Stms SOACS)
inlineFunction pat aux args (safety, loc, locs) fun = do
  Body _ stms res <-
    renameBody $ mkBody (param_stms <> body_stms) (bodyResult (funDefBody fun))
  pure $ stms <> stmsFromList (zipWith bindSubExpRes (patIdents pat) res)
  where
    param_stms =
      stmsFromList $
        certify (stmAuxCerts aux)
          <$> zipWith bindSubExp (map paramIdent $ funDefParams fun) (map fst args)

    body_stms =
      addLocations (stmAuxAttrs aux) safety (filter notmempty (loc : locs)) $
        bodyStms $
          funDefBody fun

    -- Note that the sizes of arrays may not be correct at this
    -- point - it is crucial that we run copy propagation before
    -- the type checker sees this!
    bindSubExp ident se =
      mkLet [ident] $ BasicOp $ SubExp se

    bindSubExpRes ident (SubExpRes cs se) =
      certify cs $ bindSubExp ident se

    notmempty = (/= mempty) . locOf

inlineInStms ::
  (MonadFreshNames m) =>
  M.Map Name (FunDef SOACS) ->
  Stms SOACS ->
  m (Stms SOACS)
inlineInStms fdmap stms =
  bodyStms <$> inlineInBody fdmap (mkBody stms [])

inlineInBody ::
  (MonadFreshNames m) =>
  M.Map Name (FunDef SOACS) ->
  Body SOACS ->
  m (Body SOACS)
inlineInBody fdmap = onBody
  where
    inline (Let pat aux (Apply fname args _ what) : rest)
      | Just fd <- M.lookup fname fdmap,
        not $ "noinline" `inAttrs` funDefAttrs fd,
        not $ "noinline" `inAttrs` stmAuxAttrs aux =
          (<>) <$> inlineFunction pat aux args what fd <*> inline rest
    inline (stm@(Let _ _ BasicOp {}) : rest) =
      (oneStm stm <>) <$> inline rest
    inline (stm : rest) =
      (<>) <$> (oneStm <$> onStm stm) <*> inline rest
    inline [] =
      pure mempty

    onBody (Body dec stms res) =
      Body dec <$> inline (stmsToList stms) <*> pure res
    onStm (Let pat aux e) = Let pat aux <$> mapExpM inliner e

    inliner =
      (identityMapper @SOACS)
        { mapOnBody = const onBody,
          mapOnOp = onSOAC
        }

    onSOAC =
      mapSOACM identitySOACMapper {mapOnSOACLambda = onLambda}

    onLambda (Lambda params ret body) =
      Lambda params ret <$> onBody body

-- Propagate source locations and attributes to the inlined
-- statements.  Attributes are propagated only when applicable (this
-- probably means that every supported attribute needs to be handled
-- specially here).
addLocations :: Attrs -> Safety -> [SrcLoc] -> Stms SOACS -> Stms SOACS
addLocations attrs caller_safety more_locs = fmap onStm
  where
    onStm (Let pat aux (Apply fname args t (safety, loc, locs))) =
      Let pat aux' $
        Apply fname args t (min caller_safety safety, loc, locs ++ more_locs)
      where
        aux' = aux {stmAuxAttrs = attrs <> stmAuxAttrs aux}
    onStm (Let pat aux (BasicOp (Assert cond desc (loc, locs)))) =
      Let pat (withAttrs (attrsForAssert attrs) aux) $
        case caller_safety of
          Safe -> BasicOp $ Assert cond desc (loc, locs ++ more_locs)
          Unsafe -> BasicOp $ SubExp $ Constant UnitValue
    onStm (Let pat aux (Op soac)) =
      Let pat (withAttrs attrs' aux) $
        Op $
          runIdentity $
            mapSOACM
              identitySOACMapper
                { mapOnSOACLambda = pure . onLambda
                }
              soac
      where
        attrs' = attrs `withoutAttrs` for_assert
        for_assert = attrsForAssert attrs
        onLambda lam =
          lam {lambdaBody = onBody for_assert $ lambdaBody lam}
    onStm (Let pat aux e) =
      Let pat aux $ onExp e

    onExp =
      mapExp
        identityMapper
          { mapOnBody = const $ pure . onBody attrs
          }

    withAttrs attrs' aux = aux {stmAuxAttrs = attrs' <> stmAuxAttrs aux}

    onBody attrs' body =
      body
        { bodyStms =
            addLocations attrs' caller_safety more_locs $
              bodyStms body
        }

-- | Remove functions not ultimately called from an entry point or a
-- constant.
removeDeadFunctionsF :: Prog SOACS -> Prog SOACS
removeDeadFunctionsF prog =
  let cg = buildCallGraph prog
      live_funs = filter ((`isFunInCallGraph` cg) . funDefName) $ progFuns prog
   in prog {progFuns = live_funs}

-- | Inline all functions and remove the resulting dead functions.
inlineAggressively :: Pass SOACS SOACS
inlineAggressively =
  Pass
    { passName = "Inline aggressively",
      passDescription = "Aggressively inline and remove resulting dead functions.",
      passFunction =
        copyPropagateInProg simpleSOACS . removeDeadFunctionsF <=< aggInlineFunctions
    }

-- | Inline some functions and remove the resulting dead functions.
inlineConservatively :: Pass SOACS SOACS
inlineConservatively =
  Pass
    { passName = "Inline conservatively",
      passDescription = "Conservatively inline and remove resulting dead functions.",
      passFunction =
        copyPropagateInProg simpleSOACS . removeDeadFunctionsF <=< consInlineFunctions
    }

-- | @removeDeadFunctions prog@ removes the functions that are unreachable from
-- the main function from the program.
removeDeadFunctions :: Pass SOACS SOACS
removeDeadFunctions =
  Pass
    { passName = "Remove dead functions",
      passDescription = "Remove the functions that are unreachable from entry points",
      passFunction = pure . removeDeadFunctionsF
    }
