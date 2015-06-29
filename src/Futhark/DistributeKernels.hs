{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
-- | Distribute kernels.
-- In the following, I will use the term "width" to denote the amount
-- of immediate parallelism in a map - that is, the row size of the
-- array(s) being used as input.
--
-- = Basic Idea
--
-- If we have:
--
-- @
--   map
--     map(f)
--     bnds_a...
--     map(g)
-- @
--
-- Then we want to distribute to:
--
-- @
--   map
--     map(f)
--   map
--     bnds_a
--   map
--     map(g)
-- @
--
-- But for now only if
--
--  (0) it can be done without creating irregular arrays.
--      Specifically, the size of the arrays created by @map(f)@, by
--      @map(g)@ and whatever is created by @bnds_a@ that is also used
--      in @map(g)@, must be invariant to the outermost loop.
--
--  (1) the maps are _balanced_.  That is, the functions @f@ and @g@
--      must do the same amount of work for every iteration.
--
-- The advantage is that the map-nests containing @map(f)@ and
-- @map(g)@ can now be trivially flattened at no cost, thus exposing
-- more parallelism.  Note that the @bnds_a@ map constitutes array
-- expansion, which requires additional storage.
--
-- = Distributing Sequential Loops
--
-- As a starting point, sequential loops are treated like scalar
-- expressions.  That is, not distributed.  However, sometimes it can
-- be worthwhile to distribute if they contain a map:
--
-- @
--   map
--     loop
--       map
--     map
-- @
--
-- If we distribute the loop and interchange the outer map into the
-- loop, we get this:
--
-- @
--   loop
--     map
--       map
--   map
--     map
-- @
--
-- Now more parallelism may be available.
--
-- = Unbalanced Maps
--
-- Unbalanced maps will as a rule be sequentialised, but sometimes,
-- there is another way.  Assume we find this:
--
-- @
--   map
--     map(f)
--       map(g)
--     map
-- @
--
-- Presume that @map(f)@ is unbalanced.  By the simple rule above, we
-- would then fully sequentialise it, resulting in this:
--
-- @
--   map
--     loop
--   map
--     map
-- @
--
-- == Balancing by Loop Interchange
--
-- This is not ideal, as we cannot flatten the @map-loop@ nest, and we
-- are thus limited in the amount of parallelism available.
--
-- But assume now that the width of @map(g)@ is invariant to the outer
-- loop.  Then if possible, we can interchange @map(f)@ and @map(g)@,
-- sequentialise @map(f)@ and distribute, interchanging the outer
-- parallel loop into the sequential loop:
--
-- @
--   loop(f)
--     map
--       map(g)
--   map
--     map
-- @
--
-- After flattening the two nests we can obtain more parallelism.
--
-- When distributing a map, we also need to distribute everything that
-- the map depends on - possibly as its own map.  When distributing a
-- set of scalar bindings, we will need to know which of the binding
-- results are used afterwards.  Hence, we will need to compute usage
-- information.
--
-- = Redomap
--
-- Redomap is handled much like map.  Distributed loops are
-- distributed as maps, with the parameters corresponding to the
-- neutral elements added to their bodies.  The remaining loop will
-- remain a redomap.  Example:
--
-- @
-- redomap(op,
--         fn (acc,v) =>
--           map(f)
--           map(g),
--         e,a)
-- @
--
-- distributes to
--
-- @
-- let b = map(fn v =>
--               let acc = e
--               map(f),
--               a)
-- redomap(op,
--         fn (acc,v,dist) =>
--           map(g),
--         e,a,b)
-- @
--
module Futhark.DistributeKernels
       (transformProg)
       where

import Control.Applicative
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Data.List
import Debug.Trace

import Futhark.Optimise.Simplifier.Simple (bindableSimpleOps)
import Futhark.Representation.Basic
import Futhark.MonadFreshNames
import Futhark.Tools
import qualified Futhark.FirstOrderTransform as FOT
import Futhark.Renamer
import Futhark.Util
import Futhark.CopyPropagate

import Prelude

transformProg :: Prog -> Prog
transformProg = intraproceduralTransformation transformFunDec

transformFunDec :: MonadFreshNames m => FunDec -> m FunDec
transformFunDec fundec = runDistribM $ do
  body' <- localTypeEnv (typeEnvFromParams $ funDecParams fundec) $
           transformBody $ funDecBody fundec
  return fundec { funDecBody = body' }

type DistribM = ReaderT TypeEnv (State VNameSource)

runDistribM :: MonadFreshNames m => DistribM a -> m a
runDistribM m = modifyNameSource $ runState (runReaderT m HM.empty)

transformBody :: Body -> DistribM Body
transformBody body = transformBindings (bodyBindings body) $
                     return $ resultBody $ bodyResult body

transformBindings :: [Binding] -> DistribM Body -> DistribM Body
transformBindings [] m =
  m
transformBindings (bnd:bnds) m = do
  bnd' <- transformBinding bnd
  localTypeEnv (typeEnvFromBindings bnd') $
    insertBindings bnd' <$> transformBindings bnds m

transformBinding :: Binding -> DistribM [Binding]
transformBinding (Let pat () (If c tb fb rt)) = do
  tb' <- transformBody tb
  fb' <- transformBody fb
  return [Let pat () $ If c tb' fb' rt]
transformBinding (Let pat () (LoopOp (DoLoop res mergepat form body))) =
  localTypeEnv (boundInForm form $ typeEnvFromParams mergeparams) $ do
    body' <- transformBody body
    return [Let pat () $ LoopOp $ DoLoop res mergepat form body']
  where boundInForm (ForLoop i _) = HM.insert i (Basic Int)
        boundInForm (WhileLoop _) = id
        mergeparams = map fst mergepat
transformBinding (Let pat () (LoopOp (Map cs w lam arrs))) =
  distributeMap pat $ MapLoop cs w lam arrs
transformBinding (Let pat () (LoopOp (Redomap cs w lam1 lam2 nes arrs))) = do
  (mapbnd, redbnd) <- redomapToMapAndReduce pat () (cs, w, lam1, lam2, nes, arrs)
  mapbnd' <- transformBinding mapbnd
  localTypeEnv (typeEnvFromBindings mapbnd') $ do
    redbnd' <- transformBinding redbnd
    return $ mapbnd' ++ redbnd'
transformBinding bnd = return [bnd]

data MapLoop = MapLoop Certificates SubExp Lambda [VName]

mapLoopExp :: MapLoop -> Exp
mapLoopExp (MapLoop cs w lam arrs) = LoopOp $ Map cs w lam arrs

type Target = (Pattern, Result)

-- ^ First pair element is the very innermost ("current") target.  In
-- the list, the outermost target comes first.
type Targets = (Target, [Target])

singleTarget :: Target -> Targets
singleTarget = (,[])

innerTarget :: Targets -> Target
innerTarget = fst

outerTarget :: Targets -> Target
outerTarget (inner_target, []) = inner_target
outerTarget (_, outer_target : _) = outer_target

pushOuterTarget :: Target -> Targets -> Targets
pushOuterTarget target (inner_target, targets) =
  (inner_target, target : targets)

pushInnerTarget :: Target -> Targets -> Targets
pushInnerTarget target (inner_target, targets) =
  (target, targets ++ [inner_target])

data Nesting = MapNesting Names Pattern Certificates SubExp [(LParam, VName)]
             deriving (Show)

letBoundInNesting :: Nesting -> Names
letBoundInNesting (MapNesting names _ _ _ _) = names

letBindInNesting :: Names -> Nesting -> Nesting
letBindInNesting newnames (MapNesting oldnames pat cs w params_and_arrs) =
  MapNesting (oldnames <> newnames) pat cs w params_and_arrs

-- ^ First pair element is the very innermost ("current") nest.  In
-- the list, the outermost nest comes first.
type Nestings = (Nesting, [Nesting])

singleNesting :: Nesting -> Nestings
singleNesting = (,[])

pushInnerNesting :: Nesting -> Nestings -> Nestings
pushInnerNesting nesting (inner_nesting, nestings) =
  (nesting, nestings ++ [inner_nesting])

nestingWidth :: Nesting -> SubExp
nestingWidth (MapNesting _ _ _ w _) = w

nestingParams :: Nesting -> [LParam]
nestingParams (MapNesting _ _ _ _ params_and_arrs) =
  map fst params_and_arrs

-- | Both parameters and let-bound.
boundInNesting :: Nesting -> Names
boundInNesting nesting =
  HS.fromList (map paramName (nestingParams nesting)) <>
  letBoundInNesting nesting

letBindInInnerNesting :: Names -> Nestings -> Nestings
letBindInInnerNesting names (nest, nestings) =
  (letBindInNesting names nest, nestings)

data KernelEnv = KernelEnv { kernelNest :: Nestings
                           , kernelTypeEnv :: TypeEnv
                           }

data KernelAcc = KernelAcc { kernelTargets :: Targets
                           , kernelBindings :: [Binding]
                           , kernelRequires :: Names
                           }

addBindingToKernel :: Binding -> KernelAcc -> KernelAcc
addBindingToKernel bnd acc =
  acc { kernelBindings = bnd : kernelBindings acc }

type PostKernels = [Binding]

newtype KernelM a = KernelM (RWS KernelEnv PostKernels VNameSource a)
  deriving (Functor, Applicative, Monad,
            MonadReader KernelEnv,
            MonadWriter PostKernels,
            MonadFreshNames)

instance HasTypeEnv KernelM where
  askTypeEnv = asks kernelTypeEnv

runKernelM :: (HasTypeEnv m, MonadFreshNames m) =>
              KernelEnv -> KernelM a -> m (a, [Binding])
runKernelM env (KernelM m) = modifyNameSource $ getKernels . runRWS m env
  where getKernels (x,s,a) = ((x, a), s)

distributeMap :: (HasTypeEnv m, MonadFreshNames m) =>
                 Pattern -> MapLoop -> m [Binding]
distributeMap pat (MapLoop cs w lam arrs) = do
  types <- askTypeEnv
  let env = KernelEnv { kernelNest =
                        singleNesting (MapNesting mempty pat cs w $
                                       zip (lambdaParams lam) arrs)
                      , kernelTypeEnv =
                        types <> typeEnvFromParams (lambdaParams lam)
                      }
  liftM (reverse . snd) $ runKernelM env $
    distribute =<< distributeMapBodyBindings acc (bodyBindings $ lambdaBody lam)
    where acc = KernelAcc { kernelTargets = singleTarget (pat, bodyResult $ lambdaBody lam)
                          , kernelRequires = mempty
                          , kernelBindings = mempty
                          }

withBinding :: Binding -> KernelM a -> KernelM a
withBinding bnd = local $ \env ->
  env { kernelTypeEnv =
          kernelTypeEnv env <> typeEnvFromBindings [bnd]
      , kernelNest =
        letBindInInnerNesting provided $
        kernelNest env
      }
  where provided = HS.fromList $ patternNames $ bindingPattern bnd

mapNesting :: Pattern -> Certificates -> SubExp -> Lambda -> [VName]
           -> KernelM a
           -> KernelM a
mapNesting pat cs w lam arrs = local $ \env ->
  env { kernelNest = pushInnerNesting nest $ kernelNest env
      , kernelTypeEnv = kernelTypeEnv env <>
                        typeEnvFromParams (lambdaParams lam)
      }
  where nest = MapNesting mempty pat cs w (zip (lambdaParams lam) arrs)

newKernelNames :: Names -> Body -> Names
newKernelNames let_bound inner_body =
  freeInBody inner_body `HS.intersection` let_bound

ppTargets :: Targets -> String
ppTargets (target, targets) =
  unlines $ map ppTarget $ targets ++ [target]
  where ppTarget (pat, res) =
          pretty pat ++ " <- " ++ pretty res

ppNestings :: Nestings -> String
ppNestings (nesting, nestings) =
  unlines $ map ppNesting $ nestings ++ [nesting]
  where ppNesting (MapNesting _ _ _ _ params_and_arrs) =
          pretty (map fst params_and_arrs) ++
          " <- " ++
          pretty (map snd params_and_arrs)

distributeIfPossible :: KernelAcc -> KernelM (Maybe KernelAcc)
distributeIfPossible acc = do
  env <- ask
  let bnds = kernelBindings acc
      res = snd $ innerTarget $ kernelTargets acc
  if null bnds -- No point in distributing an empty kernel.
    then return $ Just acc
    else createKernelNest
         (kernelNest env) (kernelTargets acc) (mkBody bnds res) >>=
         \case
           Just (distributed, targets) -> do
             distributed' <- optimiseKernel <$> renameBinding distributed
             trace ("distributing\n" ++
                    pretty (mkBody bnds res) ++
                    "\nas\n" ++ pretty distributed ++
                    "\ndue to targets\n" ++ ppTargets (kernelTargets acc) ++
                    "\nand with new targets\n" ++ ppTargets targets) tell [distributed']
             return $ Just KernelAcc { kernelBindings = []
                                     , kernelRequires = mempty
                                     , kernelTargets = targets
                                     }
           Nothing ->
             return Nothing


distribute :: KernelAcc -> KernelM KernelAcc
distribute acc = fromMaybe acc <$> distributeIfPossible acc

optimiseKernel :: Binding -> Binding
optimiseKernel bnd = fromMaybe bnd $ tryOptimiseKernel bnd

tryOptimiseKernel :: Binding -> Maybe Binding
tryOptimiseKernel bnd = kernelIsRearrange bnd <|>
                        kernelIsReshape bnd <|>
                        kernelBodyOptimisable bnd

singleBindingBody :: Body -> Maybe Binding
singleBindingBody (Body _ [bnd] [res])
  | [res] == map Var (patternNames $ bindingPattern bnd) =
      Just bnd
singleBindingBody _ = Nothing

singleExpBody :: Body -> Maybe Exp
singleExpBody = liftM bindingExp . singleBindingBody

kernelIsRearrange :: Binding -> Maybe Binding
kernelIsRearrange (Let outer_pat _
                   (LoopOp (Map outer_cs _ outer_fun [outer_arr]))) =
  delve 1 outer_cs outer_fun
  where delve n cs (Lambda [param] body _)
          | Just (PrimOp (Rearrange inner_cs perm arr)) <-
              singleExpBody body,
            paramName param == arr =
              let cs' = cs ++ inner_cs
                  perm' = [0..n-1] ++ map (n+) perm
              in Just $ Let outer_pat () $
                 PrimOp $ Rearrange cs' perm' outer_arr
          | Just (LoopOp (Map inner_cs _ fun [arr])) <- singleExpBody body,
            paramName param == arr =
            delve (n+1) (cs++inner_cs) fun
        delve _ _ _ =
          Nothing
kernelIsRearrange _ = Nothing

kernelIsReshape :: Binding -> Maybe Binding
kernelIsReshape (Let (Pattern [] [outer_patElem]) ()
                 (LoopOp (Map outer_cs _ outer_fun [outer_arr]))) =
  delve outer_cs outer_fun
    where new_shape = arrayDims $ patElemType outer_patElem

          delve cs (Lambda [param] body _)
            | Just (PrimOp (Reshape inner_cs _ arr)) <-
              singleExpBody body,
              paramName param == arr =
              let cs' = cs ++ inner_cs
              in Just $ Let (Pattern [] [outer_patElem]) () $
                 PrimOp $ Reshape cs' new_shape outer_arr

            | Just (LoopOp (Map inner_cs _ fun [arr])) <- singleExpBody body,
              paramName param == arr =
              delve (cs++inner_cs) fun

          delve _ _ =
            Nothing
kernelIsReshape _ = Nothing

kernelBodyOptimisable :: Binding -> Maybe Binding
kernelBodyOptimisable (Let pat () (LoopOp (Map cs w fun arrs))) = do
  bnd <- tryOptimiseKernel =<< singleBindingBody (lambdaBody fun)
  let body = (lambdaBody fun) { bodyBindings = [bnd] }
  return $ Let pat () $ LoopOp $ Map cs w fun { lambdaBody = body } arrs
kernelBodyOptimisable _ =
  Nothing

createKernelNest :: Nestings -> Targets
                 -> Body
                 -> KernelM (Maybe (Binding, Targets))
createKernelNest
  (inner_nest, nests)
  (inner_target@(inner_pat,_), targets)
  inner_body = do
    unless (length nests == length targets) $
      fail $ "Nests and targets do not match!\n" ++
      "nests: " ++ ppNestings (inner_nest, nests) ++
      "\ntargets:" ++ ppTargets (inner_target, targets)
    runMaybeT (recurse $ zip nests targets)

  where patternMapTypes =
          map rowType . patternTypes
        bound_in_nest =
          mconcat $ map boundInNesting $ inner_nest : nests
        liftedTypeOK =
          HS.null . HS.intersection bound_in_nest . freeIn . arrayDims

        distributeAtNesting :: Nesting
                            -> Pattern
                            -> Body
                            -> [Ident]
                            -> (Target -> Targets)
                            -> MaybeT KernelM (Binding, Targets)
        distributeAtNesting
          (nest@(MapNesting nest_let_bound _ cs w params_and_arrs))
          pat
          body
          inner_returned_arrs
          addTarget = do
          let (params,arrs) = unzip params_and_arrs
              width = nestingWidth nest
              (pat', body', identity_map, expand_target) =
                removeIdentityMapping pat body
              required_res = newKernelNames nest_let_bound body'
              (used_params, used_arrs) =
                unzip $
                filter ((`HS.member` freeInBody body') . paramName . fst) $
                zip params arrs

          required_res_idents <- forM (HS.toList required_res) $ \name -> do
            t <- lift $ lookupType name
            return $ Ident name t

          (free_params, free_arrs, bind_in_target) <-
            liftM unzip3 $
            forM (inner_returned_arrs++required_res_idents) $ \(Ident pname ptype) -> do
              unless (liftedTypeOK ptype) $
                fail "Would induce irregular array"
              case HM.lookup pname identity_map of
                Nothing -> do
                  arr <- newIdent (baseString pname ++ "_r") $
                         arrayOfRow ptype width
                  return (Param (Ident pname ptype) (),
                          arr,
                          True)
                Just arr ->
                  return (Param (Ident pname ptype) (),
                          arr,
                          False)

          let free_arrs_pat =
                basicPattern [] $ map ((,BindVar) . snd) $
                filter fst $ zip bind_in_target free_arrs
              free_params_pat =
                map snd $ filter fst $ zip bind_in_target free_params
              rettype = patternMapTypes pat'

              (actual_params, actual_arrs) =
                (used_params++free_params,
                 used_arrs++map identName free_arrs)

          return (Let pat' () $ LoopOp $
                  Map cs w (Lambda actual_params body' rettype) actual_arrs,

                  addTarget $ expand_target
                  (free_arrs_pat, map (Var . paramName) free_params_pat))

        recurse :: [(Nesting,Target)]
                -> MaybeT KernelM (Binding, Targets)
        recurse [] =
          distributeAtNesting
            inner_nest
            inner_pat
            inner_body
            []
            singleTarget

        recurse ((nest, (pat,res)) : nests') = do
          (inner, inner_targets) <- recurse nests'
          distributeAtNesting
            nest
            pat
            (mkBody [inner] res)
            (patternIdents $ fst $ outerTarget inner_targets)
            (`pushOuterTarget` inner_targets)

removeIdentityMapping :: Pattern -> Body
                      -> (Pattern,
                          Body, HM.HashMap VName Ident,
                          Target -> Target)
removeIdentityMapping pat body =
  let (identities, not_identities) =
        mapEither isIdentity $ zip (patternElements pat) (bodyResult body)
      (not_identity_patElems, not_identity_res) = unzip not_identities
      (identity_patElems, identity_res) = unzip identities
      expandTarget (tpat, tres) =
        (Pattern [] $ patternElements tpat ++ identity_patElems,
         tres ++ map Var identity_res)
      inIdentityRes = HM.fromList $ zip identity_res $
                      map patElemIdent identity_patElems
  in (Pattern [] not_identity_patElems,
      body { bodyResult = not_identity_res },
      inIdentityRes,
      expandTarget)
  where bound_in_body = mconcat $
                        map (patternNames . bindingPattern) $
                        bodyBindings body

        isIdentity (patElem, Var v)
          | v `notElem` bound_in_body = Left (patElem, v)
        isIdentity x                  = Right x

unbalancedMap :: MapLoop -> KernelM Bool
unbalancedMap (MapLoop _ _ origlam _) =
  return $ unbalancedLambda mempty origlam
  where subExpBound (Var i) bound = i `HS.member` bound
        subExpBound (Constant _) _ = False

        unbalancedBody bound body =
          any (unbalancedBinding (bound <> boundInBody body) . bindingExp) $
          bodyBindings body

        unbalancedLambda bound lam =
          unbalancedBody
          (foldr (HS.insert . paramName) bound $ lambdaParams lam) $
          lambdaBody lam

        -- XXX - our notion of balancing is probably still too naive.
        unbalancedBinding bound (LoopOp (Map _ w _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (Reduce _ w _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (Scan _ w _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (Redomap _ w _ _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (ConcatMap _ w _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (Stream _ w _ _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (LoopOp (DoLoop _ merge (ForLoop i iterations) body)) =
          iterations `subExpBound` bound ||
          unbalancedBody bound' body
          where bound' = foldr HS.insert bound $
                         i : map (paramName . fst) merge
        unbalancedBinding _ (LoopOp (DoLoop _ _ (WhileLoop _) _)) =
          True

        unbalancedBinding bound (If _ tbranch fbranch _) =
          unbalancedBody bound tbranch || unbalancedBody bound fbranch

        unbalancedBinding bound (SegOp (SegReduce _ w _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (SegOp (SegScan _ w _ _ _ _)) =
          w `subExpBound` bound
        unbalancedBinding bound (SegOp (SegReplicate _ w _ _)) =
          w `HS.member` bound

        unbalancedBinding _ (PrimOp _) =
          False
        unbalancedBinding _ (Apply fname _ _) =
          not $ isBuiltInFunction fname

distributeInnerMap :: Pattern -> MapLoop -> KernelAcc
                   -> KernelM KernelAcc
distributeInnerMap pat maploop@(MapLoop cs w lam arrs) acc =
  unbalancedMap maploop >>= \case
    True ->
      foldr addBindingToKernel acc <$>
      liftM snd (runBinder $ FOT.transformBindingRecursively $
                 Let pat () $ mapLoopExp maploop)
    False ->
      liftM leavingNesting $
      mapNesting pat cs w lam arrs $
      distribute =<<
      distributeMapBodyBindings acc' (bodyBindings $ lambdaBody lam)
      where acc' = KernelAcc { kernelTargets = pushInnerTarget
                                               (pat, bodyResult $ lambdaBody lam) $
                                               kernelTargets acc
                             , kernelRequires = mempty
                             , kernelBindings = mempty
                             }

leavingNesting :: KernelAcc -> KernelAcc
leavingNesting acc =
  acc { kernelTargets =
           case reverse $ snd $ kernelTargets acc of
             [] -> error "The kernel targets list is unexpectedly empty"
             x:xs -> (x, reverse xs)
      }

distributeMapBodyBindings :: KernelAcc -> [Binding] -> KernelM KernelAcc

distributeMapBodyBindings acc [] =
  return acc

distributeMapBodyBindings acc
  (bnd@(Let pat () (LoopOp (Stream cs w (Sequential accs) lam arrs _))):bnds) = do
  let (body_bnds,res) = sequentialStreamWholeArray w accs lam arrs
      reshapeRes t (Var v) = PrimOp $ Reshape cs (arrayDims t) v
      reshapeRes _ se      = PrimOp $ SubExp se
      res_bnds = [ mkLet' [] [ident] $ reshapeRes (identType ident) se
                 | (ident,se) <- zip (patternIdents pat) res ]
  stream_bnds <- copyPropagateInBindings bindableSimpleOps $
                 body_bnds ++ res_bnds
  let msg = unlines ["turned",
                     pretty bnd,
                     "into"] ++
                     unlines (map pretty stream_bnds)
  trace msg distributeMapBodyBindings acc $ stream_bnds ++ bnds

distributeMapBodyBindings acc
  (Let pat () (LoopOp (Redomap cs w lam1 lam2 nes arrs)):bnds) = do
    (mapbnd, redbnd) <- redomapToMapAndReduce pat () (cs, w, lam1, lam2, nes, arrs)
    distributeMapBodyBindings acc $ mapbnd : redbnd : bnds

distributeMapBodyBindings acc (bnd:bnds) =
  -- It is important that bnd is in scope if 'maybeDistributeBinding'
  -- wants to distribute, even if this causes the slightly silly
  -- situation that bnd is in scope of itself.
  withBinding bnd $
  maybeDistributeBinding bnd =<<
  distributeMapBodyBindings acc bnds

maybeDistributeBinding :: Binding -> KernelAcc
                       -> KernelM KernelAcc
maybeDistributeBinding bnd@(Let pat _ (LoopOp (Map cs w lam arrs))) acc =
  -- Only distribute inside the map if we can distribute everything
  -- following the map.
  distributeIfPossible acc >>= \case
    Nothing -> return $ addBindingToKernel bnd acc
    Just acc' -> distribute =<< distributeInnerMap pat (MapLoop cs w lam arrs) acc'
maybeDistributeBinding bnd@(Let _ _ (LoopOp {})) acc = do
  acc' <- distribute acc
  distribute $ addBindingToKernel bnd acc'
maybeDistributeBinding bnd@(Let _ _ (PrimOp (Rearrange {}))) acc = do
  acc' <- distribute acc
  distribute $ addBindingToKernel bnd acc'
maybeDistributeBinding bnd@(Let _ _ (PrimOp (Reshape {}))) acc = do
  acc' <- distribute acc
  distribute $ addBindingToKernel bnd acc'
maybeDistributeBinding bnd acc =
  return $ addBindingToKernel bnd acc
