{-# LANGUAGE LambdaCase #-}

-- | Add explicit parameters for global names used in device functions.
module Futhark.Pass.AddGlobalParams (addGlobalParams) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer.Strict
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Futhark.IR.GPU
import Futhark.MonadFreshNames
import Futhark.Pass
import Futhark.Transform.Substitute

data CallMode = AllCalls | ParallelCalls
  deriving (Eq)

callsInProg :: CallMode -> Prog GPU -> S.Set Name
callsInProg mode prog =
  callsInStms mode False (progConsts prog)
    <> foldMap (callsInBody mode False . funDefBody) (progFuns prog)

calledInParallel :: Prog GPU -> S.Set Name
calledInParallel = callsInProg ParallelCalls

callsInGBody :: CallMode -> Bool -> GBody GPU res -> S.Set Name
callsInGBody mode in_parallel = callsInStms mode in_parallel . bodyStms

callsInBody :: CallMode -> Bool -> Body GPU -> S.Set Name
callsInBody = callsInGBody

callsInKernelBody :: CallMode -> KernelBody GPU -> S.Set Name
callsInKernelBody mode = callsInGBody mode True

callsInStms :: CallMode -> Bool -> Stms GPU -> S.Set Name
callsInStms mode in_parallel =
  foldMap (callsInExp mode in_parallel . stmExp) . stmsToList

callsInExp :: CallMode -> Bool -> Exp GPU -> S.Set Name
callsInExp mode in_parallel = \case
  Apply fname _ _ _
    | mode == AllCalls || in_parallel -> S.singleton fname
    | otherwise -> mempty
  Match _ cases defbody _ ->
    foldMap (callsInBody mode in_parallel . caseBody) cases
      <> callsInBody mode in_parallel defbody
  Loop _ _ body ->
    callsInBody mode in_parallel body
  WithAcc inputs lam ->
    foldMap
      ( \(_, _, op) ->
          maybe mempty (\(f, _) -> callsInLambda mode in_parallel f) op
      )
      inputs
      <> callsInLambda mode in_parallel lam
  Op op ->
    callsInOp mode in_parallel op
  _ ->
    mempty

callsInLambda :: CallMode -> Bool -> Lambda GPU -> S.Set Name
callsInLambda mode in_parallel (Lambda _ _ body) = callsInBody mode in_parallel body

callsInSOAC :: CallMode -> Bool -> SOAC GPU -> S.Set Name
callsInSOAC mode in_parallel soac = execWriter $ void $ mapSOACM mapper soac
  where
    mapper =
      identitySOACMapper
        { mapOnSOACLambda = \lam -> do
            tell $ callsInLambda mode in_parallel lam
            pure lam
        }

callsInSegOp :: CallMode -> SegOp SegLevel GPU -> S.Set Name
callsInSegOp mode segop = execWriter $ void $ mapSegOpM mapper segop
  where
    mapper =
      identitySegOpMapper
        { mapOnSegBinOpLambda = \lam -> do
            tell $ callsInLambda mode True lam
            pure lam,
          mapOnSegPostOpLambda = \lam -> do
            tell $ callsInLambda mode True lam
            pure lam,
          mapOnSegOpBody = \body -> do
            tell $ callsInKernelBody mode body
            pure body
        }

callsInOp :: CallMode -> Bool -> Op GPU -> S.Set Name
callsInOp mode in_parallel = \case
  SegOp segop ->
    callsInSegOp mode segop
  OtherOp soac ->
    callsInSOAC mode in_parallel soac
  GPUBody _ body ->
    callsInBody mode True body
  _ ->
    mempty

buildCallGraphGPU :: Prog GPU -> M.Map Name (S.Set Name)
buildCallGraphGPU =
  M.fromList
    . map (\fd -> (funDefName fd, callsInBody AllCalls False $ funDefBody fd))
    . progFuns

transitiveClosure :: (Ord k) => M.Map k (S.Set k) -> S.Set k -> S.Set k
transitiveClosure graph = go
  where
    go seen =
      let seen' = seen <> S.unions [M.findWithDefault mempty f graph | f <- S.toList seen]
       in if seen' == seen then seen else go seen'

globalsPerFun :: M.Map Name (S.Set Name) -> M.Map Name (S.Set VName) -> M.Map Name (S.Set VName)
globalsPerFun call_graph = fixpoint
  where
    fixpoint m =
      let step f gs =
            gs
              <> S.unions
                [ M.findWithDefault mempty g m
                | g <- S.toList $ M.findWithDefault mempty f call_graph
                ]
          m' = M.mapWithKey step m
       in if m' == m then m else fixpoint m'

globalTypes :: Stms GPU -> M.Map VName DeclType
globalTypes =
  M.fromList
    . concatMap
      ( map
          (\pe -> (patElemName pe, toDecl (patElemType pe) Nonunique))
          . patElems
          . stmPat
      )
    . stmsToList

transformProg :: Prog GPU -> PassM (Prog GPU)
transformProg prog = do
  let global_tps = globalTypes $ progConsts prog
      globals = M.keysSet global_tps
      call_graph = buildCallGraphGPU prog
      roots = calledInParallel prog
      device_funs = transitiveClosure call_graph roots
      direct_globals =
        M.fromList
          [ (funDefName fd, S.fromList (namesToList $ freeIn fd) `S.intersection` globals)
          | fd <- progFuns prog,
            funDefName fd `S.member` device_funs
          ]
      transitive_globals = globalsPerFun call_graph direct_globals
      all_used_globals = S.unions $ M.elems transitive_globals

  global_to_param <-
    M.fromList <$> mapM (\g -> (g,) <$> newName g) (S.toAscList all_used_globals)

  let globals_ordered =
        M.map S.toAscList transitive_globals
      globals_for_fun f =
        M.findWithDefault mempty f globals_ordered
      params_for =
        mapMaybe $ \g -> do
          p <- M.lookup g global_to_param
          t <- M.lookup g global_tps
          pure $ Param mempty p t
      use_name env g = M.findWithDefault g g env
      call_args env f =
        [ (Var $ use_name env g, Observe)
        | g <- globals_for_fun f
        ]
      call_rewriter :: M.Map VName VName -> Exp GPU -> Exp GPU
      call_rewriter env (Apply fname args rettype safety) =
        Apply fname (args <> call_args env fname) rettype safety
      call_rewriter env e = mapExp mapper e
        where
          mapper :: Mapper GPU GPU Identity
          mapper =
            Mapper
              { mapOnSubExp = pure,
                mapOnBody = const $ pure . rewriteBody env,
                mapOnVName = pure,
                mapOnRetType = pure,
                mapOnBranchType = pure,
                mapOnFParam = pure,
                mapOnLParam = pure,
                mapOnOp = pure . rewriteOp env
              }
      rewriteStm :: M.Map VName VName -> Stm GPU -> Stm GPU
      rewriteStm env (Let pat aux e) = Let pat aux $ call_rewriter env e
      rewriteStms :: M.Map VName VName -> Stms GPU -> Stms GPU
      rewriteStms env = stmsFromList . map (rewriteStm env) . stmsToList
      rewriteGBody :: M.Map VName VName -> GBody GPU res -> GBody GPU res
      rewriteGBody env (Body dec stms res) =
        Body dec (rewriteStms env stms) res
      rewriteBody :: M.Map VName VName -> Body GPU -> Body GPU
      rewriteBody = rewriteGBody
      rewriteKernelBody :: M.Map VName VName -> KernelBody GPU -> KernelBody GPU
      rewriteKernelBody = rewriteGBody
      rewriteLambda :: M.Map VName VName -> Lambda GPU -> Lambda GPU
      rewriteLambda env (Lambda ps ret body) =
        Lambda ps ret $ rewriteBody env body
      rewriteOp :: M.Map VName VName -> Op GPU -> Op GPU
      rewriteOp env (SegOp segop) =
        let segmapper =
              identitySegOpMapper
                { mapOnSegBinOpLambda = pure . rewriteLambda env,
                  mapOnSegPostOpLambda = pure . rewriteLambda env,
                  mapOnSegOpBody = pure . rewriteKernelBody env
                }
         in SegOp $ runIdentity $ mapSegOpM segmapper segop
      rewriteOp env (OtherOp soac) =
        let soacmapper =
              identitySOACMapper
                { mapOnSOACLambda = pure . rewriteLambda env
                }
         in OtherOp $ runIdentity $ mapSOACM soacmapper soac
      rewriteOp env (GPUBody ts body) =
        GPUBody ts $ rewriteBody env body
      rewriteOp _ op = op

      rewriteFun :: FunDef GPU -> FunDef GPU
      rewriteFun fd
        | funDefName fd `S.member` device_funs =
            let gs = globals_for_fun (funDefName fd)
                env =
                  M.fromList
                    [ (g, p)
                    | g <- gs,
                      Just p <- [M.lookup g global_to_param]
                    ]
                substs = env
                extra_params = params_for gs
                body' = substituteNames substs $ funDefBody fd
             in fd
                  { funDefParams = funDefParams fd <> extra_params,
                    funDefBody = rewriteBody env body'
                  }
        | otherwise =
            fd {funDefBody = rewriteBody mempty (funDefBody fd)}

  pure
    prog
      { progConsts = rewriteStms mempty $ progConsts prog,
        progFuns = map rewriteFun $ progFuns prog
      }

-- | Ensure that device functions do not reference global names directly.
addGlobalParams :: Pass GPU GPU
addGlobalParams =
  Pass
    { passName = "add global params",
      passDescription = "Thread global names explicitly into device functions.",
      passFunction = transformProg
    }
