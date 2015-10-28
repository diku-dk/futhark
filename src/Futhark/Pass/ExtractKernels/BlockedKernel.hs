module Futhark.Pass.ExtractKernels.BlockedKernel
       ( blockedReduction
       , blockedScan
       )
       where

import Control.Applicative
import Control.Monad
import Data.Monoid

import Prelude

import Futhark.Representation.Basic
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Rename

blockedReduction :: (MonadFreshNames m, HasTypeEnv m) =>
                    Pattern
                 -> Certificates -> SubExp
                 -> Lambda -> Lambda
                 -> [SubExp]
                 -> [VName]
                 -> m [Binding]
blockedReduction pat cs w reduce_lam fold_lam nes arrs = runBinder_ $ do
  chunk_size <- newVName "chunk_size"
  other_index <- newVName "other_index"
  step_one_size <- blockedKernelSize w

  let one = Constant $ IntVal 1
      num_chunks = kernelWorkgroups step_one_size
      step_two_size = KernelSize one num_chunks one num_chunks one

  loop_iterator <- newVName "i"
  seq_lam_index <- newVName "lam_index"
  step_one_pat <- basicPattern' [] <$>
                      mapM (mkIntermediateIdent num_chunks) (patternIdents pat)
  step_two_pat <- basicPattern' [] <$>
             mapM (mkIntermediateIdent $ Constant $ IntVal 1) (patternIdents pat)
  let (fold_acc_params, fold_arr_params) =
        splitAt (length nes) $ lambdaParams fold_lam
      chunk_size_param = Param (Ident chunk_size $ Basic Int) ()
      other_index_param = Param (Ident other_index $ Basic Int) ()
  arr_chunk_params <- mapM (mkArrChunkParam $ Var chunk_size) fold_arr_params
  res_idents <- mapM (newIdent' (<>"_res") . paramIdent) fold_acc_params

  start_index <- newVName "start_index"

  ((merge_params, unique_nes), seq_copy_merge) <-
    collectBindings $
    unzip <$> zipWithM mkMergeParam fold_acc_params nes

  let res = map paramName merge_params
      form = ForLoop loop_iterator $ Var chunk_size

      compute_start_index =
        mkLet' [] [Ident start_index $ Basic Int] $
        PrimOp $ BinOp Times (Var seq_lam_index) (kernelElementsPerThread step_one_size) Int
      compute_index = mkLet' [] [Ident (lambdaIndex fold_lam) $ Basic Int] $
                      PrimOp $ BinOp Plus (Var start_index) (Var loop_iterator) Int
      read_array_elements =
        [ mkLet' [] [paramIdent param] $ PrimOp $ Index [] arr [Var loop_iterator]
          | (param, arr) <- zip fold_arr_params $ map paramName arr_chunk_params ]

  seq_loop_body <-
    runBodyBinder $ bindingParamTypes (lambdaParams fold_lam) $ do
      mapM_ addBinding $ compute_start_index : compute_index : read_array_elements
      let uniqueRes (Var v) = do
            t <- lookupType v
            if unique t || basicType t
              then return $ Var v
              else letSubExp "unique_res" $ PrimOp $ Copy v
          uniqueRes se =
            return se
      resultBodyM =<< mapM uniqueRes =<< bodyBind (lambdaBody fold_lam)

  let seq_loop =
        mkLet' [] res_idents $
        LoopOp $ DoLoop res (zip merge_params unique_nes) form seq_loop_body
      seq_body = mkBody (seq_copy_merge++[seq_loop]) $ map (Var . identName) res_idents

      seqlam = Lambda { lambdaParams = chunk_size_param : arr_chunk_params
                      , lambdaReturnType = lambdaReturnType fold_lam
                      , lambdaBody = seq_body
                      , lambdaIndex = seq_lam_index
                      }

      reduce_lam' = reduce_lam { lambdaParams = other_index_param :
                                                lambdaParams reduce_lam
                               }

  addBinding =<< renameBinding
    (Let step_one_pat () $
     LoopOp $ ReduceKernel cs w step_one_size reduce_lam' seqlam nes arrs)

  identity_lam_params <- mapM (mkArrChunkParam $ Var chunk_size) fold_acc_params

  elements <- zipWithM newIdent
              (map (baseString . paramName) identity_lam_params) $
              lambdaReturnType seqlam
  let read_elements =
        [ mkLet' [] [element] $
          PrimOp $ Index [] arr [Constant $ IntVal 0]
        | (element, arr) <- zip elements $ map paramName identity_lam_params ]
      identity_body = mkBody read_elements $ map (Var . identName) elements
      identity_lam = Lambda { lambdaParams = chunk_size_param : identity_lam_params
                            , lambdaReturnType = lambdaReturnType fold_lam
                            , lambdaBody = identity_body
                            , lambdaIndex = seq_lam_index
                            }

  addBinding $
    Let step_two_pat () $
    LoopOp $ ReduceKernel [] num_chunks step_two_size
    reduce_lam' identity_lam nes $ patternNames step_one_pat

  forM_ (zip (patternNames step_two_pat) (patternIdents pat)) $ \(arr, x) ->
    addBinding $ mkLet' [] [x] $ PrimOp $ Index [] arr [Constant $ IntVal 0]
  where mkArrChunkParam chunk_size arr_param = do
          ident <- newIdent (baseString (paramName arr_param) <> "_chunk") $
                   arrayOfRow (paramType arr_param) chunk_size
          return $ Param ident ()

        mkIntermediateIdent chunk_size ident =
          newIdent (baseString $ identName ident) $
          arrayOfRow (identType ident) chunk_size

        mkMergeParam acc_param (Var v) = do
          v' <- letExp (baseString v ++ "_copy") $ PrimOp $ Copy v
          return (acc_param { paramIdent =
                              paramIdent acc_param `setIdentUniqueness` Unique
                            },
                  Var v')
        mkMergeParam acc_param se =
          return (acc_param, se)

blockedKernelSize :: MonadBinder m =>
                     SubExp -> m KernelSize
blockedKernelSize w = do
  num_groups <- letSubExp "num_groups" $
                Apply (nameFromString "num_groups") [] (basicRetType Int)
  group_size <- letSubExp "group_size" $
                Apply (nameFromString "group_size") [] (basicRetType Int)

  num_threads <-
    letSubExp "num_threads" $ PrimOp $ BinOp Times num_groups group_size Int

  per_thread_elements <-
    letSubExp "per_thread_elements" =<<
    eDivRoundingUp (eSubExp w) (eSubExp num_threads)

  return $ KernelSize num_groups group_size per_thread_elements w per_thread_elements

blockedScan :: (MonadFreshNames m, HasTypeEnv m) =>
               Pattern
            -> Certificates -> SubExp
            -> Lambda
            -> [(SubExp, VName)]
            -> m [Binding]
blockedScan pat cs w lam input = runBinder_ $ do
  first_scan_size <- blockedKernelSize w
  in_workgroup_scanned <-
    letTupExp "in_workgroup_scanned" $
    LoopOp $ ScanKernel cs w first_scan_size lam input

  let num_groups = kernelWorkgroups first_scan_size

  lasts_map_index <- newVName "lasts_map_index"
  i <- newVName "i"
  lasts_map_body <- runBodyBinder $ do
    last_in_group_index <-
      letSubExp "last_in_group_index" $
      PrimOp $ BinOp Times
      (Var i) (kernelWorkgroupSize first_scan_size) Int
    group_lasts <- forM in_workgroup_scanned $ \arr ->
      letExp ("last_in_" ++ baseString arr) $
      PrimOp $ Index [] arr [last_in_group_index]
    return $ resultBody $ map Var group_lasts
  let lasts_map_returns = [ (rt, [0..arrayRank rt])
                          | rt <- lambdaReturnType lam ]
  last_in_groups <-
    letTupExp "last_in_groups_pre" $
    LoopOp $ Kernel [] num_groups lasts_map_index
    [(i, num_groups)] [] lasts_map_returns lasts_map_body
  last_in_groups' <- forM (zip last_in_groups nes) $ \(last_in_group_pre, ne) ->
    letInPlace "last_in_groups" [] last_in_group_pre [zero] $ PrimOp $ SubExp ne

  let second_scan_size = KernelSize one num_groups one num_groups one
  lam' <- renameLambda lam
  group_offsets <-
    letTupExp "group_offsets" $
    LoopOp $ ScanKernel cs num_groups second_scan_size lam' $
    zip nes last_in_groups'

  lam'' <- renameLambda lam
  result_map_index <- newVName "result_map_index"
  let j = lambdaIndex lam''
      (acc_params, arr_params) = splitAt (length nes) $ lambdaParams lam''
      mkResultInput p arr = KernelInput { kernelInputParam = p
                                        , kernelInputArray = arr
                                        , kernelInputIndices = [Var j]
                                        }
      result_inputs = zipWith mkResultInput arr_params in_workgroup_scanned
  result_map_body <- runBodyBinder $ do
    group_id <-
      letSubExp "group_id" $
      PrimOp $ BinOp Rem
      (Var j) (kernelWorkgroupSize first_scan_size) Int
    forM_ (zip acc_params group_offsets) $ \(acc, arr) ->
      letBindNames'_ [paramName acc] $
      PrimOp $ Index [] arr [group_id]
    resultBody <$> bodyBind (lambdaBody lam'')
  let result_map_returns = [ (rt, [0..arrayRank rt])
                           | rt <- lambdaReturnType lam ]
  letBind pat $ LoopOp $ Kernel [] w result_map_index
    [(j, w)] result_inputs result_map_returns result_map_body
  where zero = Constant $ IntVal 0
        one = Constant $ IntVal 1
        (nes, _) = unzip input
