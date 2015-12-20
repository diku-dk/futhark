{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Futhark.Pass.ExtractKernels.BlockedKernel
       ( blockedReduction
       , blockedScan
       , blockedSegmentedScan
       )
       where

import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Representation.Kernels
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Rename

blockedReduction :: (MonadFreshNames m, HasTypeEnv (NameType Kernels) m) =>
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
      step_two_size = KernelSize one num_chunks one num_chunks one num_chunks

  loop_iterator <- newVName "i"
  seq_lam_index <- newVName "lam_index"
  step_one_pat <- basicPattern' [] <$>
                  mapM (mkIntermediateIdent num_chunks) (patternIdents pat)
  step_two_pat <- basicPattern' [] <$>
                  mapM (mkIntermediateIdent $ Constant $ IntVal 1) (patternIdents pat)
  let (fold_acc_params, fold_arr_params) =
        splitAt (length nes) $ lambdaParams fold_lam
      chunk_size_param = Param chunk_size (Basic Int)
      other_index_param = Param other_index (Basic Int)
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
    runBodyBinder $ withLParamTypes (lambdaParams fold_lam) $ do
      mapM_ addBinding $ compute_start_index : compute_index : read_array_elements
      resultBodyM =<< bodyBind (lambdaBody fold_lam)

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
     Op $ ReduceKernel cs w step_one_size reduce_lam' seqlam nes arrs)

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
    Op $ ReduceKernel [] num_chunks step_two_size
    reduce_lam' identity_lam nes $ patternNames step_one_pat

  forM_ (zip (patternNames step_two_pat) (patternIdents pat)) $ \(arr, x) ->
    addBinding $ mkLet' [] [x] $ PrimOp $ Index [] arr [Constant $ IntVal 0]
  where mkArrChunkParam chunk_size arr_param =
          newParam (baseString (paramName arr_param) <> "_chunk") $
            arrayOfRow (paramType arr_param) chunk_size

        mkIntermediateIdent chunk_size ident =
          newIdent (baseString $ identName ident) $
          arrayOfRow (identType ident) chunk_size

        mkMergeParam (Param pname ptype) (Var v) = do
          v' <- letExp (baseString v ++ "_copy") $ PrimOp $ Copy v
          return (Param pname $ toDecl ptype Unique,
                  Var v')
        mkMergeParam acc_param se =
          return (acc_param { paramAttr = toDecl (paramAttr acc_param) Unique }, se)

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

  return $ KernelSize num_groups group_size per_thread_elements w per_thread_elements num_threads

blockedScan :: (MonadBinder m, Futhark.Tools.Lore m ~ Kernels) =>
               Pattern
            -> Certificates -> SubExp
            -> Lambda
            -> [(SubExp, VName)]
            -> m ()
blockedScan pat cs w lam input = do
  first_scan_size <- blockedKernelSize w
  other_index <- newVName "other_index"
  let num_groups = kernelWorkgroups first_scan_size
      group_size = kernelWorkgroupSize first_scan_size
      num_threads = kernelNumThreads first_scan_size
      elems_per_thread = kernelElementsPerThread first_scan_size
      other_index_param = Param other_index (Basic Int)
      first_scan_lam = lam { lambdaParams = other_index_param : lambdaParams lam }

  sequential_scan_result <-
    letTupExp "sequentially_scanned" $
      Op $ ScanKernel cs w first_scan_size ScanFlat first_scan_lam input

  let (sequentially_scanned, all_group_sums) =
        splitAt (length input) sequential_scan_result

  last_in_preceding_groups <- do
    lasts_map_index <- newVName "lasts_map_index"
    group_id <- newVName "group_id"
    lasts_map_body <- runBodyBinder $ do
      read_lasts <- runBodyBinder $ do
        last_in_group_index <-
          letSubExp "last_in_group_index" $
          PrimOp $ BinOp Minus group_size (intconst 1) Int
        carry_in_index <-
          letSubExp "preceding_group" $ PrimOp $ BinOp Minus (Var group_id) one Int
        let getLastInPrevious sums =
              return $ PrimOp $ Index [] sums [carry_in_index, last_in_group_index]
        eBody $ map getLastInPrevious all_group_sums

      group_lasts <-
        letTupExp "group_lasts" =<<
        eIf (eBinOp Less (eSubExp $ intconst 0) (eSubExp $ Var group_id) Bool)
        (pure read_lasts)
        (eBody $ map eSubExp nes)
      return $ resultBody $ map Var group_lasts
    let lasts_map_returns = [ (rt, [0..arrayRank rt])
                            | rt <- lambdaReturnType lam ]
    letTupExp "last_in_preceding_groups" $
      Op $ MapKernel [] num_groups lasts_map_index
      [(group_id, num_groups)] [] lasts_map_returns lasts_map_body

  group_carry_in <- do
    let second_scan_size = KernelSize one num_groups one num_groups one num_groups
    second_scan_lam <- renameLambda first_scan_lam
    carry_in_scan_result <-
      letTupExp "group_carry_in_and_junk" $
      Op $ ScanKernel cs num_groups second_scan_size ScanFlat second_scan_lam $
      zip nes last_in_preceding_groups
    forM (snd $ splitAt (length input) carry_in_scan_result) $ \arr ->
      letExp "group_carry_in" $
      PrimOp $ Index [] arr [zero]

  chunk_carry_out <- do
    lam'' <- renameLambda lam
    chunk_carry_out_index <- newVName "chunk_carry_out_index"
    group_id <- newVName "group_id"
    elem_id <- newVName "elem_id"
    let (acc_params, arr_params) = splitAt (length nes) $ lambdaParams lam''
        chunk_carry_out_inputs =
          zipWith (mkKernelInput [Var group_id]) acc_params group_carry_in ++
          zipWith (mkKernelInput [Var group_id, Var elem_id]) arr_params all_group_sums

    let chunk_carry_out_returns = [ (rt, [0..arrayRank rt+1])
                                 | rt <- lambdaReturnType lam ]
    letTupExp "chunk_carry_out" $
      Op $ MapKernel [] num_threads chunk_carry_out_index
      [(group_id, num_groups),
       (elem_id, group_size)]
      chunk_carry_out_inputs chunk_carry_out_returns $ lambdaBody lam''

  chunk_carry_out_flat <- forM chunk_carry_out $ \arr -> do
    arr_shape <- arrayShape <$> lookupType arr
    letExp "chunk_carry_out_flat" $
      PrimOp $ Reshape [] (reshapeOuter [DimNew num_threads] 2 arr_shape) arr

  lam''' <- renameLambda lam
  result_map_index <- newVName "result_map_index"
  let j = lambdaIndex lam'''
      (acc_params, arr_params) = splitAt (length nes) $ lambdaParams lam'''
      result_inputs = zipWith (mkKernelInput [Var j]) arr_params sequentially_scanned

  result_map_body <- runBodyBinder $
                     withLParamTypes (map kernelInputParam result_inputs) $ do
    thread_id <-
      letSubExp "thread_id" $
      PrimOp $ BinOp Quot (Var j) elems_per_thread Int
    let do_nothing =
          pure $ resultBody $ map (Var . paramName) arr_params
        add_carry_in = runBodyBinder $ do
          forM_ (zip acc_params chunk_carry_out_flat) $ \(p, arr) -> do
            carry_in_index <-
              letSubExp "carry_in_index" $
              PrimOp $ BinOp Minus thread_id one Int
            letBindNames'_ [paramName p] $
              PrimOp $ Index [] arr [carry_in_index]
          return $ lambdaBody lam'''
    group_lasts <-
      letTupExp "final_result" =<<
        eIf (eBinOp Equal (eSubExp $ intconst 0) (eSubExp thread_id) Bool)
        do_nothing
        add_carry_in
    return $ resultBody $ map Var group_lasts
  let result_map_returns = [ (rt, [0..arrayRank rt])
                           | rt <- lambdaReturnType lam ]
  letBind_ pat $ Op $ MapKernel [] w result_map_index
    [(j, w)] result_inputs result_map_returns result_map_body
  where one = Constant $ IntVal 1
        zero = Constant $ IntVal 0
        (nes, _) = unzip input

        mkKernelInput :: [SubExp] -> LParam -> VName -> KernelInput Kernels
        mkKernelInput indices p arr = KernelInput { kernelInputParam = p
                                                  , kernelInputArray = arr
                                                  , kernelInputIndices = indices
                                                  }

blockedSegmentedScan :: (MonadBinder m, Futhark.Tools.Lore m ~ Kernels) =>
                        SubExp
                     -> Pattern
                     -> Certificates
                     -> SubExp
                     -> Lambda
                     -> [(SubExp, VName)]
                     -> m ()
blockedSegmentedScan segment_size pat cs w lam input = do
  x_flag <- newVName "x_flag"
  y_flag <- newVName "y_flag"
  let x_flag_param = Param x_flag $ Basic Bool
      y_flag_param = Param y_flag $ Basic Bool
      (x_params, y_params) = splitAt (length input) $ lambdaParams lam
      params = [x_flag_param] ++ x_params ++ [y_flag_param] ++ y_params

  body <- runBodyBinder $ withLParamTypes params $ do
    new_flag <- letSubExp "new_flag" $
                PrimOp $ BinOp LogOr (Var x_flag) (Var y_flag) Bool
    seg_res <- letTupExp "seg_res" $ If (Var y_flag)
      (resultBody $ map (Var . paramName) y_params)
      (lambdaBody lam)
      (staticShapes $ lambdaReturnType lam)
    return $ resultBody $ new_flag : map Var seg_res

  flags_global_index <- newVName "flags_global_index"
  flags_i <- newVName "flags_i"
  flags_body <-
    runBodyBinder $ localTypeEnv (HM.singleton flags_i IndexType) $ do
      segment_index <- letSubExp "segment_index" $
                       PrimOp $ BinOp Rem (Var flags_i) segment_size Int
      start_of_segment <- letSubExp "start_of_segment" $
                          PrimOp $ BinOp Equal segment_index zero Bool
      flag <- letSubExp "flag" $
              If start_of_segment (resultBody [true]) (resultBody [false]) [Basic Bool]
      return $ resultBody [flag]
  flags <-
    letExp "flags" $ Op $ MapKernel [] w flags_global_index
    [(flags_i, w)] []
    [(Basic Bool, [0])]
    flags_body

  unused_flag_array <- newVName "unused_flag_array"
  let lam' = Lambda { lambdaIndex = lambdaIndex lam
                    , lambdaParams = params
                    , lambdaBody = body
                    , lambdaReturnType = Basic Bool : lambdaReturnType lam
                    }
      pat' = pat { patternValueElements = PatElem unused_flag_array BindVar
                                          (arrayOf (Basic Bool) (Shape [w]) NoUniqueness) :
                                          patternValueElements pat
                 }
      input' = (false, flags) : input
  blockedScan pat' cs w lam' input'
  where zero = Constant $ IntVal 0
        true = Constant $ LogVal True
        false = Constant $ LogVal False
