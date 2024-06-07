{-# LANGUAGE LambdaCase #-}

-- | Efficient sequentialization pass for intra-group kernels
--   Extends the MSc thesis work of Christian Marslev & Jonas Grønborg
-- 

module Futhark.Optimise.EffSeqIntra (effSeqIntra) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
-- import Data.IntMap.Strict qualified as IM
import Data.List qualified as L
import Data.Map.Strict qualified as M
-- import Data.Set qualified as S
import Futhark.Builder.Class
import Futhark.Construct
import Futhark.IR.GPU
import Futhark.IR.GPU.Simplify (simplifyGPU)
import Futhark.Pass
import Futhark.IR.Aliases
import Futhark.Analysis.Alias qualified as AnlAls
import Futhark.Analysis.LastUse
import Futhark.Optimise.ArrayShortCircuiting.DataStructs
-- import Futhark.Transform.Rename
-- import Futhark.Transform.Substitute
import Futhark.Optimise.EffSeqIntra.Helper
import Debug.Trace


type EffSeqM a = ReaderT (Scope GPU) (State VNameSource) a

-----------------------------------------------------
--- A builder with additionaql fail functionality ---
-----------------------------------------------------

-- | A builder with additional fail functionality
type EffSeqBuilder a = ExceptT () (Builder GPU) a

runEffSeqBuilder ::
  (MonadFreshNames m, HasScope GPU m) =>
  EffSeqBuilder a ->
  m (Maybe (Stms GPU))
runEffSeqBuilder (ExceptT b) = do
  (tmp, stms) <- runBuilder b
  case tmp of
    Left  _ -> pure Nothing
    Right _ -> pure . Just $ stms

collectEffSeqBuilder ::
  EffSeqBuilder a ->
  EffSeqBuilder (a, Stms GPU)
collectEffSeqBuilder (ExceptT b) = do
  (tmp, stms) <- lift $ do collectStms b
  case tmp of
    Left  _ -> throwError ()
    Right x -> pure (x, stms)

collectEffSeqBuilder' ::
  EffSeqBuilder a ->
  EffSeqBuilder (Stms GPU)
collectEffSeqBuilder' (ExceptT b) = do
  (tmp, stms) <- lift $ do collectStms b
  case tmp of
    Left  _ -> throwError ()
    Right _ -> pure stms

runEffSeqMExtScope :: EffSeqM a -> Scope GPU -> Builder GPU a
runEffSeqMExtScope m sc = do
  scp <- askScope
  let sc' = sc <> scp
  let tmp = runReaderT m sc'
  st <- get
  let tmp' = runState tmp st
  pure $ fst tmp'

---------------------------------------------------------
--- Identifying Intra-Group Kernels
---------------------------------------------------------

-- TODO: 
-- 1. this pass does not traverse the whole program

effSeqIntra :: Pass GPU GPU
effSeqIntra =
  Pass "Efficient Sequentialization of Intragroup Kernels" 
       "Optimizes communication, infers memory mappings, etc." $
    \prog -> do
      let prog_w_alises = AnlAls.aliasAnalysis prog
          (lu_tab_cts, lu_tab_fns) = lastUseGPUNoMem prog_w_alises
          lu_tab_fns_lst = map (M.toList) (M.elems lu_tab_fns) 
      (intraproceduralTransformation onStms
              >=> simplifyGPU) prog
  where
    onStms scope stms =
      modifyNameSource $
        runState $
          runReaderT (seqStms stms) scope

-- SeqStms is only to be used for top level statements, i.e., kernels.
-- Use seqStms' to ffficiently sequentialize the statements within an
-- intra-group kernel
seqStms :: Stms GPU -> EffSeqM (Stms GPU)
seqStms stms = do
  tmp <- runEffSeqBuilder $ forM (stmsToList stms) seqStm
  case tmp of
    Nothing    -> pure stms
    Just stms' -> pure stms'

-- | Matches against singular statements at the group level. That is statements
-- that are either SegOps at group level or intermediate statements between
-- such statements
seqStm :: Stm GPU -> EffSeqBuilder ()
seqStm stm@( Let pat aux ( Op ( SegOp ( SegMap lvl space ts kbody ) ) ) )
  | L.length (unSegSpace space) == 1,
    SegBlock virt (Just grid) <- lvl,
    KernelBody _ stms kres <- kbody,
    shouldSequentialize (stmAuxAttrs aux) = do
  -- implementation starts here
  let seqFactor = getSeqFactor $ stmAuxAttrs aux
  let grpId = fst $ head $ unSegSpace space
  let sizeOld = unCount $ gridBlockSize grid
  scp <- askScope
  let sizeOld_info = case sizeOld of
                       Var nm -> case M.lookup nm scp of
                                   Just (LetName ldec) -> [ldec]
                                   -- Just (IndexName itp) -> [itp]
                                   -- Just (LParamName x) -> [x]
                                   _ -> []
                       _ -> []
                       
                       
  sizeNew <- lift $ do
    letSubExp "group_size"
      =<< eBinOp
            (SDivUp Int64 Unsafe)
            (eSubExp sizeOld)
            (eSubExp seqFactor)
  let env = Env (Var grpId) sizeNew sizeOld Nothing mempty seqFactor
  
  trace  ("Cosmin Debug: seqFactor: "++prettyString seqFactor ++
         " space: "++prettyString (unSegSpace space) ++
         " grid: "++prettyString grid ++
         " old grid size: "++prettyString sizeOld ++
         " info: " ++ prettyString sizeOld_info ++
         " new grid size: "++prettyString sizeNew
         ) $ lift $ do addStm stm

seqStm stm@( Let _ _ ( Op ( SegOp ( SegMap lvl _ _ _ ) ) ) ) =
  lift $ do addStm stm

-- Catch all pattern. This will mainly just tell us if we encounter some
-- statement in a test program so that we know that we will have to handle it
seqStm stm = lift $ do addStm stm

{--
    | L.length (unSegSpace space) /= 1 = lift $ do addStm stm
    | not $ shouldSequentialize (stmAuxAttrs aux) = lift $ do addStm stm
    | otherwise = do
        let seqFactor = getSeqFactor $ stmAuxAttrs aux
        let grpId = fst $ head $ unSegSpace space
        let sizeOld = unCount $ gridBlockSize grid
        sizeNew <- lift $ do
          letSubExp "group_size"
            =<< eBinOp
              (SDivUp Int64 Unsafe)
              (eSubExp sizeOld)
              (eSubExp seqFactor)

        let env = Env (Var grpId) sizeNew sizeOld Nothing mempty seqFactor

        -- As we need to catch 'internal' errors we use runSeqBuilder here
        res <- runSeqBuilder $ do
          exp' <- buildSegMap' $ do
            env' <- mkTiles env

            let grid' = Just $ KernelGrid (gridNumBlocks grid) (Count sizeNew)
            let lvl' = SegBlock virt grid'

            _ <- seqStms' env' stms

            kres' <- lift $ do flattenResults pat kres
            pure (kres', lvl', space, ts)

          lift $ do addStm $ Let pat aux exp'

        -- Based on error or not we now return the correct program
        case res of
          Nothing -> lift $ do addStm stm
          -- Just stms' -> lift $ do addStms stms'
          Just stms'
            | not $ isOneStm (stmsToList stms') -> lift $ do addStm stm
            | otherwise -> do
                let [stm'] = stmsToList stms'

                -- Create the braches with each code version
                body1 <- lift $ do mkMatchBody stm'
                body2 <- lift $ do mkMatchBody stm

                -- Create the conditional statements
                cond <- lift $ do
                  eCmpOp
                    (CmpSle Int64)
                    (eSubExp $ intConst Int64 32)
                    (eSubExp $ grpSize env)

                matchExp <- lift $ do
                  eIf'
                    (pure cond)
                    (pure body1)
                    (pure body2)
                    MatchEquiv

                lift $ do addStm (Let pat aux matchExp)
    where
      isOneStm :: [Stm GPU] -> Bool
      isOneStm [_] = True
      isOneStm _ = False

      mkMatchBody :: Stm GPU -> Builder GPU (Body GPU)
      mkMatchBody (Let pat' aux' exp') = do
        newPat <- renamePat pat'
        newExp <- renameExp exp'
        let newStm = Let newPat aux' newExp
        let pNames = L.map patElemName $ patElems newPat
        let res = L.map (SubExpRes mempty . Var) pNames
        pure $ Body mempty (stmsFromList [newStm]) res
seqStm (Let pat aux (Match scrutinee cases def dec)) = do
  cases' <- forM cases seqCase
  let (Body ddec dstms dres) = def
  dstms' <- collectSeqBuilder' $ forM (stmsToList dstms) seqStm
  (dres', stms') <-
    collectSeqBuilder $
      localScope (scopeOf dstms') $
        fixReturnTypes pat dres
  let def' = Body ddec (dstms' <> stms') dres'
  lift $ do addStm $ Let pat aux (Match scrutinee cases' def' dec)
  where
    seqCase :: Case (Body GPU) -> SeqBuilder (Case (Body GPU))
    seqCase (Case cpat body) = do
      let (Body bdec bstms bres) = body
      bstms' <-
        collectSeqBuilder' $
          forM (stmsToList bstms) seqStm
      (bres', stms') <-
        collectSeqBuilder $
          localScope (scopeOf bstms') $
            fixReturnTypes pat bres
      let body' = Body bdec (bstms' <> stms') bres'
      pure $ Case cpat body'
seqStm (Let pat aux (Loop header form body)) = do
  let fparams = L.map fst header
  let (Body bdec bstms bres) = body
  bstms' <-
    collectSeqBuilder' $
      localScope (scopeOfFParams fparams) $
        forM_ (stmsToList bstms) seqStm
  (bres', stms') <-
    collectSeqBuilder $
      localScope (scopeOf bstms') $
        fixReturnTypes pat bres
  let body' = Body bdec (bstms' <> stms') bres'
  lift $ do addStm $ Let pat aux (Loop header form body')

-- Catch all pattern. This will mainly just tell us if we encounter some
-- statement in a test program so that we know that we will have to handle it
seqStm stm = lift $ do addStm stm
--}


