{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Futhark.Optimise.IntraSeq (intraSeq) where

import Language.Futhark.Core
import Futhark.Pass
import Futhark.IR.GPU
import Futhark.Builder.Class
import Futhark.Construct
import Futhark.Analysis.PrimExp.Convert

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M

type SeqM = ReaderT (Scope GPU) (State VNameSource)


-- An Env is a mapping from SubExp to SubExp. The keys are subexpressions
-- present in the original program and the value is a newly generated
-- subexpression generated during the transformation
type Env = M.Map SubExp SubExp

-- The sequentialistion factor, i.e. how many elements each thread should
-- process. This is not supposed to be a constant.
seqFactor :: SubExp
seqFactor = intConst Int64 4

getTypes :: [Type] -> [PrimType]
getTypes = map f
  where
    f x =
      case x of
          Prim t -> t
          Array t shp u-> t
          _ -> error "What to do here?"

-- read in seq factor elements for a given thread
getChunk :: (MonadBuilder m) => VName -> SubExp -> m VName
getChunk arr tid = do
  arr_t <- lookupType arr
  slice_offset <- letSubExp "slice_offset" =<< toExp (pe64 tid * pe64 seqFactor)
  let slice = DimSlice slice_offset seqFactor (intConst Int64 1)
  letExp "chunk" $ BasicOp $ Index arr $ fullSlice arr_t [slice]

-- bind the new group size
bindNewGroupSize :: SubExp -> Builder GPU VName
bindNewGroupSize group_size = do
  name <- newVName "group_size"
  letBindNames [name] $ BasicOp $ BinOp (SDivUp Int64 Unsafe) group_size seqFactor
  pure name



-- This function takes the SegBinOp array of e.g. a SegRed and produces a kernel
-- body from it. It also takes a KernelBody as the result part is used to 
-- determine exactly which statements of the BinOps needs to be inserted into
-- the new KernelBody
mkBodyFromBinops :: [SegBinOp GPU] -> KernelBody GPU -> KernelBody GPU
mkBodyFromBinops bops kbody = do
  let dec  = kernelBodyDec kbody
  let stms = kernelBodyStms kbody
  let res  = kernelBodyResult kbody

  -- Figure out how many results of the original KernelBody are used in
  -- the SegBinOps. This is done by, for each result, backtrack and see which
  -- statements are needed to compute said result
  -- let needed = filter (\(r,x) -> not $ null x) $ zip res $ map (backtrack stms) res
  let stms' = mconcat $ map (backtrack stms) res
  KernelBody dec stms' res

  where
    backtrack :: Stms GPU -> KernelResult -> Stms GPU
    backtrack _ (WriteReturns {})   = error "How to handle WriteReturns"
    backtrack _ (TileReturns {})    = error "How to handle TileReturns"
    backtrack _ (RegTileReturns {}) = error "How to handle RegTileReturns"
    backtrack stms (Returns manifest certs subexp) = do
      -- If the returned value is constant there is nothing to do
      case subexp of
        Constant _ -> mempty
        Var vname -> backtrack' vname (stmsToList stms)

    backtrack' :: VName -> [Stm GPU] -> Stms GPU
    backtrack' _ [] = mempty
    backtrack' vname (stm:stms)= do
      let pat = patElems $ stmPat stm
      let used = any (\ x -> patElemName x == vname) pat
      if used then oneStm stm <> backtrack' vname stms
      else mempty




-- NOTE uncomment this for pretty printing AST
-- intraSeq :: Pass GPU GPU
  -- intraSeq = Pass "test" "desc" printAst
  -- printAst :: Prog GPU -> PassM (Prog GPU)
-- printAst prog = pTrace (show prog) (pure prog)

-- TODO handle when last thread does not have seqFactor elements to process
intraSeq :: Pass GPU GPU
intraSeq =
    Pass "name" "description" $
      intraproceduralTransformation onStms
    where
      onStms scope stms =
        modifyNameSource $
          runState $
            runReaderT (seqStms M.empty stms) scope

-- TDOO: Are we sure that a mapM is fine here. Couldnt we get a case where
-- we got a long line of simple let bindings that would the updated environment?
seqStms :: Env -> Stms GPU -> SeqM (Stms GPU)
seqStms env stms =
  localScope (scopeOf stms) $
    mconcat <$> mapM (seqStm env) (stmsToList stms)

seqBody :: Env -> KernelBody GPU -> SeqM (KernelBody GPU)
seqBody env (KernelBody dec stms result) = do
  stms' <- seqStms env stms
  pure $ KernelBody dec stms' result

-- seq outer map, divide group size by sequentialization factor
seqStm :: Env -> Stm GPU -> SeqM (Stms GPU)
seqStm env (Let pat aux (Op (SegOp (SegMap
        lvl@(SegGroup virt (Just (KernelGrid num_groups (Count group_size))))
        space ts kbody)))) = do
  (gr_name, gr_stm) <- runBuilder $ bindNewGroupSize group_size
  let lvl' = SegGroup virt (Just $ KernelGrid num_groups $ Count (Var gr_name))
  kbody' <- seqBody (M.insert group_size (Var gr_name) env) kbody
  pure $ gr_stm <> oneStm (Let pat aux (Op (SegOp (SegMap lvl' space ts kbody'))))


-- When reaching a segred at thread level we need to create the segmap doing 
-- the sequential work and modify the original segred to use the newly created
-- intermediate array from the segmap
seqStm env (Let pat aux (Op (SegOp (SegRed lvl@SegThread {} space binops ts kbody)))) = do
  -- Extract different information from the original SegRed
  let [Prim tp] = ts                              -- TODO: Always expects a single type
  let phys = segFlat space                        -- The physical thread
  let (gtid, bound):ps = unSegSpace space         -- (gtid < bound) TODO: What to do with ps?

  -- Create new names for the to be created SegMap and update the Env
  phys' <- newName phys
  gtid' <- newName gtid
  let env' = M.insert (Var gtid) (Var gtid') env

  let Just map_bound = M.lookup bound env       -- TODO handle nothing
  let map_space = SegSpace phys' ((gtid', map_bound) : ps)
  map_ident <- newIdent "map_res" (Array tp (Shape [map_bound]) mempty)

  -- TODO handle and update kernelbody
  let map_bodies = lambdaBody $ segBinOpLambda $ head binops    -- TODO: Handle multiple binops
  let results    = map (resToRes . resSubExp) (bodyResult map_bodies)
  stms <- runBuilder_ $ mkRedLoop (head ts) (head $ segBinOpNeutral $ head binops) (bodyStms map_bodies)
  let map_kbody  = KernelBody mempty stms results
  let map_stm    = mkLet' [map_ident] aux (Op (SegOp (SegMap lvl map_space ts map_kbody)))

  let red_space = SegSpace phys ((gtid, map_bound) : ps)
  let red_stm   = Let pat aux (Op (SegOp (SegRed lvl red_space binops ts kbody)))

  -- TODO let a = count number of statements in kernelbody
  -- let b = subtract accumulators from arugments to reduce lambda
  -- kernelbody stms not used in reduce = a - b 

  pure $ stmsFromList [map_stm, red_stm]
  where
    resToRes :: SubExp -> KernelResult
    resToRes = Returns ResultMaySimplify mempty

seqStm env (Let pat aux (Op (SegOp (SegScan lvl@SegThread {} space binops ts kbody)))) = do
  pure $ oneStm $ Let pat aux (Op (SegOp (SegScan lvl space binops ts kbody)))

seqStm env (Let pat aux (Op (SegOp (SegHist lvl@SegThread {} space binops ts kbody)))) = do
  pure $ oneStm $ Let pat aux (Op (SegOp (SegHist lvl space binops ts kbody)))

seqStm env stm = pure $ oneStm stm


-- Make a loop that performs a reduction. 
-- First argument is the neutral element, second is the statements that make
-- up the opereation i.e. + in 'reduce (+) 0 arr'
mkRedLoop :: Type -> SubExp -> Stms GPU -> Builder GPU ()
mkRedLoop tp ne stms = do
  i <- newVName "i"
  let loop_body = mkBody stms $ varsRes [i]
  params <- newParam "v" $ toDecl tp Unique
  loop_exp <- letExp "loop_res" $ Loop [(params, ne)] (ForLoop i Int64 seqFactor []) loop_body
  pure ()