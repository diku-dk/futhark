{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
module Futhark.Optimise.IntraSeq (intraSeq) where

import Language.Futhark.Core
import Futhark.Pass
import Futhark.IR.GPU
import Futhark.Builder.Class
import Futhark.Construct

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Futhark.Transform.Rename
import Futhark.Transform.FirstOrderTransform




type SeqM = ReaderT (Scope GPU) (State VNameSource)

seqFactor :: SubExp
seqFactor = intConst Int64 4



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
            runReaderT (seqStms stms) scope




-- SeqStms is only to be used for top level statements. To sequentialize
-- statements within a body use seqStms'
seqStms :: Stms GPU -> SeqM (Stms GPU)
seqStms stms =
  localScope (scopeOf stms) $
    mconcat <$> mapM seqStm (stmsToList stms)





-- Like seqStms, segStm is only for top level statements
seqStm :: Stm GPU -> SeqM (Stms GPU)
seqStm (Let pat aux (Op (SegOp (
            SegMap lvl@(SegGroup virt (Just grid)) space ts kbody)))) = do
  -- Create the new group size
  let grpSizeOld = unCount $ gridGroupSize grid
  (groupSize, groupStms) <- runBuilder $ bindNewGroupSize grpSizeOld
  let grid' = Just $  KernelGrid (gridNumGroups grid) (Count groupSize)

  -- Get the group id (gtid)
  -- TODO: Assumes only a single space
  let [(gtid, _)] = unSegSpace space

  kbody' <- seqBody (Var gtid) (grpSizeOld, groupSize) kbody
  pure $ groupStms <>
     oneStm (Let pat aux (Op (SegOp (SegMap (SegGroup virt grid') space ts kbody'))))

seqStm stm = error $ "Expected a SegMap at Group level but got " ++ show stm




-- First SubExp is the group id (gtid)
-- Second SubExp is the gruup size
seqBody :: 
  SubExp ->                 -- Group ID
  (SubExp, SubExp) ->       -- (old size, new size)
  KernelBody GPU ->         
  SeqM (KernelBody GPU)
seqBody grpId sizes (KernelBody dec stms result) = do
  stms' <- localScope (scopeOf stms) $ runBuilder_ $ seqStms' grpId sizes stms
  pure $ KernelBody dec stms' result








-- Much the same as seqStms but takes the group size to pass along
seqStms' ::
  SubExp ->             -- Group id
  (SubExp, SubExp)->    -- (old size, new size)
  Stms GPU ->
  Builder GPU ()
seqStms' grpId sizes stms = do
  mapM_ (seqStm' grpId sizes) $ stmsToList stms

-- seqStm' is assumed to only match on statements encountered within some
-- SegOp at group level
seqStm' ::
  SubExp ->             -- Group id
  (SubExp, SubExp) ->   -- (old size, new size)
  Stm GPU ->
  Builder GPU ()
seqStm' grpId sizes stm@(Let pat aux (Op (SegOp
                      (SegRed lvl@(SegThread {}) space binops ts kbody)))) = do
  -- Get the thread id
  let [(tid, _)] = unSegSpace space

  -- Create a tile
  -- TODO: Might be better to do an analysis on which arrays are read and then
  -- create tiles for these
  allScope <- askScope
  let fparams = M.toList $ M.filter isFParam allScope
  let fparamNames = map fst fparams
  tiles <- mapM (mkTile grpId sizes) fparams
  let tileNames = map (\(Var x) -> x) tiles

  -- For each BinOp extract the lambda and create a SegMap
  -- TODO: uses head.
  reds <- mapM (mkSegMapRed (head tileNames) sizes) binops
  let redNames = map (\(Var x) -> x) reds

  -- Update the kbody to use the tile
  let phys = segFlat space
  let [(gtid, _)] = unSegSpace space
  let space' = SegSpace phys [(gtid, snd sizes)]

  -- Each VName in fparams should be paired with the corresponding tile
  -- created for the array of that VName
  let kbody' = substituteIndexes kbody [(head fparamNames, head redNames)]

  -- Temporary just to get the original statement along
  addStm $ Let pat aux (Op (SegOp (SegRed lvl space' binops ts kbody')))
  -- addStm stm

  pure ()
  -- undefined
  where
    isFParam :: NameInfo GPU -> Bool
    isFParam (FParamName typebase) = isArray typebase
    isFParam _ = False

    isArray :: TypeBase shape u -> Bool
    isArray (Array {}) = True
    isArray _ = False

      

seqStm' _ _ _ = undefined


substituteIndexes :: KernelBody GPU -> [(VName, VName)] -> KernelBody GPU
substituteIndexes (KernelBody dec stms results) names = 
  let stms' = stmsFromList $ map (substituteIndex names) $ stmsToList stms
  in KernelBody dec stms' results

substituteIndex :: [(VName, VName)] -> Stm GPU -> Stm GPU
substituteIndex [(from, to)] stm@(Let pat aux (BasicOp (Index name slice)))
  | from == name = do
    let (_:slice') = unSlice slice
    Let pat aux (BasicOp (Index to $ Slice slice'))
substituteIndex _ stm = stm

-- bind the new group size
bindNewGroupSize :: SubExp -> Builder GPU SubExp
bindNewGroupSize group_size = do
  name <- newVName "group_size"
  letBindNames [name] $ BasicOp $ BinOp (SDivUp Int64 Unsafe) group_size seqFactor
  pure $ Var name


mkSegMapRed ::
  VName ->                  -- The array to reduce over
  (SubExp, SubExp) ->                 -- (old size, new size)
  SegBinOp GPU ->
  Builder GPU SubExp
mkSegMapRed arrName grpSize binop = do
  let comm = segBinOpComm binop
  lambda <- renameLambda $ segBinOpLambda binop
  let neutral = segBinOpNeutral binop

  let reduce = Reduce comm lambda neutral

  screma <- reduceSOAC [reduce]

  buildSegMapThread "red_intermediate" $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"
      size <- mkChunkSize tid $ fst grpSize
      e <- letExp "chunk" $ BasicOp $
              Index arrName (Slice [DimFix (Var tid),
                            DimSlice (intConst Int64 0) size (intConst Int64 1)])
      tmp <- letSubExp "tmp" $ Op $ OtherOp $ Screma size [e] screma
      let lvl = SegThread SegNoVirt Nothing
      let space = SegSpace phys [(tid, snd grpSize)]
      let types = scremaType seqFactor screma
      pure (Returns ResultMaySimplify mempty tmp, lvl, space, types)



-- | The making of a tile consists of a SegMap to load elements into local
-- memory in a coalesced manner. Some intermediate instructions to modify
-- the tile. Lastly another SegMap to load the correct values into registers
-- 
-- The first SubExp is the group id 
-- The second SubExp is Var containing the groupsize
-- Returns a SubExp that is the chunks variable
mkTile :: 
  SubExp ->                   -- Group id
  (SubExp, SubExp) ->         -- (old size, new size)
  (VName, NameInfo GPU) -> 
  Builder GPU SubExp
mkTile gid (oldSize, newSize) (arrName, arrInfo)= do
  let (FParamName typebase) =  arrInfo
  let arrType = elemType typebase

  segMap <- buildSegMapThread_ "tile" $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"
      -- size <- mkChunkSize tid oldSize
      e <- letSubExp "slice" $ BasicOp $
                Index arrName (Slice [DimFix gid, DimSlice (Var tid) seqFactor newSize])
      let lvl = SegThread SegNoVirt Nothing
      let space = SegSpace phys [(tid, newSize)]
      let types = [Array arrType (Shape [seqFactor]) NoUniqueness]
      pure (Returns ResultMaySimplify mempty e, lvl, space, types)

  tileTrans <- letExp "tile_T" $ BasicOp $ Rearrange [1,0] segMap
  tileFlat <- letExp "tile_flat" $ BasicOp $
                Reshape ReshapeArbitrary (Shape [oldSize]) tileTrans

  -- SegMap to read the actual chunks the threads need
  buildSegMapThread "chunks" $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"
      start <- letSubExp "start" $ BasicOp $
                BinOp (Mul Int64 OverflowUndef) (Var tid) seqFactor
      -- size <- mkChunkSize tid oldSize
      chunk <- letSubExp "chunk" $ BasicOp $
                Index tileFlat (Slice [DimSlice start seqFactor (intConst Int64 1)])
      let lvl = SegThread SegNoVirt Nothing
      let space = SegSpace phys [(tid, newSize)]
      let types = [Array arrType (Shape [seqFactor]) NoUniqueness]
      pure (Returns ResultPrivate mempty chunk, lvl, space, types)


-- Generates statements that compute the pr. thread chunk size. This is needed
-- as the last thread in a block might not have seqFactor amount of elements
-- to read. 
mkChunkSize :: 
  VName ->               -- The thread id
  SubExp ->              -- old size 
  Builder GPU SubExp     -- Returns the SubExp in which the size is
mkChunkSize tid sOld = do 
  offset <- letSubExp "offset" $ BasicOp $
              BinOp (Mul Int64 OverflowUndef) (Var tid) seqFactor
  tmp <- letSubExp "tmp" $ BasicOp $
              BinOp (Sub Int64 OverflowUndef) sOld offset 
  letSubExp "size" $ BasicOp $
              BinOp (SMin Int64) tmp seqFactor
  

  
  -- cmp <- eCmpOp (CmpSle Int64) (eSubExp $ Var tid) (eSubExp seqFactor)
  -- ifB <- eBody [eSubExp seqFactor]
  -- elseB <- buildBody_  $ do
  --     g <- letSubExp "global" $ BasicOp $ BinOp (Mul Int64 OverflowUndef) (Var tid) seqFactor
  --     l <- letSubExp "local" $ BasicOp $ BinOp (Sub Int64 OverflowUndef) grpSize g
  --     pure [SubExpRes mempty l]
  -- ifExp <- eIf (pure cmp) (pure ifB) (pure elseB)
  -- letSubExp "sliceSize" =<< ifExp


  -- cmp <- eCmpOp (CmpSle Int64) (eSubExp $ Var tid) (eSubExp grpSize)
  -- ifBody <- eBody [eSubExp seqFactor]

  -- -- The computations for the else body before the creation of the body
  -- g <- eBinOp (Mul Int64 OverflowUndef) (eSubExp $ Var tid) (eSubExp seqFactor)
  -- l <- eBinOp (Sub Int64 OverflowUndef) (eSubExp grpSize) (pure g)y
  -- elseBody <- eBody [pure g, pure l]

  -- ifRes <- eIf (cmp) (ifBody) (elseBody)

  -- letSubExp "slice_sice" =<< ifRes



-- Builds a SegMap at thread level containing all bindings created in m
-- and returns the subExp which is the variable containing the result
buildSegMapThread ::
  String ->
  Builder GPU (KernelResult, SegLevel, SegSpace, [Type]) ->
  Builder GPU SubExp
buildSegMapThread name  m = do
  ((res, lvl, space, ts), stms) <- collectStms m
  let kbody = KernelBody () stms [res]
  letSubExp name $ Op $ SegOp $ SegMap lvl space ts kbody

-- Like buildSegMapThread but returns the VName instead of the actual 
-- SubExp. Just for convinience
buildSegMapThread_ ::
  String ->
  Builder GPU (KernelResult, SegLevel, SegSpace, [Type]) ->
  Builder GPU VName
buildSegMapThread_ name m = do
  subExp <- buildSegMapThread name m
  let (Var name') = subExp
  pure name'




