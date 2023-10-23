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
import Futhark.Transform.Rename
-- import Debug.Pretty.Simple
import Data.List
type SeqM = ReaderT (Scope GPU) (State VNameSource)

seqFactor :: SubExp
seqFactor = intConst Int64 4


-- NOTE uncomment this for pretty printing AST
-- intraSeq :: Pass GPU GPU
  -- intraSeq = Pass "test" "desc" printAst
  -- printAst :: Prog GPU -> PassM (Prog GPU)
-- printAst prog = pTrace (show prog) (pure prog)

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

seqStm stm = error $ "Expected a SegMap at Group level but got " Data.List.++ show stm




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

-- | seqStm' is assumed to only match on statements encountered within some
-- SegOp at group level
seqStm' ::
  SubExp ->             -- Group id
  (SubExp, SubExp) ->   -- (old size, new size)
  Stm GPU ->
  Builder GPU ()
seqStm' gid sizes stm@(Let pat aux (Op (SegOp
          (SegRed lvl@(SegThread {}) space binops ts kbody)))) = do
  -- Get the thread id
  let [(tid, _)] = unSegSpace space

  -- creates tiles for the arrays read
  names <- tileSegKernelBody kbody sizes gid (Var tid)
  let (vNames, tileNames) = Data.List.unzip names
  -- For each BinOp extract the lambda and create a SegMap performing thread local reduction
  reds <- mapM (mkSegMapRed tileNames sizes) binops
  let redNames = Data.List.map (\(Var x) -> x) reds

  -- -- Update the kbody to use the tile
  let phys = segFlat space
  let [(gtid, _)] = unSegSpace space
  let space' = SegSpace phys [(gtid, snd sizes)]

  -- -- Each VName in fparams should be paired with the corresponding tile
  -- -- created for the array of that VName
  let kbody' = substituteIndexes kbody $ Data.List.zip vNames redNames
  addStm $ Let pat aux (Op (SegOp (SegRed lvl space' binops ts kbody')))
  pure ()
  where
    isFParam :: NameInfo GPU -> Bool
    isFParam (FParamName typebase) = isArray typebase
    isFParam _ = False

    isArray :: TypeBase shape u -> Bool
    isArray (Array {}) = True
    isArray _ = False


seqStm' gid grpSizes (Let pat aux e@(Op (SegOp 
                      (SegScan (SegThread {}) space binops ts kbody)))) = do
  -- TODO: Right now this whole function assumes that only a single tile is
  -- created and that there is only one single SegBinOp as this simplifies
  -- IR generation
  
  -- Create the tiles
  let [(gtid, _)] = unSegSpace space
  names <- tileSegKernelBody kbody grpSizes gid (Var gtid)
  let (vNames, tileNames) = unzip names

  -- Create the reduction part
  redRes <- mapM (mkSegMapRed tileNames grpSizes) binops

  -- for each array of intermediate reduction results, create IR to scan it
  scanAggs <- forM redRes $ \ redRes' -> do
    buildSegScan "scan_agg" $ do
      let (Var redName) = redRes'
      tid <- newVName "tid"
      phys <- newVName "phys_tid"
      binops' <- renameSegBinOp binops

      -- TODO: Need some checks or filtering of the binops actually needed
      -- maybe uneeded parts are removed by simplify?

      e' <- letSubExp "elem" =<< eIndex redName (eSubExp $ Var tid)

      let lvl' = SegThread SegNoVirt Nothing
      let space' = SegSpace phys [(tid, snd grpSizes)]
      pure ([Returns ResultMaySimplify mempty e'],
            lvl', space', binops', ts) -- TODO: idk if ts is correct
        

  -- for each array of aggregate scan results we want to create a SegMap
  -- containing a sequential scan over the chunks 
  res <- forM scanAggs $ \ agg -> do
    buildSegMapThread "res" $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"
      let (Var aggName) = agg

      -- TODO: uses only the first binop right now and assumes singular neutral
      -- element
      let binop = head binops
      let neutral = head $ segBinOpNeutral binop
      lambda <- renameLambda $ segBinOpLambda binop

      idx <- letSubExp "idx" =<< eBinOp (Sub Int64 OverflowUndef)
                                        (eSubExp $ Var tid)
                                        (eSubExp $ intConst Int64 1)
      ne <- letSubExp "ne" =<< eIf (eCmpOp (CmpEq $ IntType Int64)
                                      (eSubExp $ Var tid)
                                      (eSubExp $ intConst Int64 0)
                                   )
                                   (eBody [toExp neutral])
                                   (eBody [eIndex aggName (eSubExp idx)])
      scan <- scanSOAC [Scan lambda [ne]]
      chunkSize <- mkChunkSize tid $ fst grpSizes
      es <- letChunkExp seqFactor tid (snd $ head names) -- TODO: head
      res <- letExp "res" $ Op $ OtherOp $ Screma seqFactor [es] scan
      res' <- letSubExp "res" $ BasicOp $ Reshape ReshapeArbitrary 
                (Shape [seqFactor]) res
      let lvl' = SegThread SegNoVirt Nothing
      let space' = SegSpace phys [(tid, snd grpSizes)]
      let types' = scremaType seqFactor scan
      pure (Returns ResultMaySimplify mempty res', lvl', space', types')

  let (Var res') = head res -- TODO: head
  let exp' = Reshape ReshapeArbitrary (Shape [fst grpSizes]) res'
  addStm $ Let pat aux $ BasicOp exp'
  -- exp' <- eSubExp $ head res -- TODO: head
  -- addStm $ Let pat aux exp'
  pure ()
  


seqStm' _ _ _ = undefined









mkSegMapRed ::
  [VName] ->                  -- The arrays to reduce over
  (SubExp, SubExp) ->                 -- (old size, new size)
  SegBinOp GPU ->
  Builder GPU SubExp
mkSegMapRed arrNames grpSizes binop = do
  let comm = segBinOpComm binop
  lambda <- renameLambda $ segBinOpLambda binop
  let neutral = segBinOpNeutral binop

  let reduce = Reduce comm lambda neutral

  screma <- reduceSOAC [reduce]

  buildSegMapThread "red_intermediate" $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"
      currentSize <- mkChunkSize tid $ fst grpSizes
      es <- mapM (letChunkExp currentSize tid) arrNames
      tmp <- letSubExp "tmp" $ Op $ OtherOp $ Screma currentSize es screma
      let lvl = SegThread SegNoVirt Nothing
      let space = SegSpace phys [(tid, snd grpSizes)]
      let types = scremaType seqFactor screma
      pure (Returns ResultMaySimplify mempty tmp, lvl, space, types)


      
-- Creates a SegMap at thread level performing a sequnetial scan of the
-- threads corresponding chunk
mkSegMapScan ::
  (SubExp, SubExp) ->    -- (old size, new size)
  (VName, VName) ->      -- (arr, tile)
  SubExp ->              -- aggregates
  Exp GPU ->             -- The SegOp
  Builder GPU SubExp
mkSegMapScan grpSizes tile agg (Op (SegOp (SegScan _ _ binops _ _))) = do
  -- Create the scan ScremaForm from the binop
  let binop = head binops -- TODO: asumes one binop
  lambda <- renameLambda $ segBinOpLambda binop
  let neutral = head $ segBinOpNeutral binop -- TODO: head
  let (Var aggName) = agg

   -- Create the actual SegMap operation
  buildSegMapThread "res" $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"

      idx <- letSubExp "idx" 
              =<< eBinOp (Sub Int64 OverflowUndef) 
                        (eSubExp $ Var tid) 
                        (eSubExp $ intConst Int64 1)
      ne <- letSubExp "ne" 
              =<< eIf (eCmpOp (CmpEq $ IntType Int64) 
                              (eSubExp $ Var tid) 
                              (eSubExp $ intConst Int64 0))
                          (eBody [toExp neutral])
                          (eBody [eIndex aggName (eSubExp idx)])

      scan <- scanSOAC [Scan lambda [ne]]
      currentSize <- mkChunkSize tid $ fst grpSizes
      es <- letChunkExp currentSize tid (snd tile)  
      res <- letSubExp "scan_res" $ Op $ OtherOp $ 
                Screma currentSize [es] scan           

      let lvl = SegThread SegNoVirt Nothing
      let space = SegSpace phys [(tid, snd grpSizes)]
      let types = scremaType seqFactor scan
      pure (Returns ResultMaySimplify mempty res, lvl, space, types)
        
  
 -- Catch all
mkSegMapScan _ _ _ e = error $ "Expected a SegScan but got" ++ show e

mkSegScanThread ::
  (SubExp, SubExp) ->      -- (old size, new size)
  SubExp ->                -- The SubExp to scan
  Exp GPU ->               -- The SegScan to "copy"
  Builder GPU SubExp
mkSegScanThread grpsizes arr (Op (SegOp (SegScan lvl space segbinops ts kbody))) = do
  tid <- newVName "tid"
  phys <- newVName "phys_tid"

  -- Modify the segbinop 
  segbinops' <- renameSegBinOp segbinops

  -- Create the new kernelbody
  kbody' <- buildKernelBody $ do
          let (Var arrName) = arr
          e <- letSubExp "elem" $ BasicOp (Index arrName (Slice [DimFix (Var tid)]))
          pure $ Returns ResultMaySimplify mempty e 

  let space'  = SegSpace phys [(tid, snd grpsizes)]
  let segScan = SegScan lvl space' segbinops' ts kbody'

  letSubExp "scan_agg" $ Op $ SegOp segScan
  

mkSegScanThread _ _ e = 
  error $ "Expected a SegScan but got: " ++ show e


buildKernelBody :: Builder GPU KernelResult -> Builder GPU (KernelBody GPU)
buildKernelBody m = do
  (kres, stms) <- collectStms m
  pure $ KernelBody mempty stms [kres]


  
  
renameSegBinOp :: [SegBinOp GPU] -> Builder GPU [SegBinOp GPU]
renameSegBinOp segbinops =
  forM segbinops $ \(SegBinOp comm lam ne shape) -> do
    lam' <- renameLambda lam
    pure $ SegBinOp comm lam' ne shape



substituteIndexes :: KernelBody GPU -> [(VName, VName)] -> KernelBody GPU
substituteIndexes (KernelBody dec stms results) names =
  let stms' = stmsFromList $ Data.List.map (substituteIndex names) $ stmsToList stms
  in KernelBody dec stms' results

substituteIndex :: [(VName, VName)] -> Stm GPU -> Stm GPU
substituteIndex names stm@(Let pat aux (BasicOp (Index name slice))) =
  let (fromNames, toNames) = Data.List.unzip names
      index = Data.List.elemIndex name fromNames
      in case index of
        Just i ->
          let (_:slice') = unSlice slice
          in Let pat aux (BasicOp (Index (toNames Data.List.!! i) $ Slice slice'))
        Nothing -> stm
substituteIndex _ stm = stm

bindNewGroupSize :: SubExp -> Builder GPU SubExp
bindNewGroupSize group_size = do
  name <- newVName "group_size"
  letBindNames [name] $ BasicOp $ BinOp (SDivUp Int64 Unsafe) group_size seqFactor
  pure $ Var name

  
letChunkExp :: SubExp -> VName -> VName -> Builder GPU VName
letChunkExp size tid arrName = do
  letExp "chunk" $ BasicOp $
    Index arrName (Slice [DimFix (Var tid),
    DimSlice (intConst Int64 0) size (intConst Int64 1)])


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
-- SubExp. Just for convenience
buildSegMapThread_ ::
  String ->
  Builder GPU (KernelResult, SegLevel, SegSpace, [Type]) ->
  Builder GPU VName
buildSegMapThread_ name m = do
  subExp <- buildSegMapThread name m
  let (Var name') = subExp
  pure name'

-- | The [KernelResult] from the input monad is what is being passed to the 
-- segmented binops
buildSegScan ::
  String ->          -- SubExp name
  Builder GPU ([KernelResult], SegLevel, SegSpace, [SegBinOp GPU], [Type]) ->
  Builder GPU SubExp
buildSegScan name m = do
  ((results, lvl, space, bops, ts), stms) <- collectStms m
  let kbody = KernelBody () stms results
  letSubExp name $ Op $ SegOp $ SegScan lvl space bops ts kbody


-- | The making of a tile consists of a SegMap to load elements into local
-- memory in a coalesced manner. Some intermediate instructions to modify
-- the tile. Lastly another SegMap to load the correct values into registers
-- 
-- The first SubExp is the group id 
-- The second SubExp is Var containing the groupsize
-- Returns a SubExp that is the chunks variable
mkTile :: SubExp -> (SubExp, SubExp) -> (VName, PrimType) -> Builder GPU SubExp
mkTile gid (oldSize, newSize) (arrName, arrType) = do
  segMap <- buildSegMapThread_ "tile" $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"
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
      chunk <- letSubExp "chunk" $ BasicOp $
                Index tileFlat (Slice [DimSlice start seqFactor (intConst Int64 1)])
      let lvl = SegThread SegNoVirt Nothing
      let space = SegSpace phys [(tid, newSize)]
      let types = [Array arrType (Shape [seqFactor]) NoUniqueness]
      pure (Returns ResultPrivate mempty chunk, lvl, space, types)



-- create tiles for the arrays used in the segop kernelbody
-- each entry in the returned list is a tuple of the name of the array and its tiled replacement
tileSegKernelBody :: KernelBody GPU -> (SubExp, SubExp) -> SubExp -> SubExp
                      -> Builder GPU [(VName, VName)]
tileSegKernelBody (KernelBody _ stms _) grpSizes gid tid = do
  -- let stmsToTile = filter shouldTile $ stmsToList stms
  let stmsToTile = Data.List.foldr shouldTile [] $ stmsToList stms
  let stmsInfos = Data.List.map getTileStmInfo stmsToTile
  tiles <- mapM (mkTile gid grpSizes) stmsInfos
  let (names, _) = Data.List.unzip stmsInfos
  let tileNames = Data.List.map (\(Var x) -> x) tiles
  pure $ Data.List.zip names tileNames
  where
    -- arrays to tile use the thread id and haven't been tiled in same kernelbody
    shouldTile :: Stm GPU -> [Stm GPU] -> [Stm GPU]
    shouldTile stm@(Let _ _ (BasicOp (Index _ slice))) acc =
      let tidIndex = DimFix tid
      in case unSlice slice of
        (_:tid':_) -> if tid' == tidIndex && notTiled then stm:acc else acc
        (tid':_) -> if tid' == tidIndex && notTiled then stm:acc else acc
        _ -> acc
      where
        notTiled = stm `Data.List.notElem` acc
    shouldTile _ acc = acc

    getTileStmInfo :: Stm GPU -> (VName, PrimType)
    getTileStmInfo stm@(Let pat _ (BasicOp (Index name _))) =
      let pes = patElems pat
      in case pes of
        [PatElem _ dec] -> (name, elemType dec)
        _ -> error
              $ "Statements used for tiling should only contain a single VName " Data.List.++ show stm
    getTileStmInfo stm =
      error
        $ "Invalid statement for tiling in IntraSeq " Data.List.++ show stm
