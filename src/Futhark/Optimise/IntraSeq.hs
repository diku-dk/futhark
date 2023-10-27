module Futhark.Optimise.IntraSeq (intraSeq) where

import Language.Futhark.Core
import Futhark.Pass
import Futhark.IR.GPU
import Futhark.Builder.Class
import Futhark.Construct
import Futhark.Transform.Rename

import Control.Monad.Reader
import Control.Monad.State

import Data.Map as M
import Data.IntMap.Strict as IM

import Debug.Pretty.Simple
import Data.List as L




type SeqM a = ReaderT (Scope GPU) (State VNameSource) a



runSeqM :: SeqM a -> Builder GPU a
runSeqM m = do
  scp <- askScope
  let tmp = runReaderT m scp
  st <- get
  let tmp' = runState tmp st
  pure $ fst tmp'



-- | A structure for conveniant passing of different information needed at 
-- various stages during the pass.
data Env = Env {
  grpId      :: SubExp,             -- The group id
  grpSize    :: SubExp,             -- The group size after seq
  grpsizeOld :: SubExp,             -- The group size before seq
  tileMap    :: M.Map VName VName,  -- Mapping from arrays to tiles
  seqFactor  :: SubExp
}



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
seqStms ::
  Stms GPU ->
  SeqM (Stms GPU)
seqStms stms =
  foldM (\ss s -> do
      ss' <- runBuilder_ $ localScope (scopeOf ss) $ seqStm s
      pure $ ss <> ss'
      ) mempty (stmsToList stms)


-- | Matches agaoinst singular statements at the group level. That is statements
-- that are either SegOps at group level or intermediate statements between
-- such statements
seqStm ::
  Stm GPU ->
  Builder GPU ()
seqStm (Let pat aux (Op (SegOp (
            SegMap (SegGroup virt (Just grid)) space ts kbody)))) = do
  -- As we are at group level all arrays in scope must be global, i.e. not
  -- local to the current group. We simply create a tile for all such arrays
  -- and let a Simplify pass remove unused tiles.

  -- TODO: Somehow select what the seqFactor should be
  let e       = intConst Int64 4
  let grpId   = fst $ head $ unSegSpace space
  -- let sizeOld = snd $ head $ unSegSpace space
  let sizeOld = unCount $ gridGroupSize grid
  sizeNew <- letSubExp "group_size" =<< eBinOp (SDivUp Int64 Unsafe)
                                            (eSubExp sizeOld)
                                            (eSubExp e)

  let env = Env (Var grpId) sizeNew sizeOld mempty e

  exp' <- buildSegMap' $ do
    -- Update the env with mappings
    env' <- mkTiles env

    -- Create the new grid with the new group size
    let grid' = Just $ KernelGrid (gridNumGroups grid) (Count sizeNew)
    kresults <- seqKernelBody env' kbody

    let lvl' = SegGroup virt grid'

    kresults' <- flattenResults pat kresults

    pure (kresults', lvl', space, ts)

  addStm $ Let pat aux exp'
  pure ()



-- Catch all pattern. This will mainly just tell us if we encounter some
-- statement in a test program so that we know that we will have to handle it
seqStm stm = error $
             "Encountered unhandled statement at group level: " ++ show stm



seqKernelBody ::
  Env ->
  KernelBody GPU ->
  Builder GPU [KernelResult]
seqKernelBody env (KernelBody _ stms results) = do
  seqStms' env stms
  pure results




-- | Much like seqStms but now carries an Env
seqStms' ::
  Env ->
  Stms GPU ->
  Builder GPU ()
seqStms' env stms = do
  stms' <- foldM (\ss s -> do
      ss' <- runBuilder_ $ localScope (scopeOf ss <> scopeOf s) $ seqStm' env s
      pure $ ss <> ss'
      ) mempty (stmsToList stms)
  addStms stms'


-- |Expects to only match on statements at thread level. That is SegOps at
-- thread level or statements between such SegOps
seqStm' ::
  Env ->
  Stm GPU ->
  Builder GPU ()
seqStm' env (Let pat aux
            (Op (SegOp (SegRed lvl@(SegThread {}) space binops ts kbody)))) = do

  -- Find all free variables from the kbody and use that to filter out 
  -- unneeded tiles
  let free = IM.elems $ namesIntMap $ freeIn kbody
  let tiles = M.toList $ tileMap env
  let tilesUsed = intersectBy (\(a,_)(b,_) -> a == b) tiles (zip free free)

  -- For each SegBinOp we should create an intermediate reduction result
  reds <- mapM (mkSegMapRed env tilesUsed) binops

  -- Pair the original arrays with the intermediate results
  -- TODO head in reds until multiple binops are supported
  let mapping = M.fromList $ zipWith (curry (\((a,_), r) -> (a,r))) tilesUsed $ head reds

  -- Modify the kernel body
  -- TODO: Assumes single dimension
  let tid = fst $ head $ unSegSpace space
  kbody' <- runSeqM $ seqKernelBody' env mapping tid kbody

  let space' = SegSpace (segFlat space) [(fst $ head $ unSegSpace space, grpSize env)]



  addStm $ Let pat aux (Op (SegOp (SegRed lvl space' binops ts kbody')))



seqStm' env (Let pat aux
            (Op (SegOp (SegScan (SegThread {}) space binops ts kbody)))) = do

  let free = IM.elems $ namesIntMap $ freeIn kbody
  let tiles = M.toList $ tileMap env
  let tilesUsed = intersectBy (\(a,_)(b,_) -> a == b) tiles (zip free free)

  reds <- mapM (mkSegMapRed env tilesUsed) binops
  -- TODO: head until multiple binops
  let redshead = head reds
  scans <- forM redshead $ \red -> buildSegScan "scan_agg" $ do
    tid <- newVName "tid"
    phys <- newVName "phys_tid"
    binops' <- renameSegBinOp binops

    e' <- letSubExp "elem" =<< eIndex red (eSubExp $ Var tid)

    let lvl' = SegThread SegNoVirt Nothing
    let space' = SegSpace phys [(tid, grpSize env)]
    pure ([Returns ResultMaySimplify mempty e'], lvl', space', binops', ts)

  scans' <- forM scans $ \scan -> buildSegMapTup_ "scan_res" $ do
    tid <- newVName "tid"
    phys <- newVName "phys_tid"

    -- TODO: Uses head
    let binop = head binops
    let neutral = head $ segBinOpNeutral binop
    lambda <- renameLambda $ segBinOpLambda binop

    let (Var scanName) = scan

    idx <- letSubExp "idx" =<< eBinOp (Sub Int64 OverflowUndef)
                                    (eSubExp $ Var tid)
                                    (eSubExp $ intConst Int64 1)
    ne <- letSubExp "ne" =<< eIf (eCmpOp (CmpEq $ IntType Int64)
                                  (eSubExp $ Var tid)
                                  (eSubExp $ intConst Int64 0)
                               )
                               (eBody [toExp neutral])
                               (eBody [eIndex scanName (eSubExp idx)])
    scanSoac <- scanSOAC [Scan lambda [ne]]
    es <- letChunkExp (seqFactor env) tid (snd $ head $ M.toList $ tileMap env)
    res <- letSubExp "res" $ Op $ OtherOp $ Screma (seqFactor env) [es] scanSoac

    let lvl' = SegThread SegNoVirt Nothing
    let space' = SegSpace phys [(tid, grpSize env)]
    let types' = scremaType (seqFactor env) scanSoac
    pure ([Returns ResultMaySimplify mempty res], lvl', space', types')

  -- TODO first head mult binops
  let exp' = Reshape ReshapeArbitrary (Shape [grpsizeOld env]) (head $ head scans')
  addStm $ Let pat aux $ BasicOp exp'

  pure ()



-- Catch all
seqStm' _ stm = error $
                "Encountered unhandled statement at thread level: " ++ show stm



seqKernelBody' ::
  Env ->
  Map VName VName -> -- mapping from global array to intermediate results
  VName ->            -- Thread id
  KernelBody GPU ->
  SeqM (KernelBody GPU)
seqKernelBody' env mapping tid (KernelBody dec stms results) = do
  stms' <- seqStms'' env mapping tid stms
  pure $ KernelBody dec stms' results


seqStms'' ::
  Env ->
  Map VName VName ->
  VName ->            -- Thread id
  Stms GPU ->
  SeqM (Stms GPU)
seqStms'' env mapping tid stms = do
  foldM (\ss s -> do
      ss' <- runBuilder_ $ localScope (scopeOf ss <> scopeOf s) $ seqStm'' env mapping tid s
      pure $ ss <> ss'
      ) mempty (stmsToList stms)


seqStm'' ::
  Env ->
  Map VName VName ->
  VName ->            -- Thread id
  Stm GPU ->
  Builder GPU ()
seqStm'' env mapping tid stm@(Let pat aux (BasicOp (Index arr _))) =
  if M.member arr mapping then do
    let (Just arr') = M.lookup arr mapping
    let slice' = Slice [DimFix $ Var tid]
    addStm $ Let pat aux (BasicOp (Index arr' slice'))
  else
    addStm stm

seqStm'' _ _ _ stm = addStm stm


mkSegMapRed ::
  Env ->
  [(VName, VName)] ->
  SegBinOp GPU ->
  Builder GPU [VName]
mkSegMapRed env mapping binop = do

    let comm = segBinOpComm binop
    let ne   = segBinOpNeutral binop
    lambda <- renameLambda $ segBinOpLambda binop

    reduce <- reduceSOAC [Reduce comm lambda ne]

    buildSegMapTup_ "red_intermediate" $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"
      sz <- mkChunkSize tid env

      -- For each tile we can then get the corresponding chunk
      chunks <- mapM (getChunk env tid sz) mapping

      res <- letTupExp' "res" $ Op $ OtherOp $
                Screma sz chunks reduce

      let lvl' = SegThread SegNoVirt Nothing
      let space' = SegSpace phys [(tid, grpSize env)]
      let types' = scremaType (seqFactor env) reduce
      let kres = L.map (Returns ResultMaySimplify mempty) res
      pure (kres, lvl', space', types')


getChunk ::
  Env ->
  VName ->              -- thread Id
  SubExp ->             -- size of chunk
  (VName, VName) ->     -- array to tile mapping
  Builder GPU VName
getChunk _ tid sz (_, tile) = do
  letExp "chunk" $ BasicOp $ Index tile
    (Slice [DimFix $ Var tid,
            DimSlice (intConst Int64 0) sz (intConst Int64 1)])


flattenResults ::
  Pat (LetDec GPU)->
  [KernelResult] ->
  Builder GPU [KernelResult]
flattenResults pat kresults = do
  let types = L.map patElemType (patElems pat)
  subExps <- forM (zip kresults types) $ \(res, tp)-> do
    let resSubExp = kernelResultSubExp res
    case resSubExp of
      (Constant _) -> letSubExp "const_res" $ BasicOp $ SubExp resSubExp
      (Var name) -> do
          resType <- lookupType name
          if arrayRank resType == 0 then
            letSubExp "scalar_res" $ BasicOp $ SubExp resSubExp
          else
            letSubExp "reshaped_res" $ BasicOp $ Reshape ReshapeArbitrary (arrayShape $ stripArray 1 tp) name

  let kresults' = L.map (Returns ResultMaySimplify mempty) subExps

  pure kresults'



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



-- substituteIndexes :: KernelBody GPU -> [(VName, VName)] -> KernelBody GPU
-- substituteIndexes (KernelBody dec stms results) names =
--   let stms' = stmsFromList $ Data.List.map (substituteIndex names) $ stmsToList stms
--   in KernelBody dec stms' results

-- substituteIndex :: [(VName, VName)] -> Stm GPU -> Stm GPU
-- substituteIndex names stm@(Let pat aux (BasicOp (Index name slice))) =
--   let (fromNames, toNames) = unzip names
--       in case elemIndex name fromNames of
--         Just i ->
--           -- after tiling we only need the last index
          -- let slice' = last $ unSlice slice
--           in Let pat aux (BasicOp (Index (toNames !! i) $ Slice [slice']))
--         Nothing -> stm
-- bindNewGroupSize :: SubExp -> Builder GPU SubExp
-- bindNewGroupSize group_size = do
--   name <- newVName "group_size"
--   letBindNames [name] $ BasicOp $ BinOp (SDivUp Int64 Unsafe) group_size seqFactor
--   pure $ Var name


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
  Env ->
  Builder GPU SubExp     -- Returns the SubExp in which the size is
mkChunkSize tid env = do
  offset <- letSubExp "offset" $ BasicOp $
              BinOp (Mul Int64 OverflowUndef) (Var tid) (seqFactor env)
  tmp <- letSubExp "tmp" $ BasicOp $
              BinOp (Sub Int64 OverflowUndef) (grpsizeOld env) offset
  letSubExp "size" $ BasicOp $
              BinOp (SMin Int64) tmp (seqFactor env)




-- | The making of a tile consists of a SegMap to load elements into local
-- memory in a coalesced manner. Some intermediate instructions to modify
-- the tile. Lastly another SegMap to load the correct values into registers
-- 
-- The first SubExp is the group id 
-- The second SubExp is Var containing the groupsize
-- Returns a SubExp that is the chunks variable
-- mkTile :: SubExp -> (SubExp, SubExp) -> (VName, PrimType) -> Builder GPU SubExp
-- mkTile gid (oldSize, newSize) (arrName, arrType) = do
--   segMap <- buildSegMap_ "tile" $ do
--       tid <- newVName "tid"
--       phys <- newVName "phys_tid"
--       e <- letSubExp "slice" $ BasicOp $
--                 Index arrName (Slice [DimFix gid, DimSlice (Var tid) seqFactor newSize])
--       let lvl = SegThread SegNoVirt Nothing
--       let space = SegSpace phys [(tid, newSize)]
--       let types = [Array arrType (Shape [seqFactor]) NoUniqueness]
--       pure (Returns ResultMaySimplify mempty e, lvl, space, types)

--   tileTrans <- letExp "tile_T" $ BasicOp $ Rearrange [1,0] segMap
--   tileFlat <- letExp "tile_flat" $ BasicOp $
--                 Reshape ReshapeArbitrary (Shape [oldSize]) tileTrans

--   -- SegMap to read the actual chunks the threads need
--   buildSegMap "chunks" $ do
--       tid <- newVName "tid"
--       phys <- newVName "phys_tid"
--       start <- letSubExp "start" $ BasicOp $
--                 BinOp (Mul Int64 OverflowUndef) (Var tid) seqFactor
--       chunk <- letSubExp "chunk" $ BasicOp $
--                 Index tileFlat (Slice [DimSlice start seqFactor (intConst Int64 1)])
--       let lvl = SegThread SegNoVirt Nothing
--       let space = SegSpace phys [(tid, newSize)]
--       let types = [Array arrType (Shape [seqFactor]) NoUniqueness]
--       pure (Returns ResultPrivate mempty chunk, lvl, space, types)

-- | Creates a tile for each array in scope at the time of caling it.
-- That is if called at the correct time it will create a tile for each
-- global array
mkTiles ::
  Env ->
  Builder GPU Env
mkTiles env = do
  scope <- askScope
  let arraysInScope = M.toList $  M.filter isArray scope

  tiles <- forM arraysInScope $ \ (arrName, arrInfo) -> do

    let tp = elemType $ typeOf arrInfo
    -- Read coalesced
    tile <- buildSegMap_ "tile" $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"
      let outerDim = ([DimFix $ grpId env | arrayRank (typeOf arrInfo) > 1])
      es <- letSubExp "elems" $ BasicOp $ Index arrName
                  (Slice $ outerDim ++
                            [DimSlice (Var tid) (seqFactor env) (grpSize env)])
      let lvl = SegThread SegNoVirt Nothing
      let space = SegSpace phys [(tid, grpSize env)]
      let types = [Array tp (Shape [seqFactor env]) NoUniqueness]
      pure ([Returns ResultMaySimplify mempty es], lvl, space, types)

    -- transpose and flatten
    tileT <- letExp "tileT" $ BasicOp $ Rearrange [1,0] tile
    tileFlat <- letExp "tile_flat" $ BasicOp $ Reshape
                ReshapeArbitrary (Shape [grpsizeOld env]) tileT

    -- Read the pr. thread chunk into registers
    tile' <- buildSegMap_ "tile" $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"
      start <- letSubExp "start" =<< eBinOp (Mul Int64 OverflowUndef)
                                            (eSubExp $ Var tid)
                                            (eSubExp $ seqFactor env)
      es <- letSubExp "chunk" $ BasicOp $ Index tileFlat
            (Slice [DimSlice start (seqFactor env) (intConst Int64 1)])
      let lvl = SegThread SegNoVirt Nothing
      let space = SegSpace phys [(tid, grpSize env)]
      let types = [Array tp (Shape [seqFactor env]) NoUniqueness]
      pure ([Returns ResultPrivate mempty es], lvl, space, types)

    -- return the original arr name with the name of its local tile
    pure (arrName, tile')

  pure $ (\(Env gid gSize gSizeOld _ factor) ->
            Env gid gSize gSizeOld (M.fromList tiles) factor) env

  where
    isArray :: NameInfo GPU -> Bool
    isArray info = arrayRank (typeOf info) > 0


-- create tiles for the arrays used in the segop kernelbody
-- each entry in the returned list is a tuple of the name of the array and its tiled replacement
-- tileSegKernelBody :: KernelBody GPU -> (SubExp, SubExp) -> SubExp -> SubExp
--                       -> Builder GPU [(VName, VName)]
-- tileSegKernelBody (KernelBody _ stms _) grpSizes gid tid = do
--   -- let stmsToTile = filter shouldTile $ stmsToList stms
--   let stmsToTile = Data.List.foldr shouldTile [] $ stmsToList stms
--   let stmsInfos = Data.List.map getTileStmInfo stmsToTile
--   tiles <- mapM (mkTile gid grpSizes) stmsInfos
--   let (names, _) = Data.List.unzip stmsInfos
--   let tileNames = Data.List.map (\(Var x) -> x) tiles
--   pure $ Data.List.zip names tileNames
--   where
--     -- arrays to tile use the thread id and haven't been tiled in same kernelbody
--     shouldTile :: Stm GPU -> [Stm GPU] -> [Stm GPU]
--     shouldTile stm@(Let _ _ (BasicOp (Index _ slice))) acc =
--       let tidIndex = DimFix tid
--       in case unSlice slice of
--         (_:tid':_) -> if tid' == tidIndex && notTiled then stm:acc else acc
--         (tid':_) -> if tid' == tidIndex && notTiled then stm:acc else acc
--         _ -> acc
--       where
--         notTiled = stm `Data.List.notElem` acc
--     shouldTile _ acc = acc

--     getTileStmInfo :: Stm GPU -> (VName, PrimType)
--     getTileStmInfo stm@(Let pat _ (BasicOp (Index name _))) =
--       let pes = patElems pat
--       in case pes of
--         [PatElem _ dec] -> (name, elemType dec)
--         _ -> error
--               $ "Statements used for tiling should only contain a single VName " Data.List.++ show stm
--     getTileStmInfo stm =
--       error
--         $ "Invalid statement for tiling in IntraSeq " Data.List.++ show stm



-- Monadic builder functions


-- Builds a SegMap at thread level containing all bindings created in m
-- and returns the subExp which is the variable containing the result
buildSegMap ::
  String ->
  Builder GPU ([KernelResult], SegLevel, SegSpace, [Type]) ->
  Builder GPU SubExp
buildSegMap name m = do
  ((res, lvl, space, ts), stms) <- collectStms m
  let kbody = KernelBody () stms res
  letSubExp name $ Op $ SegOp $ SegMap lvl space ts kbody

-- Like buildSegMap but returns the VName instead of the actual 
-- SubExp. Just for convenience
buildSegMap_ ::
  String ->
  Builder GPU ([KernelResult], SegLevel, SegSpace, [Type]) ->
  Builder GPU VName
buildSegMap_ name m = do
  subExps <- buildSegMap name m
  pure $ varFromExp subExps
  where
    varFromExp :: SubExp -> VName
    varFromExp (Var nm) = nm
    varFromExp e = error $ "Expected SubExp of type Var, but got:\n" ++ show e

-- like buildSegMap but builds a tup exp
buildSegMapTup ::
  String ->
  Builder GPU ([KernelResult], SegLevel, SegSpace, [Type]) ->
  Builder GPU [SubExp]
buildSegMapTup name m = do
  ((res, lvl, space, ts), stms) <- collectStms m
  let kbody = KernelBody () stms res
  letTupExp' name $ Op $ SegOp $ SegMap lvl space ts kbody

-- Like buildSegMapTup but returns the VName instead of the actual 
-- SubExp. Just for convenience
buildSegMapTup_ ::
  String ->
  Builder GPU ([KernelResult], SegLevel, SegSpace, [Type]) ->
  Builder GPU [VName]
buildSegMapTup_ name m = do
  subExps <- buildSegMapTup name m
  pure $ L.map varFromExp subExps
  where
    varFromExp :: SubExp -> VName
    varFromExp (Var nm) = nm
    varFromExp e = error $ "Expected SubExp of type Var, but got:\n" ++ show e


buildSegMap' ::
  Builder GPU ([KernelResult], SegLevel, SegSpace, [Type]) ->
  Builder GPU (Exp GPU)
buildSegMap' m = do
  ((res, lvl, space, ts), stms) <- collectStms m
  let kbody' = KernelBody () stms res
  pure $ Op $ SegOp $ SegMap lvl space ts kbody'

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
