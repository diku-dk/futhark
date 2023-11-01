{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use uncurry" #-}
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
import Debug.Trace
import Text.Pretty.Simple
import qualified Control.Monad




type SeqM a = ReaderT (Scope GPU) (State VNameSource) a



runSeqM :: SeqM a -> Builder GPU a
runSeqM m = do
  scp <- askScope
  let tmp = runReaderT m scp
  st <- get
  let tmp' = runState tmp st
  pure $ fst tmp'



-- | A structure for convenient passing of different information needed at 
-- various stages during the pass.
data Env = Env {
  grpId      :: SubExp,             -- The group id
  grpSize    :: SubExp,             -- The group size after seq
  grpsizeOld :: SubExp,             -- The group size before seq
  threadId   :: Maybe VName,        -- the thread id if available at given stage
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

  let env = Env (Var grpId) sizeNew sizeOld Nothing mempty e

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

  let tid = fst $ head $ unSegSpace space
  let env' = updateEnvTid env tid

  (orgArrays, usedArrays) <- arraysInScope env kbody

  -- For each SegBinOp we should create an intermediate reduction result
  reds <- mapM (mkSegMapRed env' (orgArrays, usedArrays) kbody ts) binops

  -- Pair the original arrays with the intermediate results
  -- TODO head in reds until multiple binops are supported
  let redExps = L.map (\r -> BasicOp $ Index r $ Slice [DimFix $ Var tid]) $ head reds
  -- let (arrs, tiles) = unzip $ M.toList $ tileMap env
  -- let mapping = M.fromList $ zip arrs redExps <> zip tiles redExps
  let mapping = M.fromList $ zip orgArrays redExps
  -- let mapping = M.fromList $ zip (trace (show usedArrays) orgArrays) redExps
  -- let mapping = M.fromList $ zipWith (curry (\((a,_), r) -> (a,r))) usedArrays redExps

  -- Modify the kernel body
  -- TODO: Assumes single dimensionfo
  kbody' <- runSeqM $ seqSegRedKernelBody env' mapping kbody
  let numResConsumed = numArgsConsumedBySegop binops
  let fusedReds = L.drop numResConsumed $ head reds
  kbody'' <- addMapResultsToKbody env' kbody' fusedReds numResConsumed

  -- Update remaining types
  let space' = SegSpace (segFlat space) [(fst $ head $ unSegSpace space, grpSize env')]
  -- TODO: binop head
  tps <- mapM lookupType $ head reds
  let ts' = L.map (stripArray 1) tps
  let (patKeep, patUpdate) = L.splitAt numResConsumed $ patElems pat
  let pat' = Pat $ patKeep ++ L.map (\(p, t) -> setPatElemDec p t) (zip patUpdate (L.drop numResConsumed tps))

  addStm $ Let pat' aux (Op (SegOp (SegRed lvl space' binops ts' kbody'')))

seqStm' env (Let pat aux (Op (SegOp
          (SegMap lvl@(SegThread {}) space ts kbody)))) = do
  let tid = fst $ head $ unSegSpace space
  let env' = updateEnvTid env tid

  (orgArrays, usedArrays) <- arraysInScope env kbody

  maps <- buildSegMapTup_ "map_intermediate" $ do
    tidName <- newVName "tid"
    phys <- newVName "phys_tid"
    screma <- buildMapScrema orgArrays
    chunks <- mapM (letChunkExp (seqFactor env) tidName) usedArrays
    res <- letTupExp' "res" $ Op $ OtherOp $
            Screma (seqFactor env) chunks screma
    let lvl' = SegThread SegNoVirt Nothing
    let space' = SegSpace phys [(tidName, grpSize env)]
    let types' = scremaType (seqFactor env) screma
    let kres = L.map (Returns ResultPrivate mempty) res
    pure (kres, lvl', space', types')

  -- let mapExps = L.map (\m -> BasicOp $ Index m $ Slice [DimFix $ Var tid]) maps
  -- let mapping = M.fromList $ zip orgArrays mapExps
  -- kbody' <- runSeqM $ seqKernelBody' env' mapping kbody
  -- TODO: change name of addMapResultsToKbody here it is not because fused
  kbody' <- addMapResultsToKbody env' kbody maps 0

  let space' = SegSpace (segFlat space) [(fst $ head $ unSegSpace space, grpSize env')]
  tps <- mapM lookupType maps
  let ts' = L.map (stripArray 1) tps
  let pat' = Pat $ L.map (\(p, t) -> setPatElemDec p t) (zip (patElems pat) tps)
  addStm $ Let pat' aux (Op (SegOp (SegMap lvl space' ts' kbody')))

  where
    buildMapScrema :: [VName] -> Builder GPU (ScremaForm GPU)
    buildMapScrema orgArrs = do
      params <- mapM (newParam "par") ts
      let mapExps = L.map (BasicOp . SubExp . Var . paramName ) params
      let mapping' = M.fromList $ zip orgArrs mapExps
      kbody' <- runSeqM $ seqKernelBody' env mapping' kbody
      let body = kbodyToBody kbody'
      lamb <- renameLambda $
                Lambda
                { lambdaParams = params,
                  lambdaBody = body,
                  lambdaReturnType = ts
                }
      pure $ mapSOAC lamb


seqStm' env (Let pat aux
            (Op (SegOp (SegScan (SegThread {}) _ binops ts kbody)))) = do

  (orgArrays, usedArrays) <- arraysInScope env kbody

  reds <- mapM (mkSegMapRed env (orgArrays, usedArrays) kbody ts) binops
  -- TODO: head until multiple binops
  let redshead = head reds
  scan <- buildSegScan "scan_agg" $ do
    tid <- newVName "tid"
    phys <- newVName "phys_tid"
    binops' <- renameSegBinOp binops

    e' <- forM redshead $ \ r -> do
      letSubExp "elem" $ BasicOp $ Index r (Slice [DimFix $ Var tid])

    -- map (\ x -> eIndex x (eSubExp $ Var tid)) redshead

    let lvl' = SegThread SegNoVirt Nothing
    let space' = SegSpace phys [(tid, grpSize env)]
    let results = L.map (Returns ResultMaySimplify mempty) e'
    pure (results, lvl', space', binops', ts)

  scans' <- buildSegMapTup_ "scan_res" $ do
    tid <- newVName "tid"
    phys <- newVName "phys_tid"

    -- TODO: Uses head
    let binop = head binops
    let neutral = segBinOpNeutral binop
    lambda <- renameLambda $ segBinOpLambda binop

    let scanNames = L.map getVName scan

    idx <- letSubExp "idx" =<< eBinOp (Sub Int64 OverflowUndef)
                                    (eSubExp $ Var tid)
                                    (eSubExp $ intConst Int64 1)
    ne <- letTupExp' "ne" =<< eIf (eCmpOp (CmpEq $ IntType Int64)
                                  (eSubExp $ Var tid)
                                  (eSubExp $ intConst Int64 0)
                               )
                               (eBody $ L.map toExp neutral)
                               (eBody $ L.map (\s -> eIndex s (eSubExp idx)) scanNames)
                              --  (eBody [toExp neutral])
                              --  (eBody [eIndex (head scanNames) (eSubExp idx)])

    scanSoac <- scanSOAC [Scan lambda ne]
    -- es <- letChunkExp (seqFactor env) tid (snd $ head $ M.toList $ tileMap env)
    -- sz <- mkChunkSize tid env
    es <- mapM (getChunk env tid (seqFactor env)) usedArrays
    res <- letTupExp' "res" $ Op $ OtherOp $ Screma (seqFactor env) es scanSoac

    let lvl' = SegThread SegNoVirt Nothing
    let space' = SegSpace phys [(tid, grpSize env)]
    let types' = scremaType (seqFactor env) scanSoac
    let returns = L.map (Returns ResultMaySimplify mempty) res
    pure (returns, lvl', space', types')

  -- TODO first head mult binops
  -- let exp' = Reshape ReshapeArbitrary (Shape [grpsizeOld env]) (head scans')
  -- addStm $ Let pat aux $ BasicOp exp'
  -- exps <- mapM (eSubExp . Var) scans'

  --scans'' <- mapM eSubExp scans'
  forM_ (zip (patElems pat) scans') (\(p, s) ->
            let exp' = Reshape ReshapeArbitrary (Shape [grpsizeOld env]) s
            in addStm $ Let (Pat [p]) aux $ BasicOp exp')


  --exp <- letTupExp "tmp" $ 
 -- addStm $ Let pat aux 



-- Catch all
seqStm' _ stm = error $
                "Encountered unhandled statement at thread level: " ++ show stm

getVName :: SubExp -> VName
getVName (Var name) = name
getVName e = error $ "SubExp is not of type Var in getVName:\n" ++ show e

-- resConsumed: num of kernels results used in the segop
addMapResultsToKbody :: Env -> KernelBody GPU -> [VName] -> Int -> Builder GPU (KernelBody GPU)
addMapResultsToKbody env (KernelBody dec stms res) names resConsumed =
  case threadId env of
    Just tid -> do
      (resExp, stms') <- collectStms $ do
        let slice = Slice [DimFix $ Var tid, DimSlice (intConst Int64 0) (seqFactor env) (intConst Int64 1)]
        mapM (\n -> letSubExp "fused" $ BasicOp $ Index n slice) names
      let res' = L.take resConsumed res ++
                  L.map (Returns ResultMaySimplify mempty) resExp
      pure $ KernelBody dec (stms <> stms') res'
    Nothing -> error "threadId required to add fused statements, but none given"

numArgsConsumedBySegop :: [SegBinOp GPU] -> Int
numArgsConsumedBySegop binops =
  let numResUsed = L.foldl
                    (\acc (SegBinOp _ (Lambda pars _ _) neuts _)
                      -> acc + length pars - length neuts) 0 binops
  in numResUsed

seqKernelBody' ::
  Env ->
  Map VName (Exp GPU) -> -- mapping from global array to intermediate results
  KernelBody GPU ->
  SeqM (KernelBody GPU)
seqKernelBody' env mapping (KernelBody dec stms results) = do
  stms' <- seqStms'' env mapping stms
  pure $ KernelBody dec stms' results

seqStms'' ::
  Env ->
  Map VName (Exp GPU) ->
  Stms GPU ->
  SeqM (Stms GPU)
seqStms'' env mapping stms = do
  foldM (\ss s -> do
      ss' <- runBuilder_ $ localScope (scopeOf ss <> scopeOf s) $ seqStm'' env mapping s
      pure $ ss <> ss'
      ) mempty (stmsToList stms)

seqStm'' ::
  Env ->
  Map VName (Exp GPU) ->
  Stm GPU ->
  Builder GPU ()
seqStm'' _ mapping stm@(Let pat aux (BasicOp (Index arr _))) =
  if M.member arr mapping then do
    let (Just op) = M.lookup arr mapping
    addStm $ Let pat aux op
  else
    addStm stm
seqStm'' _ _ stm = addStm stm

seqSegRedKernelBody ::
  Env ->
  Map VName (Exp GPU) -> -- mapping from global array to intermediate results
  KernelBody GPU ->
  SeqM (KernelBody GPU)
seqSegRedKernelBody env mapping (KernelBody dec stms results) = do
  (nms, stms') <- seqSegRedStms env mapping stms
  -- TODO: IS THIS CORRECT??
  let results' = L.map (\(Returns m c _, n) -> Returns m c (Var n)) $ zip results nms
  pure $ KernelBody dec stms' results'

seqSegRedStms ::
  Env ->
  Map VName (Exp GPU) ->
  Stms GPU ->
  SeqM ([VName], Stms GPU)
seqSegRedStms env mapping stms = do
  foldM (\(nms, ss) s -> do
      (nms', ss') <- runBuilder $ localScope (scopeOf ss <> scopeOf s) $ seqSegRedStm env mapping s
      pure (nms ++ nms', ss <> ss')
      ) ([], mempty) (stmsToList stms)

seqSegRedStm ::
  Env ->
  Map VName (Exp GPU) ->
  Stm GPU ->
  Builder GPU [VName]
seqSegRedStm _ mapping (Let pat aux (BasicOp (Index arr _))) =
  if M.member arr mapping then do
    let (Just op) = M.lookup arr mapping
    addStm $ Let pat aux op
    let pNames = L.map patElemName $ patElems pat
    pure pNames
  else pure []
seqSegRedStm _ _ _ = pure []

mkSegMapRed ::
  Env ->
  ([VName],[VName]) ->
  KernelBody GPU ->
  [Type] ->                   -- segmap return types
  SegBinOp GPU ->
  Builder GPU [VName]
mkSegMapRed env (orgArrs, usedArrs) kbody retTs binop = do
    let comm = segBinOpComm binop
    let ne   = segBinOpNeutral binop
    lambda <- renameLambda $ segBinOpLambda binop

    buildSegMapTup_ "red_intermediate" $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"
      sz <- mkChunkSize tid env
      screma <- buildRedoMap [Reduce comm lambda ne]
      -- For each tile we can then get the corresponding chunk
      chunks <- mapM (getChunk env tid sz) usedArrs


      -- expandArr is only because we might need scratch arrays that are not used for reductions
      -- const (0 :: Int64) is thus just a dummy
      let chunkElems = expandArr ne (constant (0 :: Int64)) (length usedArrs - length ne)
      chunksScratch <- mapM (letExp "chunk_scratch" . BasicOp .
                              Replicate (Shape [seqFactor env])) chunkElems
      chunks' <- mapM (\(scratch, chunk) ->
        letExp "chunk" $ BasicOp $ Update Unsafe scratch
          (Slice [DimSlice (intConst Int64 0) sz (intConst Int64 1)])
          $ Var chunk) $ L.zip chunksScratch chunks
      res <- letTupExp' "res" $ Op $ OtherOp $
                Screma (seqFactor env) chunks' screma

      let lvl' = SegThread SegNoVirt Nothing
      let space' = SegSpace phys [(tid, grpSize env)]
      let types' = scremaType (seqFactor env) screma
      let kres = L.map (Returns ResultPrivate mempty) res
      pure (kres, lvl', space', types')
  where
    buildRedoMap :: [Reduce GPU] -> Builder GPU (ScremaForm GPU)
    buildRedoMap reds = do
      ts <- mapM lookupType usedArrs
      let ts' = L.map (Prim . elemType) ts
      params <- mapM (newParam "par" ) ts'
      let mapExps = L.map (BasicOp . SubExp . Var . paramName ) params
      let mapping' = M.fromList $ zip orgArrs mapExps

      if not $ kernelNeeded kbody mapping' then do
        reduceSOAC reds
      else do
        kbody' <- runSeqM $ seqKernelBody' env mapping' kbody
        let body = kbodyToBody kbody'
        lamb <- renameLambda $
                    Lambda
                    { lambdaParams = params,
                      lambdaBody = body,
                      lambdaReturnType = retTs
                    }
        pTrace (show (zip orgArrs usedArrs )) (pure $ redomapSOAC reds lamb)


kernelNeeded ::
  KernelBody GPU ->
  Map VName (Exp GPU) ->
  Bool
kernelNeeded (KernelBody _ stms _) mapping = needed $ stmsToList stms
   where
    needed [] = False
    needed ((Let _ _ (BasicOp (Index arr _))):t) =
       not (M.member arr mapping) || needed t
    needed _ = True



getChunk ::
  Env ->
  VName ->              -- thread Id
  SubExp ->             -- size of chunk
  -- (VName, VName) ->     -- array to tile mapping
  VName ->              -- Array to get chunk from
  Builder GPU VName
getChunk env tid sz arr = do
  tp <- lookupType arr

  offset <- letSubExp "offset" =<< eBinOp (Mul Int64 OverflowUndef)
                                          (eSubExp $ seqFactor env)
                                          (eSubExp $ Var tid)

  let dims =
        case arrayRank tp of
          1 -> [DimSlice offset sz (intConst Int64 1)]
          2 -> [DimFix $ Var tid, DimSlice (intConst Int64 0) sz (intConst Int64 1)]
          _ -> error "unhandled dims in getChunk"

  letExp "chunk" $ BasicOp $ Index arr (Slice dims)

flattenResults ::
  Pat (LetDec GPU)->
  [KernelResult] ->
  Builder GPU [KernelResult]
flattenResults pat kresults = do
  subExps <- forM (zip kresults $ patTypes pat) $ \(res, tp)-> do
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


-- | Creates a tile for each array in scope at the time of caling it.
-- That is if called at the correct time it will create a tile for each
-- global array
mkTiles ::
  Env ->
  Builder GPU Env
mkTiles env = do
  scope <- askScope
  let arraysInScope = M.toList $  M.filter isArray scope

  scratchSize <- letSubExp "tile_size" =<< eBinOp (Mul Int64 OverflowUndef)
                                               (eSubExp $ seqFactor env)
                                               (eSubExp $ grpSize env)

  tiles <- forM arraysInScope $ \ (arrName, arrInfo) -> do
    let tp = elemType $ typeOf arrInfo
    -- tile <- letExp "tile" $ BasicOp $ Scratch tp [scratchSize]

    -- Build SegMap that will write to tile
    tile <- buildSegMap_ "tile_staging" $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"

      -- Allocate local scratch chunk
      chunk <- letExp "chunk_scratch" $ BasicOp $ Scratch tp [seqFactor env]

      -- Compute the chunk size of the current thread. Last thread might need to read less
      sliceSize <- mkChunkSize tid env
      let outerDim = ([DimFix $ grpId env | arrayRank (typeOf arrInfo) > 1])
      let sliceIdx = DimSlice (Var tid) sliceSize (grpSize env)
      slice <- letSubExp "slice" $ BasicOp $ Index arrName
                                  (Slice $ outerDim ++ [sliceIdx])

      -- Update the chunk
      chunk' <- letSubExp "chunk" $ BasicOp $ Update Unsafe chunk
                                    (Slice [DimSlice (intConst Int64 0) sliceSize (intConst Int64 1)]) slice

      -- let shape = Shape [grpSize env]

      let lvl = SegThread SegNoVirt Nothing
      let space = SegSpace phys [(tid, grpSize env)]
      let types = [Array tp (Shape [seqFactor env]) NoUniqueness]
      pure ([Returns ResultMaySimplify mempty chunk'], lvl, space, types)
      -- pure ([WriteReturns mempty shape tile [(Slice [sliceIdx], chunk')]], lvl, space, types)

    -- transpose and flatten
    tileT <- letExp "tileT" $ BasicOp $ Rearrange [1,0] tile
    tileFlat <- letExp "tile_flat" $ BasicOp $ Reshape
                ReshapeArbitrary (Shape [scratchSize]) tileT

    -- Now each thread will read their actual chunk
    tile' <- buildSegMap_ "tile" $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"

      start <- letSubExp "start" =<< eBinOp (Mul Int64 OverflowUndef)
                                            (eSubExp $ Var tid)
                                            (eSubExp $ seqFactor env)
      -- NOTE: Can jsut use seqFactor here as we read from the padded tile craeted above
      let dimSlice = DimSlice start (seqFactor env) (intConst Int64 1)

      chunk <- letSubExp "chunk" $ BasicOp $ Index tileFlat
                                    (Slice [dimSlice])
      let lvl = SegThread SegNoVirt Nothing
      let space = SegSpace phys [(tid, grpSize env)]
      let types = [Array tp (Shape [seqFactor env]) NoUniqueness]
      pure ([Returns ResultPrivate mempty chunk], lvl, space, types)

    pure (arrName, tile')

  pure $ (\(Env gid gSize gSizeOld tid _ factor) ->
            Env gid gSize gSizeOld tid (M.fromList tiles) factor) env

isArray :: NameInfo GPU -> Bool
isArray info = arrayRank (typeOf info) > 0

-- | Returns the VName of all arrays in scope that are also free variable in
-- the kernel body
arraysInScope ::
  Env ->
  KernelBody GPU ->
  Builder GPU ([VName], [VName])
arraysInScope env kbody = do
  scope <- askScope
  let (arrays, _) = unzip $ M.toList $  M.filter isArray scope

  let free = IM.elems $ namesIntMap $ freeIn kbody

  let freeArrays = arrays `intersect` free

  -- For each array in arrays see if it has been tiled 
  let arrays' =
        L.map ( \ arr ->
          if M.member arr (tileMap env) then
            let (Just tile) = M.lookup arr (tileMap env)
            in tile
          else
            arr
        ) freeArrays

  pure (arrays, arrays')

  -- let free = IM.elems $ namesIntMap $ freeIn kbody
  -- let tiles = M.toList $ tileMap env
  -- let (used, tiles') = unzip $ intersectBy (\(a,_)(b,_) -> a == b) tiles (zip free free)
  -- let free' = free L.\\ used

  -- -- Only need the arrays from free'
  -- infos <- mapM lookupInfo free'
  -- let (free'', _) = unzip $ L.filter (\(_, i) -> isArray i) $ zip free' infos

  -- pure $ free'' <> tiles'


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
  Builder GPU [SubExp]
buildSegScan name m = do
  ((results, lvl, space, bops, ts), stms) <- collectStms m
  let kbody = KernelBody () stms results
  letTupExp' name $ Op $ SegOp $ SegScan lvl space bops ts kbody

-- HELPERS
kbodyToBody :: KernelBody GPU -> Body GPU
kbodyToBody (KernelBody dec stms res) =
  let res' = L.map (subExpRes . kernelResultSubExp) res
  in Body
    { bodyDec = dec,
      bodyStms = stms,
      bodyResult = res'
    }

updateEnvTid :: Env -> VName -> Env
updateEnvTid (Env gid sz szo _ tm sq) tid = Env gid sz szo (Just tid) tm sq

expandArr :: [a] -> a -> Int -> [a]
expandArr arr ele num = arr ++ replicate num ele
