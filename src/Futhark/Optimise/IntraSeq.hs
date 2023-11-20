{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Use lambda-case" #-}
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
import Data.List as L
import Data.Set as S

import Debug.Pretty.Simple
import Debug.Trace


type SeqM a = ReaderT (Scope GPU) (State VNameSource) a

runSeqM' :: SeqM a -> Scope GPU -> Builder GPU a
runSeqM' m sc = do
  let tmp = runReaderT m sc
  st <- get
  let tmp' = runState tmp st
  pure $ fst tmp'

runSeqM :: SeqM a -> Builder GPU a
runSeqM m = do
  scp <- askScope
  runSeqM' m scp

runSeqMExtendedScope :: SeqM a -> Scope GPU -> Builder GPU a
runSeqMExtendedScope m sc = do
  scp <- askScope
  runSeqM' m (sc <> scp)


-- | A structure for convenient passing of different information needed at 
-- various stages during the pass.
data Env = Env {
  grpId      :: SubExp,             -- The group id
  grpSize    :: SubExp,             -- The group size after seq
  grpsizeOld :: SubExp,             -- The group size before seq
  threadId   :: Maybe VName,        -- the thread id if available at given stage
  nameMap    :: M.Map VName VName,  -- Mapping from arrays to tiles
  seqFactor  :: SubExp
}

setMapping :: Env -> M.Map VName VName -> Env
setMapping (Env gid gSize gSizeOld tid _ factor) mapping =
            Env gid gSize gSizeOld tid mapping factor

updateMapping :: Env -> M.Map VName VName -> Env
updateMapping env mapping =
  let mapping' = mapping `M.union` nameMap env
  in setMapping env mapping'

lookupMapping :: Env -> VName -> Maybe VName
lookupMapping env name
  | M.member name (nameMap env) = do
    case M.lookup name (nameMap env) of
      Just n ->
        case lookupMapping env n of
          Nothing -> Just n
          n' -> n'
      Nothing -> Nothing
lookupMapping _ _ = Nothing

updateEnvTid :: Env -> VName -> Env
updateEnvTid (Env gid sz szo _ tm sq) tid = Env gid sz szo (Just tid) tm sq

getThreadId :: Env -> VName
getThreadId env =
  case threadId env of
    (Just tid ) -> tid
    _ -> error "No tid to get"

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


-- | Matches against singular statements at the group level. That is statements
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

-- Catch all pattern. This will mainly just tell us if we encounter some
-- statement in a test program so that we know that we will have to handle it
seqStm stm = addStm stm


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
  (_, stms') <- collectStms $ mapM (seqStm' env) stms
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

  -- thread local reduction
  reds <- mkIntmRed env' kbody ts binops
  kbody' <- mkResultKBody env' kbody reds

  -- Update remaining types
  let numResConsumed = numArgsConsumedBySegop binops
  let space' = SegSpace (segFlat space) [(fst $ head $ unSegSpace space, grpSize env')]
  tps <- mapM lookupType reds
  let ts' = L.map (stripArray 1) tps
  let (patKeep, patUpdate) = L.splitAt numResConsumed $ patElems pat
  let pat' = Pat $ patKeep ++
        L.map (\(p, t) -> setPatElemDec p t) (zip patUpdate (L.drop numResConsumed tps))

  addStm $ Let pat' aux (Op (SegOp (SegRed lvl space' binops ts' kbody')))


seqStm' env stm@(Let pat _ (Op (SegOp
          (SegMap lvl@(SegThread {}) _ ts kbody@(KernelBody dec _ _)))))
  | isScatter kbody = seqScatter env stm
  | otherwise = do
    usedArrays <- getUsedArraysIn env kbody
    ((kres, space', types'), stms) <- collectStms $ do
      tid <- newVName "tid"
      phys <- newVName "phys_tid"
      let env' = updateEnvTid env tid
      lambSOAC <- buildSOACLambda env' usedArrays kbody ts
      let screma = mapSOAC lambSOAC
      chunks <- mapM (letChunkExp (seqFactor env') tid) usedArrays
      res <- letTupExp' "res" $ Op $ OtherOp $
              Screma (seqFactor env) chunks screma
      let space' = SegSpace phys [(tid, grpSize env)]
      let types' = scremaType (seqFactor env) screma
      let kres = L.map (Returns ResultMaySimplify  mempty) res
      pure (kres, space', types')

    let kbody' = KernelBody dec stms kres
    let names = patNames pat
    letBindNames names $ Op $ SegOp $ SegMap lvl space' types' kbody'


seqStm' env (Let pat aux
            (Op (SegOp (SegScan (SegThread {}) _ binops ts kbody)))) = do
  usedArrays <- getUsedArraysIn env kbody

  -- do local reduction
  reds <- mkIntmRed env kbody ts binops
  let numResConsumed = numArgsConsumedBySegop binops
  let (scanReds, fusedReds) = L.splitAt numResConsumed reds

  -- scan over reduction results
  imScan <- buildSegScan "scan_agg" $ do
    tid <- newVName "tid"
    let env' = updateEnvTid env tid
    phys <- newVName "phys_tid"
    binops' <- renameSegBinOp binops

    let lvl' = SegThread SegNoVirt Nothing
    let space' = SegSpace phys [(tid, grpSize env')]
    results <- mapM (buildKernelResult env') scanReds
    let ts' = L.take numResConsumed ts
    pure (results, lvl', space', binops', ts')

  scans' <- buildSegMapTup_ "scan_res" $ do
    tid <- newVName "tid"
    phys <- newVName "phys_tid"

    let neutrals = L.map segBinOpNeutral binops
    scanLambdas <- mapM (renameLambda . segBinOpLambda) binops

    let scanNames = L.map getVName imScan

    idx <- letSubExp "idx" =<< eBinOp (Sub Int64 OverflowUndef)
                                    (eSubExp $ Var tid)
                                    (eSubExp $ intConst Int64 1)
    nes <- forM neutrals  (\n -> letTupExp' "ne" =<< eIf (eCmpOp (CmpEq $ IntType Int64)
                                  (eSubExp $ Var tid)
                                  (eSubExp $ intConst Int64 0)
                               )
                               (eBody $ L.map toExp n)
                               (eBody $ L.map (\s -> eIndex s [eSubExp idx]) scanNames))

    let env' = updateEnvTid env tid
    lambSOAC <- buildSOACLambda env' usedArrays kbody ts
    let scans = L.map (\(l, n) -> Scan l n) $ zip scanLambdas nes
    let scanSoac = scanomapSOAC scans lambSOAC
    es <- mapM (getChunk env tid (seqFactor env)) usedArrays
    res <- letTupExp' "res" $ Op $ OtherOp $ Screma (seqFactor env) es scanSoac
    let usedRes = L.map (Returns ResultMaySimplify mempty) $ L.take numResConsumed res
    fused <- mapM (buildKernelResult env') fusedReds

    let lvl' = SegThread SegNoVirt Nothing
    let space' = SegSpace phys [(tid, grpSize env)]
    let types' = scremaType (seqFactor env) scanSoac
    pure (usedRes ++ fused, lvl', space', types')

  forM_ (zip (patElems pat) scans') (\(p, s) ->
            let exp' = Reshape ReshapeArbitrary (Shape [grpsizeOld env]) s
            in addStm $ Let (Pat [p]) aux $ BasicOp exp')


-- Need to potentially fix index statements between segops
-- seqStm' env stm@(Let pat aux (BasicOp (Index arr slice))) = do
--   case M.lookup arr (nameMap env) of
--     Nothing -> addStm stm 
--     (Just arr') -> do
--       -- Start by flattening the tile for single use
--       size <- letSubExp "flat_size" =<< eBinOp (Mul Int64 OverflowUndef) 
--                                                (eSubExp $ seqFactor env)
--                                                (eSubExp $ grpSize env)
--       tileFlat <- letExp "flat" $ BasicOp $ Reshape ReshapeArbitrary (Shape [size]) arr'
--       let slice' = Slice $ tail $ unSlice slice
--       addStm $ Let pat aux (BasicOp (Index tileFlat slice'))


-- Catch all
seqStm' _ stm = addStm stm


seqScatter :: Env -> Stm GPU -> Builder GPU ()
seqScatter env (Let pat aux (Op (SegOp
              (SegMap (SegThread {}) _ ts kbody)))) = do

  -- Create the Loop expression
  let (dests, upds) = L.unzip $ L.map (\(WriteReturns _ dest upds ) -> (dest, upds)) (kernelBodyResult kbody)
  loopInit <-
      forM dests $ \d -> do
          tp <- lookupType d
          let decl = toDecl tp Unique
          p <- newParam "loop_param" decl
          pure (p, Var d)


  -- Collect a set of all is and vs used in returns (only the Vars)
  let upds' = L.concatMap (\ (slice, vs) -> do
            let is = L.map (\ dim ->
                              case dim of
                                DimFix d -> d
                                _ -> error "please no"
                  ) (unSlice slice)
            vs : is
          ) $ concat upds
  let upd'' = L.filter (\u ->
            case u of
              Var _ -> True
              _ -> False
        ) upds'
  let updNames = S.fromList $ L.map (\(Var n) -> n) upd''
  -- Intersect it with all pattern names from the kbody
  let names = S.fromList $ L.concatMap (\ (Let pat _ _) ->
            patNames pat
          ) $ kernelBodyStms kbody
  -- The names that should have "producing statements"
  let pStms = S.toList $ S.difference updNames names

  let paramMap = M.fromList $ L.map invert loopInit

  i <- newVName "loop_i"
  let loopForm = ForLoop i Int64 (seqFactor env)

  body <- buildBody_ $ do

      mapRes <- buildSegMapTup "map_res" $ do
          tid <- newVName "write_i"
          phys <- newVName "phys_tid"

          -- size <- mkChunkSize tid env
          offset <- letSubExp "offset" $ BasicOp $
                      BinOp (Mul Int64 OverflowUndef) (Var tid) (seqFactor env)
          tmp <- letSubExp "tmp" $ BasicOp $
                      BinOp (Sub Int64 OverflowUndef) (grpsizeOld env) offset
          size <- letSubExp "size" $ BasicOp $
                      BinOp (SMin Int64) tmp (seqFactor env)
          size' <- letSubExp "size'" =<< eBinOp (Sub Int64 OverflowUndef)
                                                (eSubExp size)
                                                (eSubExp $ intConst Int64 1)
          i' <- letSubExp "loop_i'" $ BasicOp $
                                 BinOp (SMin Int64) size' (Var i)
          idx <- letSubExp "idx" =<< eBinOp (Add Int64 OverflowUndef)
                                            (eSubExp i')
                                            (eSubExp offset)

          -- Modify original statements
          forM_ (kernelBodyStms kbody) $ \ stm -> do
              case stm of
                (Let pat' aux' (BasicOp (Index arr _))) -> do
                    let arr' = M.findWithDefault arr arr (nameMap env)
                    tp' <- lookupType arr'
                    let slice' = case arrayRank tp' of
                                    1 -> Slice [DimFix idx]
                                    2 -> Slice [DimFix $ Var tid, DimFix i']
                                    _ -> error "Scatter more than two dimensions"
                    addStm $ Let pat' aux' (BasicOp (Index arr' slice'))
                stm -> addStm stm

          -- Potentially create more statements and create a mapping from the
          -- original name to the new subExp
          mapping <- forM pStms $ \ nm -> do
              offset <- letSubExp "iota_offset" =<< eBinOp (Mul Int64 OverflowUndef)
                                                           (eSubExp $ Var tid)
                                                           (eSubExp $ seqFactor env)
              val <- letSubExp "iota_val" =<< eBinOp (Add Int64 OverflowUndef)
                                                         (eSubExp offset)
                                                         (eSubExp i')
              pure (Var nm, val)
          let valMap = M.fromList mapping


          -- Update the original WriteReturns to target the loop params instead
          res' <- forM (kernelBodyResult kbody) $ \ res -> do
              case res of
                  (WriteReturns _ dest upd) -> do
                      let (Just destParam) = M.lookup (Var dest) paramMap
                      let dest' = paramName destParam
                      let upd' = L.map (mapUpdates valMap) upd
                      pure $ WriteReturns mempty dest' upd'
                  _ -> error "Expected WriteReturns in scatter"

          -- Return the results of the update statements form the segmap
          let lvl' = SegThread SegNoVirt Nothing
          let space' = SegSpace phys [(tid, grpSize env)]
          -- let res' = L.map (Returns ResultMaySimplify mempty) updates
          pure (res', lvl', space', ts)


      -- Return the results from the segmap from the loop
      let res = L.map (SubExpRes mempty) mapRes
      pure res

  -- Construct the final loop
  let loopExp = Loop loopInit loopForm body

  addStm $ Let pat aux loopExp

  -- End
  pure ()
  where
    invert (a,b) = (b,a)

    mapUpdates :: M.Map SubExp SubExp -> (Slice SubExp, SubExp) -> (Slice SubExp, SubExp)
    mapUpdates mapping (Slice dims, vs) = do
      let vs' = M.findWithDefault vs vs mapping
      let dims' = L.map (\d ->
            case d of 
              DimFix d' -> DimFix $ M.findWithDefault d' d' mapping  
              d' -> d' -- should never happen
            ) dims
      (Slice dims', vs')


seqScatter _ stm = error $
                  "SeqScatter error. Should be a map at thread level but got"
                  ++ show stm

buildSOACLambda :: Env -> [VName] -> KernelBody GPU -> [Type] -> Builder GPU (Lambda GPU)
buildSOACLambda env usedArrs kbody retTs = do
  ts <- mapM lookupType usedArrs
  let ts' = L.map (Prim . elemType) ts
  params <- mapM (newParam "par" ) ts'
  let mapNms = L.map paramName params
  let env' = updateMapping env $ M.fromList $ zip usedArrs mapNms
  kbody' <- runSeqMExtendedScope (seqKernelBody' env' kbody) (scopeOfLParams params)
  let body = kbodyToBody kbody'
  renameLambda $
    Lambda
    { lambdaParams = params,
      lambdaBody = body,
      lambdaReturnType = retTs
    }

getVName :: SubExp -> VName
getVName (Var name) = name
getVName e = error $ "SubExp is not of type Var in getVName:\n" ++ show e

getTidIndexExp :: Env -> VName -> Builder GPU (Exp GPU)
getTidIndexExp env name = do
  tp <- lookupType name
  let outerDim = [DimFix $ Var $ getThreadId env]
  let index =
        case arrayRank tp of
          0 -> SubExp $ Var name
          1 -> Index name $ Slice outerDim
          2 -> Index name $ Slice $
                outerDim ++ [DimSlice (intConst Int64 0) (seqFactor env) (intConst Int64 1)]
          _ -> error "Arrays are not expected to have more than 2 dimensions \n"
  pure $ BasicOp index

buildKernelResult :: Env -> VName -> Builder GPU KernelResult
buildKernelResult env name = do
  i <- getTidIndexExp env name
  res <- letSubExp "res" i
  pure $ Returns ResultMaySimplify mempty res

mkResultKBody :: Env -> KernelBody GPU -> [VName] -> Builder GPU (KernelBody GPU)
mkResultKBody env (KernelBody dec _ _) names = do
  (res, stms) <- collectStms $ do mapM (buildKernelResult env) names
  pure $ KernelBody dec stms res



numArgsConsumedBySegop :: [SegBinOp GPU] -> Int
numArgsConsumedBySegop binops =
  let numResUsed = L.foldl
                    (\acc (SegBinOp _ (Lambda pars _ _) neuts _)
                      -> acc + length pars - length neuts) 0 binops
  in numResUsed

seqKernelBody' ::
  Env ->
  KernelBody GPU ->
  SeqM (KernelBody GPU)
seqKernelBody' env (KernelBody dec stms results) = do
  stms' <- seqStms'' env stms
  pure $ KernelBody dec stms' results

seqStms'' ::
  Env ->
  Stms GPU ->
  SeqM (Stms GPU)
seqStms'' env stms = do
  (stms', _) <- foldM (\(ss, env') s -> do
      (env'', ss') <- runBuilder $ localScope (scopeOf ss <> scopeOf s) $ seqStm'' env' s
      pure (ss <> ss', env'')
      ) (mempty, env) (stmsToList stms)
  pure stms'

seqStm'' ::
  Env ->
  Stm GPU ->
  Builder GPU Env
seqStm'' env stm@(Let pat aux (BasicOp (Index arr _))) =
  case lookupMapping env arr of
    Just name -> do
      i <- getTidIndexExp env name
      addStm $ Let pat aux i
      pure env
    Nothing -> do
      addStm stm
      pure env
seqStm'' env stm = do
  addStm stm
  pure env

-- create the intermediate reduction used in scan and reduce
mkIntmRed ::
  Env ->
  KernelBody GPU ->
  [Type] ->                   -- segmap return types
  [SegBinOp GPU] ->
  Builder GPU [VName]
mkIntmRed env kbody retTs binops = do
    let ne   = L.map segBinOpNeutral binops
    lambda <- mapM (renameLambda . segBinOpLambda) binops

    buildSegMapTup_ "red_intermediate" $ do
      tid <- newVName "tid"
      let env' = updateEnvTid env tid
      phys <- newVName "phys_tid"
      sz <- mkChunkSize tid env
      usedArrs <- getUsedArraysIn env kbody
      lambSOAC <- buildSOACLambda env' usedArrs kbody retTs
      -- TODO analyze if any fused maps then produce reduce?
      -- we build the reduce as a scan initially
      let scans = L.map (\(l, n) -> Scan l n) $ zip lambda ne
      let screma = scanomapSOAC scans lambSOAC
      chunks <- mapM (getChunk env tid (seqFactor env)) usedArrs

      res <- letTupExp' "res" $ Op $ OtherOp $
                Screma (seqFactor env) chunks screma
      let numRes = numArgsConsumedBySegop binops
      let (scanRes, mapRes) = L.splitAt numRes res

      -- get the reduction result from the scan
      redIndex <- letSubExp "red_index" =<< eBinOp (Sub Int64 OverflowUndef)
                                                   (eSubExp sz)
                                                   (eSubExp $ intConst Int64 1)
      redRes <- forM scanRes
            (\r -> letSubExp "red_res" $ BasicOp $ Index (getVName r) (Slice [DimFix redIndex]))
      let res' = redRes ++ mapRes
      let lvl' = SegThread SegNoVirt Nothing
      let space' = SegSpace phys [(tid, grpSize env)]
      let kres = L.map (Returns ResultMaySimplify mempty) res'
      types' <- mapM subExpType res'
      pure (kres, lvl', space', types')

getUsedArraysIn ::
  Env ->
  KernelBody GPU ->
  Builder GPU [VName]
getUsedArraysIn env kbody = do
  scope <- askScope
  let (arrays, _) = unzip $ M.toList $ M.filter isArray scope
  let free = IM.elems $ namesIntMap $ freeIn kbody
  let freeArrays = arrays `intersect` free
  let arrays' =
        L.map ( \ arr ->
          if M.member arr (nameMap env) then
            let (Just tile) = M.lookup arr (nameMap env)
            in tile
          else arr
          ) freeArrays
  pure arrays'


getChunk ::
  Env ->
  VName ->              -- thread Id
  SubExp ->             -- size of chunk
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


kbodyToBody :: KernelBody GPU -> Body GPU
kbodyToBody (KernelBody dec stms res) =
  let res' = L.map (subExpRes . kernelResultSubExp) res
  in Body
    { bodyDec = dec,
      bodyStms = stms,
      bodyResult = res'
    }


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

renameSegBinOp :: [SegBinOp GPU] -> Builder GPU [SegBinOp GPU]
renameSegBinOp segbinops =
  forM segbinops $ \(SegBinOp comm lam ne shape) -> do
    lam' <- renameLambda lam
    pure $ SegBinOp comm lam' ne shape


letChunkExp :: SubExp -> VName -> VName -> Builder GPU VName
letChunkExp sz tid arrName = do
  letExp "chunk" $ BasicOp $
    Index arrName (Slice [DimFix (Var tid),
    DimSlice (intConst Int64 0) sz (intConst Int64 1)])


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
  let arrsInScope = M.toList $ M.filter isArray scope

  tileSize <- letSubExp "tile_size" =<< eBinOp (Mul Int64 OverflowUndef)
                                               (eSubExp $ seqFactor env)
                                               (eSubExp $ grpSize env)

  tiles <- forM arrsInScope $ \ (arrName, arrInfo) -> do
    let tp = elemType $ typeOf arrInfo

    tileScratch <- letExp "tile_scratch" $ BasicOp $
                      Scratch tp [tileSize]

    let outerDim = ([DimFix $ grpId env | arrayRank (typeOf arrInfo) > 1])
    let sliceIdx = DimSlice (intConst Int64 0) (grpsizeOld env) (intConst Int64 1)

    tileSlice <- letSubExp "tile_slice" $ BasicOp $
                      Index arrName (Slice $ outerDim ++ [sliceIdx])

    tileStaging <- letExp "tile_staging" $ BasicOp $
                      Update Unsafe tileScratch
                        (Slice [DimSlice (intConst Int64 0)
                                         (grpsizeOld env)
                                         (intConst Int64 1)]
                        ) tileSlice

    -- Now read the chunks using a segmap
    let (VName n _) = arrName
    tile <- buildSegMap_ ("tile_" ++ nameToString n) $ do
      tid <- newVName "tid"
      phys <- newVName "phys"

      start <- letSubExp "start" =<< eBinOp (Mul Int64 OverflowUndef)
                                            (eSubExp $ Var tid)
                                            (eSubExp $ seqFactor env)
      let slice = Slice [DimSlice start (seqFactor env) (intConst Int64 1)]
      chunk <- letSubExp "chunk" $ BasicOp $ Index tileStaging slice

      let lvl = SegThread SegNoVirt Nothing
      let space = SegSpace phys [(tid, grpSize env)]
      let types = [Array tp (Shape [seqFactor env]) NoUniqueness]
      let res = [Returns ResultPrivate mempty chunk]
      pure (res, lvl, space, types)


    pure (arrName, tile)

  pure $ setMapping env (M.fromList tiles)

-- mkTiles ::
--   Env ->
--   Builder GPU Env
-- mkTiles env = do
--   scope <- askScope
--   let arrsInScope = M.toList $  M.filter isArray scope

--   scratchSize <- letSubExp "tile_size" =<< eBinOp (Mul Int64 OverflowUndef)
--                                                (eSubExp $ seqFactor env)
--                                                (eSubExp $ grpSize env)


--   tiles <- forM arrsInScope $ \ (arrName, arrInfo) -> do
--     let tp = elemType $ typeOf arrInfo

--     -- The array to save the tile in
--     scratch <- letExp "tile_scratch" $ BasicOp $ Scratch tp [scratchSize]

--     -- Build SegMap that will write to tile
--     tile <- buildSegMap_ "tile_staging" $ do
--       tid <- newVName "tid"
--       phys <- newVName "phys_tid"

--       -- Allocate local scratch chunk
--       chunk <- letExp "chunk_scratch" $ BasicOp $ Scratch tp [seqFactor env]

--       -- Compute the chunk size of the current thread. Last thread might need to read less
--       -- sliceSize <- mkChunkSize tid env
--       tmp <- letSubExp "tmp" =<< eBinOp (Sub Int64 OverflowUndef)
--                                         (eSubExp $ grpsizeOld env)
--                                         (eSubExp $ Var tid)
--       sliceSize <- letSubExp "slice_size" =<< eBinOp (SDivUp Int64 Unsafe)
--                                                      (eSubExp tmp)
--                                                      (eSubExp $ grpSize env)

--       let outerDim = ([DimFix $ grpId env | arrayRank (typeOf arrInfo) > 1])
--       let sliceIdx = DimSlice (Var tid) sliceSize (grpSize env)
--       vals <- letSubExp "slice" $ BasicOp $ Index arrName
--                                   (Slice $ outerDim ++ [sliceIdx])

--       -- Update the chunk
--       chunk' <- letSubExp "chunk" $ BasicOp $ Update Unsafe chunk
--                                     (Slice [DimSlice (intConst Int64 0) sliceSize (intConst Int64 1)]) vals

--       let lvl = SegThread SegNoVirt Nothing
--       let space = SegSpace phys [(tid, grpSize env)]
--       -- let types = [Array tp (Shape [seqFactor env]) NoUniqueness]
--       let types = [Array tp (Shape [scratchSize]) NoUniqueness]

--       -- 
--       -- start <- letSubExp "start" =<< eBinOp (Mul Int64 OverflowUndef)
--       --                                       (eSubExp $ Var tid)
--       --                                       (eSubExp $ grpSize env)
--       let slice = Slice [DimSlice (Var tid) (seqFactor env) (grpSize env)]
--       let res = [WriteReturns mempty scratch [(slice, chunk')]]

--       pure (res, lvl, space, types)

--     -- transpose and flatten
--     -- tileT <- letExp "tileT" $ BasicOp $ Rearrange [1,0] tile
--     -- tileFlat <- letExp "tile_flat" $ BasicOp $ Reshape
--     --             ReshapeArbitrary (Shape [scratchSize]) tileT

--     -- Now each thread will read their actual chunk to registers
--     let (VName n _) = arrName
--     tile' <- buildSegMap_ ("tile_" ++ nameToString n) $ do
--       tid <- newVName "tid"
--       phys <- newVName "phys_tid"

--       start <- letSubExp "start" =<< eBinOp (Mul Int64 OverflowUndef)
--                                             (eSubExp $ Var tid)
--                                             (eSubExp $ seqFactor env)
--       -- NOTE: Can just use seqFactor here as we read from the padded tile craeted above
--       let dimSlice = DimSlice start (seqFactor env) (intConst Int64 1)

--       chunk <- letSubExp "chunk" $ BasicOp $ Index tile
--                                     (Slice [dimSlice])
--       let lvl = SegThread SegNoVirt Nothing
--       let space = SegSpace phys [(tid, grpSize env)]
--       let types = [Array tp (Shape [seqFactor env]) NoUniqueness]
--       pure ([Returns ResultPrivate mempty chunk], lvl, space, types)

--     pure (arrName, tile')

--   pure $ setMapping env (M.fromList tiles)

isArray :: NameInfo GPU -> Bool
isArray info = arrayRank (typeOf info) > 0

-- | Checks if a kernel body ends in only WriteReturns results as then it
-- must be the body of a scatter
isScatter :: KernelBody GPU -> Bool
isScatter (KernelBody _ _ res) =
  L.all isWriteReturns res
  where
    isWriteReturns (WriteReturns {}) = True
    isWriteReturns _ = False

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

-- buildSegMapTup' ::
--   String -> 
--   Builder GPU ([KernelResult], SegLevel, SegSpace, [Type]) ->
--   Builder GPU (Exp GPU)
-- buildSegMapTup' name m = do undefined


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

