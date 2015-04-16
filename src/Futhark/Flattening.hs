{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, ScopedTypeVariables #-}
module Futhark.Flattening ( flattenProg )
  where

import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List as L

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.Basic
import Futhark.Substitute

--------------------------------------------------------------------------------

data FlatState = FlatState {
    vnameSource   :: VNameSource
  , mapLetArrays   :: M.Map VName Ident
  -- ^ arrays for let values in maps
  --
  -- @let res = map (\xs -> let y = reduce(+,0,xs) in
  --                        let z = iota(y) in z, xss)
  -- @
  -- would be transformed into:
  -- @ let ys = reduce^(+,0,xss) in
  --   let zs = iota^(ys) in
  --   let res = zs
  -- @
  -- so we would need to know that what the array for @y@ and @z@ was,
  -- creating the mapping [y -> ys, z -> zs]

  , flattenedDims :: M.Map (SubExp,SubExp) SubExp

  , segDescriptors :: M.Map [SubExp] [SegDescp]
    -- ^ segment descriptors for arrays. This is a should not be
    -- empty. First element represents the outermost
    -- dimension. (sometimes called segment descriptor 0 -- as it
    -- belong to dimension 0)
    --
    -- [SubExp] should always contain at least two elements (a 1D
    -- array has no segment descriptor)
  }

data Regularity = Regular
                | Irregular
                deriving(Show, Eq)

type SegDescp = (Ident, Regularity)


newtype FlatM a = FlatM (StateT FlatState (Either Error) a)
                deriving ( MonadState FlatState
                         , Monad, Applicative, Functor
                         )

instance MonadFreshNames FlatM where
  getNameSource = gets vnameSource
  putNameSource newSrc = modify $ \s -> s { vnameSource = newSrc }

instance HasTypeEnv FlatM where
  askTypeEnv = error "Please give Futhark.Flattening a proper type environment."

--------------------------------------------------------------------------------

data Error = Error String
           | MemTypeFound
           | ArrayNoDims Ident

instance Show Error where
  show (Error msg) = msg
  show MemTypeFound = "encountered Mem as Type"
  show (ArrayNoDims i) = "found array without any dimensions " ++ pretty i

runFlatM :: FlatState -> FlatM a -> Either Error (a, FlatState)
runFlatM s (FlatM a) = runStateT a s

flatError :: Error -> FlatM a
flatError e = FlatM . lift $ Left e

--------------------------------------------------------------------------------
-- Functions for working with FlatState
--------------------------------------------------------------------------------

getMapLetArray' :: VName -> FlatM Ident
getMapLetArray' vn = do
  letArrs <- gets mapLetArrays
  case M.lookup vn letArrs of
    Just letArr -> return letArr
    Nothing -> flatError $ Error $ "getMapLetArray': Couldn't find " ++
                                   pretty vn ++
                                   " in table"

addMapLetArray :: VName -> Ident -> FlatM ()
addMapLetArray vn letArr = do
  letArrs <- gets mapLetArrays
  case M.lookup vn letArrs of
    (Just _) -> flatError $ Error $ "addMapLetArray: " ++
                                    pretty vn ++
                                    " already present in table"
    Nothing -> do
      let letArrs' = M.insert vn letArr letArrs
      modify (\s -> s{mapLetArrays = letArrs'})

----------------------------------------

getFlattenedDims1 :: (SubExp, SubExp) -> FlatM SubExp
getFlattenedDims1 (outer,inner) = do
  fds <- gets flattenedDims
  case M.lookup (outer,inner) fds of
    Just sz -> return sz
    Nothing -> flatError $ Error $ "getFlattenedDims not created for" ++
                                    show (pretty outer, pretty inner)

getFlattenedDims :: (SubExp, SubExp) -> FlatM (Maybe SubExp)
getFlattenedDims (outer,inner) = do
  fds <- gets flattenedDims
  return $ M.lookup (outer,inner) fds
----------------------------------------

getSegDescriptors1Ident :: Ident -> FlatM [SegDescp]
getSegDescriptors1Ident (Ident _ (Array _ (Shape subs) _)) =
  getSegDescriptors1 subs
getSegDescriptors1Ident i =
  flatError $ Error $ "getSegDescriptors1Ident, not an array " ++ show i

getSegDescriptors1 :: [SubExp] -> FlatM [SegDescp]
getSegDescriptors1 subs = do
  segmap <- gets segDescriptors
  case M.lookup subs segmap of
    Just segs -> return segs
    Nothing   -> flatError $ Error $ "getSegDescriptors: Couldn't find " ++
                                      show subs ++
                                      " in table"

getSegDescriptors :: [SubExp] -> FlatM (Maybe [SegDescp])
getSegDescriptors subs = do
  segmap <- gets segDescriptors
  return $ M.lookup subs segmap

addSegDescriptors :: [SubExp] -> [SegDescp] -> FlatM ()
addSegDescriptors [] segs =
  flatError $ Error $ "addSegDescriptors: subexpressions empty for " ++ show segs
addSegDescriptors subs [] =
  flatError $ Error $ "addSegDescriptors: empty seg array for " ++ show subs
addSegDescriptors subs segs = do
  segmap <- gets segDescriptors
  case M.lookup subs segmap of
    (Just _) -> flatError $ Error $ "addSegDescriptors:  " ++ show subs ++
                                    " already present in table"
    Nothing -> do
      let segmap' = M.insert subs segs segmap
      modify (\s -> s{segDescriptors = segmap'})

-- A list of subexps (defining shape of array), will define the segment descriptor
createSegDescsForArray :: Ident -> FlatM ()
createSegDescsForArray (Ident vn (Array _ (Shape (dim0:dims@(_:_))) _)) = do
  alreadyCreated <- liftM isJust $ getSegDescriptors (dim0:dims)

  unless alreadyCreated $ do
    (sizes, singlesegs) :: ([[SubExp]], [SegDescp]) <-
      liftM (unzip . reverse . (\(res,_,_,_) -> res)) $
        foldM  (\(res,dimouter,n::Int,_:dimrest) diminner -> do
                   segsize <- createFlattenedDims n (dimouter, diminner)
                   let segname = baseString vn ++ "_seg" ++ show n
                   seg <- newIdent segname (Array Int (Shape [segsize]) Nonunique)
                   return ((dimouter:dimrest, (seg, Regular)):res, segsize, n+1, dimrest)
               )
               ([],dim0,0,dim0:dims) dims

    let segs = map (`drop` singlesegs) [0..length singlesegs]
    zipWithM_ addSegDescriptors sizes segs

  where
    createFlattenedDims :: Int -> (SubExp, SubExp) -> FlatM SubExp
    createFlattenedDims n (outer,inner) = do
      fds <- gets flattenedDims
      case M.lookup (outer,inner) fds of
           Just sz -> return sz
           Nothing -> do
             new_subexp <- Var <$> newVName ("fakesize" ++ show n)
             let fds' = M.insert (outer,inner) new_subexp fds
             modify (\s -> s{flattenedDims = fds'})
             return new_subexp

createSegDescsForArray i = flatError . Error $ "createSegDescsForArray on non 2D array: " ++
                                               show i

--------------------------------------------------------------------------------

flattenProg :: Prog -> Either Error Prog
flattenProg p@(Prog funs) = do
  let funs' = map renameToOld funs
  (funsTrans,_) <- mapAndUnzipM (runFlatM initState . transformFun) funs
  return $ Prog (funsTrans ++ funs')
  where
    initState = FlatState { vnameSource = newNameSourceForProg p
                          , mapLetArrays = M.empty
                          , flattenedDims = M.empty
                          , segDescriptors = M.empty
                          }
    renameToOld (FunDec name retType params body) =
      FunDec name' retType params body
      where
        name' = nameFromString $ nameToString name ++ "_orig"

--------------------------------------------------------------------------------

transformFun :: FunDec -> FlatM FunDec
transformFun (FunDec name retType params body) = do
  mapM_ createSegDescsForArray $
    filter (maybe False (>=2) . dimentionality . identType)
           (map fparamIdent params)
  body' <- transformBody body
  return $ FunDec name' retType params body'
  where
    name' = nameFromString $ nameToString name ++ "_flattrans"

transformBody :: Body -> FlatM Body
transformBody (Body () bindings (Result ses)) = do
  bindings' <- concat <$> mapM transformBinding bindings
  return $ Body () bindings' (Result ses)

-- | Transform a function to use parallel operations.
-- Only maps needs to be transformed, @map f xs@ ~~> @f^ xs@
transformBinding :: Binding -> FlatM [Binding]
transformBinding topBnd@(Let (Pattern pats) ()
                             (LoopOp (Map certs lambda arrs))) = do
  okLamBnds <- mapM isSafeToMapBinding lamBnds
  let grouped = foldr group [] $ zip okLamBnds lamBnds

  arrtps <- mapM lookupType arrs
  outerSize <- case arrtps of
                 Array _ (Shape (outer:_)) _:_ -> return outer
                 _ -> flatError $ Error "impossible, map argument was not a list"

  case grouped of
   [Right _] -> return [topBnd]
   _ -> do
     loopinv_vns <-
       filter (`notElem` arrs) <$> filterM (liftM isJust . vnDimentionality)
       (HS.toList $ freeInExp (LoopOp $ Map certs lambda arrs))
     (loopinv_repbnds, loopinv_repidents) <-
       mapAndUnzipM (replicateVName outerSize) loopinv_vns

     let mapInfo = MapInfo { mapListArgs = arrs ++ map identName loopinv_repidents
                           , lamParamVNs = lambdaParamVNs ++ loopinv_vns
                           , mapLets = letBoundIdentsInLambda
                           , mapSize = outerSize
                           , mapCerts = certs
                           }

     let mapResNeed = HS.unions $ map freeIn
                      (resultSubExps $ bodyResult $ lambdaBody lambda)
     let freeIdents = flip map grouped $ \case
                Right bnds -> HS.unions $ map (freeInExp . bindingExp) bnds
                Left bnd -> freeInExp $ bindingExp bnd
     let _:needed = scanr HS.union mapResNeed freeIdents
     let defining = flip map grouped $ \case
                      -- TODO: assuming Bindage == BindVar (which is ok?)
           Right bnds -> concatMap (map (identName . patElemIdent)
                                    . patternElements
                                    . bindingPattern
                                   ) bnds
           Left bnd -> map (identName . patElemIdent) $
                            patternElements $ bindingPattern bnd
     let shouldReturn = zipWith (\def need -> filter (`HS.member` need ) def)
                                defining needed
     let argsNeeded = zipWith (\def freeIds -> filter (`notElem` def) freeIds)
                              defining (map HS.toList freeIdents)

     grouped' <- zipWithM
       (\v bndInfo -> case v of
                Right bnds -> liftM Right $ wrapRightInMap mapInfo bndInfo bnds
                Left bnd ->  liftM Left $ pullOutOfMap mapInfo bndInfo bnd
       ) grouped (zip argsNeeded shouldReturn)

     res' <- forM (resultSubExps . bodyResult $ lambdaBody lambda) $
             \se -> case se of
                      (Constant bv) -> return $ Constant bv
                      (Var vn) -> Var <$> identName <$> getMapLetArray' vn

     let resBnds =
           zipWith (\pe se -> Let (Pattern [pe]) () (PrimOp $ SubExp se))
                   pats res'

     return $ loopinv_repbnds ++
              concatMap (either id (: [])) grouped' ++ resBnds

  where
    lamBnds = bodyBindings $ lambdaBody lambda
    letBoundIdentsInLambda =
      concatMap (map (identName . patElemIdent) . patternElements . bindingPattern)
                (bodyBindings $ lambdaBody lambda)

    lambdaParamVNs = map identName $ lambdaParams lambda

    group :: (Bool, Binding)
             -> [Either Binding [Binding]]
             -> [Either Binding [Binding]]
    group (True, bnd) (Right bnds : list) = (Right $ bnd:bnds) : list
    group (True, bnd) list                = Right [bnd] : list
    group (False, bnd) list               = Left bnd : list

    wrapRightInMap :: MapInfo -> ([VName], [VName]) -> [Binding] -> FlatM Binding
    wrapRightInMap mapInfo (argsneeded, toreturn_vns) bnds = do
      (mapparams, argarrs) <- liftM (unzip . catMaybes)
        $ forM argsneeded $ \arg -> do
            argarr <- findTarget mapInfo arg
            case argarr of
              Just val -> do
                val_tp <- lookupType arg
                return $ Just (Ident arg val_tp, identName val)
              Nothing -> return Nothing

      pat <- liftM (Pattern . map (\i -> PatElem i BindVar () ))
             $ forM toreturn_vns $ \sr -> do
                 iarr <- wrapInArrVName (mapSize mapInfo) sr
                 addMapLetArray sr iarr
                 return iarr

      let lambody = Body { bodyLore = ()
                         , bodyBindings = bnds
                         , bodyResult = Result $ map Var toreturn_vns
                         }

      toreturn_tps <- mapM lookupType toreturn_vns
      let wrappedlambda = Lambda { lambdaParams = mapparams
                                 , lambdaBody = lambody
                                 , lambdaReturnType = toreturn_tps
                                 }

      let theMapExp = LoopOp $ Map certs wrappedlambda argarrs
      return $ Let pat () theMapExp

transformBinding bnd = return [bnd]

--------------------------------------------------------------------------------

data MapInfo = MapInfo {
    mapListArgs :: [VName]
    -- ^ the lists supplied to the map, ie [xs,ys] in
    -- @map f {xs,ys}@
  , lamParamVNs :: [VName]
    -- ^ the idents parmas in the map, ie [x,y] in
    -- @map (\x y -> let z = x+y in z) {xs,ys}@
  , mapLets :: [VName]
    -- ^ the idents that are bound in the outermost level of the map,
    -- ie [z] in @map (\x y -> let z = x+y in z) {xs,ys}@
  , mapSize :: SubExp
    -- ^ the number of elements being mapped over
  , mapCerts :: Certificates
  }

-- |3nd arg is a _single_ binding that we need to take out of a map, ie
-- either @y = reduce(+,0,xs)@ or @z = iota(y)@ in
-- @map (\xs -> let y = reduce(+,0,xs) in let z = iota(y) in z) xss@
--
-- 1st arg is general information about the map we should pull
-- something out of
--
-- 2nd arg is ([Ident used in expression], [Ident needed by other expressions])
--
-- Invariant is that /all/ idents must add their new parent array to
-- the @mapLetArray@. so after transforming
-- @let y = reduce(+,0,xs)@
-- ~~>
-- @ys = segreduce(+,0,xss)@
-- we must add the mapping @y |-> ys@ so that we can find the correct array
-- for @y@ when processing @z = iota(y)@
pullOutOfMap :: MapInfo -> ([VName], [VName]) -> Binding -> FlatM [Binding]
-- If no expressions is needed, do nothing (this case should be
-- covered by other optimisations
pullOutOfMap _ (_,[]) _ = return []
pullOutOfMap mapInfo _
                  (Let (Pattern [PatElem resIdent BindVar ()]) ()
                       (PrimOp (Reshape certs dimses reshapearr))) = do
  Just target <- findTarget mapInfo reshapearr

  loopdep_dim_subexps <- filterM (\case
                                  Var i -> liftM isJust $ findTarget mapInfo i
                                  Constant _ -> return False
                               ) dimses

  unless (null loopdep_dim_subexps) $
    flatError $ Error $ "pullOutOfMap Reshape: loop dependant variable used " ++
                       show (map pretty loopdep_dim_subexps) ++
                       " ^ TODO: implement SegReshape thingy"

  newResIdent <-
    case resIdent of
      (Ident vn (Array bt (Shape shpdms) uniq)) -> do
        vn' <- newID (baseName vn)
        return $ Ident vn' (Array bt (Shape (mapSize mapInfo:shpdms)) uniq)
      _ -> flatError $ Error "impossible, result of reshape not list"

  addMapLetArray (identName resIdent) newResIdent

  let newReshape = PrimOp $ Reshape (certs ++ mapCerts mapInfo)
                                    (mapSize mapInfo:dimses) $ identName target

  return [Let (Pattern [PatElem newResIdent BindVar ()]) () newReshape]

pullOutOfMap mapInfo (argsneeded, _)
                     (Let (Pattern pats) ()
                          (LoopOp (Map certs lambda arrs))) = do
  -- For all argNeeded that are not already being mapped over:
  --
  -- 1) if they where created as an intermediate result in the outer map,
  --    distribute/replicate the values of the array holding the intermediate
  --    results, so we can map over them. ie
  --    @ map(\xs y -> let z = y*y in
  --                       map (\x z -> z+x, xs)
  --         , {xss,ys})
  --    @
  --    ~~>
  --    @ map(\xs y -> let z = y*y in
  --                   let zs_dist = replicate(z,?) in
  --                       map (\x z -> z+x, {xs,xz})
  --         , {xss,ys})
  --    @
  --    ~~>
  --    @ let zs = map (\y -> y*y) ys
  --      let zs_dist = distribute (zs, ?) // map (\z -> replicate(z,?)) zs
  --      let zs_dist_sd = stepdown(zs_dist)
  --      let xss_sd = stepdown(xss)
  --      let res_sd = map (\x z -> z+x, {xs,zs})
  --      in stepup(res_sd)
  --    @
  --
  -- 2) They are also invariant in the outer loop TODO

  -----------------------------------------------
  -- Handle argument identifiers for inner map --
  -----------------------------------------------
  (okIdents, okLambdaParams) <-
      liftM unzip
      $ filterM (\(vn,_) -> isJust <$> findTarget mapInfo vn)
              $ zip arrs (lambdaParams lambda)
  (loopInvIdents, loopInvLambdaParams) <-
      liftM unzip
      $ filterM (\(vn,_) -> isNothing <$> findTarget mapInfo vn)
              $ zip arrs (lambdaParams lambda)
  (loopInvRepBnds, loopInvIdentsArrs) <-
    mapAndUnzipM (replicateVName $ mapSize mapInfo) loopInvIdents

  (flattenIdents, flatIdents) <-
    mapAndUnzipM (flattenArg mapInfo) $
                 (Right <$> okIdents) ++ (Left <$> loopInvIdentsArrs)

  (unflattenPats, pats') <- mapAndUnzipM unflattenRes pats


  -- We need this later on
  arrs_tps <- mapM lookupType arrs
  innerMapSize <- case arrs_tps of
                    Array _ (Shape (outer:_)) _:_ -> return outer
                    _ -> flatError $ Error "impossible, map argument was not a list"

  -------------------------------------------------------------
  -- Handle Idents needed by body, which are not mapped over --
  -------------------------------------------------------------

  -- TODO: This will lead to size variables been mapeed over :(
  let reallyNeeded = filter (`notElem` arrs) argsneeded
  --
  -- Intermediate results needed
  --
  intmres_vns <- filterM (liftM isJust . findTarget mapInfo) reallyNeeded

  -- Need to rename so our intermediate result will not be found in
  -- other calls (through mapLetArray)
  intmres_vns' <- mapM newName intmres_vns
  intmres_tps <- mapM lookupType intmres_vns
  let intmres_params = zipWith Ident intmres_vns' intmres_tps
  intmres_arrs <- mapM (findTarget1 mapInfo) intmres_vns

  --
  -- Distribute and flatten idents needed (from above)
  --
  (distBnds, distArrIdents) <- mapAndUnzipM (distributeExtraArg innerMapSize)
                                            intmres_arrs
  (flatDistBnds, flatDistArrIdents) <- mapAndUnzipM (flattenArg mapInfo) $
                                         Left <$> distArrIdents


  -----------------------------------------
  -- Merge information and update lambda --
  -----------------------------------------
  let newInnerIdents = flatIdents ++ flatDistArrIdents
  let lambdaParams' = okLambdaParams ++ loopInvLambdaParams
                      ++ intmres_params

  let lambdaBody' = substituteNames
                    (HM.fromList $ zip intmres_vns
                                       intmres_vns')
                    $ lambdaBody lambda

  let lambda' = lambda { lambdaParams = lambdaParams'
                       , lambdaBody = lambdaBody'
                       }

  let mapBnd' = Let (Pattern pats') ()
                    (LoopOp (Map (certs ++ mapCerts mapInfo)
                                 lambda' $
                                 map identName newInnerIdents))

  mapBnd'' <- transformBinding mapBnd'

  return $ distBnds ++ flatDistBnds ++
           loopInvRepBnds ++
           flattenIdents ++ mapBnd'' ++ unflattenPats


  where
    -- | The inner map apparently depends on some variable that does
    -- not come from the lists mapped over, so we'll need to add that
    --
    -- 1st arg is the size of the inner map
    distributeExtraArg :: SubExp -> Ident -> FlatM (Binding, Ident)
    distributeExtraArg sz i@(Ident vn tp) = do
      distTp <- case tp of
                 Mem{} -> flatError MemTypeFound
                 Array _ (Shape []) _ -> flatError $ ArrayNoDims i
                 (Basic bt) ->
                   return $ Array bt (Shape [sz]) Nonunique
                 (Array bt (Shape (out:rest)) uniq) -> do
                   when (out /= mapSize mapInfo) $
                     flatError $
                       Error $ "distributeExtraArg: " ++
                               "trying to distribute array with incorrect outer size " ++
                               pretty i
                   return $ Array bt (Shape $ out:sz:rest) uniq

      distIdent <- newIdent (baseString vn ++ "_dist") distTp

      let distExp = Apply (nameFromString "distribute")
                          [(Var vn, Observe), (sz, Observe)]
                          --  ^ TODO: I guess  Observe is okay for now
                          (basicRetType Int)
                          --  ^ TODO: stupid exsitensial types :(


      let distBnd = Let (Pattern [PatElem distIdent BindVar ()]) () distExp

      return (distBnd, distIdent)

    -- | Steps for exiting a nested map, meaning we step-up/unflatten the result
    unflattenRes :: PatElem -> FlatM (Binding, PatElem)
    unflattenRes (PatElem (Ident vn (Array bt (Shape (outer:rest)) uniq))
                          BindVar ()) = do
      flatSize <- getFlattenedDims1 (mapSize mapInfo, outer)
      let flatTp = Array bt (Shape $ flatSize:rest) uniq
      flatResArr <- newIdent (baseString vn ++ "_sd") flatTp
      let flatResArrPat = PatElem flatResArr BindVar ()

      let finalTp = Array bt (Shape $ mapSize mapInfo :outer:rest) uniq
      finalResArr <- newIdent (baseString vn) finalTp

      addMapLetArray vn finalResArr

      let unflattenExp = Apply (nameFromString "stepup")
                          [(Var $ identName flatResArr, Observe)]
                          --  ^ TODO: I guess Observe is okay for now
                          (basicRetType Int)
                          --  ^ TODO: stupid exsitensial types :(
      let unflattenBnd = Let (Pattern [PatElem finalResArr BindVar ()])
                             () unflattenExp

      return (unflattenBnd, flatResArrPat)
    unflattenRes pe = flatError $ Error $ "unflattenRes applied to " ++ pretty pe

pullOutOfMap mapInfo _
                     topBnd@(Let (Pattern pats) letlore
                                 (LoopOp (Reduce certs lambda args))) = do
  ok <- isSafeToMapBody $ lambdaBody lambda
  if not ok
  then flatError . Error $ "map of reduce with \"advanced\" operator"
                           ++ pretty topBnd
  else do
    -- Create new PatElems and Idents to hold result (now arrays)

    pats' <- forM pats $ \(PatElem i BindVar ()) -> do
      let vn = identName i
      arr <- wrapInArrIdent (mapSize mapInfo) i
      addMapLetArray vn arr
      return $ PatElem arr BindVar ()

    -- FIXME: This does not handle completely loop invariant things
    (flatBnds, flatargs) <- liftM unzip $
      forM args $ \(ne,i) -> do
        (bnd, flatArr) <- flattenArg mapInfo $ Right i
        return (bnd, (ne, identName flatArr))

    seginfo <- case args of
                 (_,argarr):_ ->
                   getSegDescriptors1Ident =<< findTarget1 mapInfo argarr
                 [] -> flatError $ Error "Reduce on empty array, not possible"
    segdescp <- case seginfo of
                  [(descp,_)] -> return $ identName descp
                  _ -> flatError $ Error $ "pullOutOfMap Reduce: argument array was not two dimensional"
                                         ++ pretty topBnd

    let redBnd' = Let (Pattern pats') letlore
                      (SegOp (SegReduce certs lambda flatargs segdescp))

    return $ flatBnds ++ [redBnd']

pullOutOfMap mapinfo _ (Let (Pattern [PatElem ident1 BindVar _]) _
                      (PrimOp (SubExp (Var name)))) = do
  addMapLetArray (identName ident1) =<< findTarget1 mapinfo name
  return []

pullOutOfMap _ _ binding =
  flatError $ Error $ "pullOutOfMap not implemented for " ++ pretty binding ++
                      "\n\t" ++ show binding

----------------------------------------


-- | preparation steps to enter nested map, meaning we
    -- step-down/flatten/concat the outer array.
    --
    -- 1st arg is the parrent array for the Ident. In most cases, this
    -- will be @Nothing@ and this function will find it itself.
-- TODO: does not currently handle loop invariant arrays
flattenArg :: MapInfo -> Either Ident VName -> FlatM (Binding, Ident)
flattenArg mapInfo targInfo = do
  target <- case targInfo of
              Left targ -> return targ
              Right innerMapArg -> findTarget1 mapInfo innerMapArg

  -- tod = Target Outer Dimension
  (tod1, tod2, rest, bt, uniq) <- case target of
    (Ident _ (Array bt (Shape (tod1:tod2:rest)) uniq)) ->
              return (tod1, tod2, rest, bt, uniq)
    _ -> flatError $ Error $ "trying to flatten less than 2D array: " ++ show target

  newSize <- getFlattenedDims1 (tod1, tod2)

  let flatTp = Array bt (Shape (newSize : rest)) uniq
  flatIdent <- newIdent (baseString (identName target) ++ "_sd") flatTp

  let flattenExp = Apply (nameFromString "stepdown")
                         [(Var $ identName target, Observe)]
                         --  ^ TODO: I guess Observe is okay for now
                         (basicRetType Int)
                         --  ^ TODO: stupid exsitensial types :(


  let flatBnd = Let (Pattern [PatElem flatIdent BindVar ()])
                    () flattenExp

  return (flatBnd, flatIdent)

-- | Find the "parent" array for a given Ident in a /specific/ map
findTarget :: MapInfo -> VName -> FlatM (Maybe Ident)
findTarget mapInfo vn =
  case L.elemIndex vn $ lamParamVNs mapInfo of
    Just n -> do
      let target_vn = mapListArgs mapInfo !! n
      target_tp <- lookupType target_vn
      return . Just $ Ident target_vn target_tp
    Nothing -> if vn `notElem` mapLets mapInfo
               -- this argument is loop invariant
               then return Nothing
               else liftM Just $ getMapLetArray' vn

findTarget1 :: MapInfo -> VName -> FlatM Ident
findTarget1 mapInfo vn =
  findTarget mapInfo vn >>= \case
    Just arr -> return arr
    Nothing -> flatError $ Error $ "findTarget': couldn't find expected arr for "
                                   ++ pretty vn

wrapInArrIdent :: SubExp -> Ident -> FlatM Ident
wrapInArrIdent sz (Ident vn tp) = do
  arrtp <- case tp of
    Basic bt                   -> return $ Array bt (Shape [sz]) Nonunique
    Array bt (Shape rest) uniq -> return $ Array bt (Shape $ sz:rest) uniq
    Mem _ -> flatError MemTypeFound
  newIdent (baseString vn ++ "_arr") arrtp

replicateIdent :: SubExp -> Ident -> FlatM (Binding, Ident)
replicateIdent sz i = do
  arrRes <- wrapInArrIdent sz i
  let repExp = PrimOp $ Replicate sz $ Var $ identName i
      repBnd = Let (Pattern [PatElem arrRes BindVar ()]) () repExp
  return (repBnd, arrRes)


wrapInArrVName :: SubExp -> VName -> FlatM Ident
wrapInArrVName sz vn = do
  tp <- lookupType vn
  wrapInArrIdent sz $ Ident vn tp

replicateVName :: SubExp -> VName -> FlatM (Binding, Ident)
replicateVName sz vn = do
  tp <- lookupType vn
  replicateIdent sz $ Ident vn tp

--------------------------------------------------------------------------------

isSafeToMapBody :: Body -> FlatM Bool
isSafeToMapBody (Body _ bindings _) = and <$> mapM isSafeToMapBinding bindings

isSafeToMapBinding :: Binding -> FlatM Bool
isSafeToMapBinding (Let _ _ e) = isSafeToMapExp e

-- | Is it safe to put this expression @e@ in a flat map ?
-- ie. @map(fn x -> e, xs)@
-- Else we need to apply a segmented operator on it
isSafeToMapExp :: Exp -> FlatM Bool
isSafeToMapExp (PrimOp po) = do
  ts <- primOpType po
  and <$> mapM isSafeToMapType ts
-- DoLoop/Map/ConcatMap/Reduce/Scan/Filter/Redomap
isSafeToMapExp (LoopOp _) = return False
isSafeToMapExp (SegOp _) = return False
isSafeToMapExp (If _ e1 e2 _) =
  liftM2 (&&) (isSafeToMapBody e1) (isSafeToMapBody e2)
isSafeToMapExp (Apply{}) =
  flatError $ Error "TODO: isSafeToMap not implemented for Apply"

isSafeToMapType :: Type -> FlatM Bool
isSafeToMapType (Mem{}) = flatError MemTypeFound
isSafeToMapType (Basic _) = return True
isSafeToMapType (Array{}) = return False

--------------------------------------------------------------------------------

dimentionality :: Type -> Maybe Int
dimentionality (Array _ (Shape dims) _) = Just $ length dims
dimentionality _ = Nothing

vnDimentionality :: VName -> FlatM (Maybe Int)
vnDimentionality vn =
  liftM dimentionality $ lookupType vn
