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
getMapLetArray' ident = do
  letArrs <- gets mapLetArrays
  case M.lookup ident letArrs of
    Just letArr -> return letArr
    Nothing -> flatError $ Error $ "getMapLetArray': Couldn't find " ++
                                   pretty ident ++
                                   " in table"

addMapLetArray :: VName -> Ident -> FlatM ()
addMapLetArray ident letArr = do
  letArrs <- gets mapLetArrays
  case M.lookup ident letArrs of
    (Just _) -> flatError $ Error $ "addMapLetArray: " ++
                                    pretty ident ++
                                    " already present in table"
    Nothing -> do
      let letArrs' = M.insert ident letArr letArrs
      modify (\s -> s{mapLetArrays = letArrs'})

----------------------------------------

getFlattenedDims :: (SubExp, SubExp) -> FlatM SubExp
getFlattenedDims (outer,inner) = do
  fds <- gets flattenedDims
  case M.lookup (outer,inner) fds of
    Just sz -> return sz
    Nothing -> flatError $ Error $ "getFlattenedDims not created for" ++
                                    show (pretty outer, pretty inner)

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
    filter (maybe False (>=2) . identDimentionality)
           (map fparamIdent params)
  body' <- transformBody body
  return $ FunDec name' retType params body'
  where
    name' = nameFromString $ nameToString name ++ "_flattrans"

transformBody :: Body -> FlatM Body
transformBody (Body lore bindings (Result ses)) = do
  bindings' <- concat <$> mapM transformBinding bindings
  return $ Body lore bindings' (Result ses)

-- | Transform a function to use parallel operations.
-- Only maps needs to be transformed, @map f xs@ ~~> @f^ xs@
transformBinding :: Binding -> FlatM [Binding]
transformBinding topBnd@(Let (Pattern pats) ()
                             (LoopOp (Map certs lambda arrs))) = do
  idents <- mapM toIdent arrs
  okLamBnds <- mapM isSafeToMapBinding lamBnds
  let grouped = foldr group [] $ zip okLamBnds lamBnds

  outerSize <- case idents of
                 Ident _ (Array _ (Shape (outer:_)) _):_ -> return outer
                 _ -> flatError $ Error "impossible, map argument was not a list"

  case grouped of
   [Right _] -> return [topBnd]
   _ -> do
     loopinv_idents <-
       filter (`notElem` idents) <$> filter (isJust . identDimentionality) <$>
       mapM toIdent (HS.toList $ freeInExp (LoopOp $ Map certs lambda arrs))
     (loopinv_repbnds, loopinv_repidents) <-
       mapAndUnzipM (replicateIdent outerSize) loopinv_idents

     let mapInfo = MapInfo { mapListArgs = idents ++ loopinv_repidents
                           , lamParams = lambdaParams lambda ++ loopinv_idents
                           , mapLets = letBoundIdentsInLambda lambda
                           , mapSize = outerSize
                           , mapCerts = certs
                           }

     mapResNeed <- liftM HS.fromList $ mapM toIdent $ HS.toList $
                   HS.unions $ map freeIn
                   (resultSubExps $ bodyResult $ lambdaBody lambda)
     freeIdents <- liftM (map HS.fromList) $ mapM (mapM toIdent . HS.toList) $
                   flip map grouped $ \case
       Right bnds -> HS.unions $ map (freeInExp . bindingExp) bnds
       Left bnd -> freeInExp $ bindingExp bnd
     let _:needed = scanr HS.union mapResNeed freeIdents
     let defining = flip map grouped $ \case
                      -- TODO: assuming Bindage == BindVar (which is ok?)
           Right bnds -> concatMap (map patElemIdent
                                    . patternElements
                                    . bindingPattern
                                   ) bnds
           Left bnd -> map patElemIdent $ patternElements $ bindingPattern bnd
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
                      (Var ident) -> Var <$> identName <$> getMapLetArray' ident

     let resBnds =
           zipWith (\pe se -> Let (Pattern [pe]) () (PrimOp $ SubExp se))
                   pats res'

     return $ loopinv_repbnds ++
              concatMap (either id (: [])) grouped' ++ resBnds

  where
    lamBnds = bodyBindings $ lambdaBody lambda

    group :: (Bool, Binding)
             -> [Either Binding [Binding]]
             -> [Either Binding [Binding]]
    group (True, bnd) (Right bnds : list) = (Right $ bnd:bnds) : list
    group (True, bnd) list                = Right [bnd] : list
    group (False, bnd) list               = Left bnd : list

    wrapRightInMap :: MapInfo -> ([Ident], [Ident]) -> [Binding] -> FlatM Binding
    wrapRightInMap mapInfo (argsNeeded, shouldReturn) bnds = do

      (mapIdents, argArrs) <- liftM (unzip . catMaybes)
        $ forM argsNeeded $ \arg -> do
            argArr <- findTarget mapInfo $ identName arg
            case argArr of
              Just val -> return $ Just (arg, val)
              Nothing -> return Nothing

      pat <- liftM (Pattern . map (\i -> PatElem i BindVar () ))
             $ forM shouldReturn $ \i -> do
                 iArr <- wrapInArrIdent (mapSize mapInfo) i
                 addMapLetArray (identName i) iArr
                 return iArr

      let lamBody = Body { bodyLore = ()
                         , bodyBindings = bnds
                         , bodyResult = Result $ map (Var . identName) shouldReturn
                         }

      let wrapLambda = Lambda { lambdaParams = mapIdents
                              , lambdaBody = lamBody
                              , lambdaReturnType = map identType shouldReturn
                              }

      let theMapExp = LoopOp $ Map certs wrapLambda $ map identName argArrs
      return $ Let pat () theMapExp

transformBinding bnd = return [bnd]

letBoundIdentsInLambda :: Lambda -> [Ident]
letBoundIdentsInLambda lambda =
   concatMap (\(Let (Pattern pats) _ _) ->map (\(PatElem i _ _) -> i)  pats)
             (bodyBindings $ lambdaBody lambda)

--------------------------------------------------------------------------------

data MapInfo = MapInfo {
    mapListArgs :: [Ident]
    -- ^ the lists supplied to the map, ie [xs,ys] in
    -- @map f {xs,ys}@
  , lamParams :: [Ident]
    -- ^ the idents parmas in the map, ie [x,y] in
    -- @map (\x y -> let z = x+y in z) {xs,ys}@
  , mapLets :: [Ident]
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
pullOutOfMap :: MapInfo -> ([Ident], [Ident]) -> Binding -> FlatM [Binding]
-- If no expressions is needed, do nothing (this case should be
-- covered by other optimisations
pullOutOfMap _ (_,[]) _ = return []
pullOutOfMap mapInfo _
                  (Let (Pattern [PatElem resIdent BindVar patlore]) letlore
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

  return [Let (Pattern [PatElem newResIdent BindVar patlore])
              letlore newReshape]

pullOutOfMap mapInfo (argsNeeded, _)
                     (Let (Pattern pats) letlore
                          (LoopOp (Map certs lambda arrs))) = do
  idents <- mapM toIdent arrs
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
      $ filterM (\(i,_) -> isJust <$> findTarget mapInfo (identName i))
              $ zip idents (lambdaParams lambda)
  (loopInvIdents, loopInvLambdaParams) <-
      liftM unzip
      $ filterM (\(i,_) -> isNothing <$> findTarget mapInfo (identName i))
              $ zip idents (lambdaParams lambda)
  (loopInvRepBnds, loopInvIdentsArrs) <- mapAndUnzipM (replicateIdent $ mapSize mapInfo)
                                                      loopInvIdents

  (flattenIdents, flatIdents) <-
    mapAndUnzipM (flattenArg mapInfo) $
                 (Right <$> okIdents) ++ (Left <$> loopInvIdentsArrs)

  (unflattenPats, pats') <- mapAndUnzipM unflattenRes pats


  -- We need this later on
  innerMapSize <- case idents of
                    Ident _ (Array _ (Shape (outer:_)) _):_ -> return outer
                    _ -> flatError $ Error "impossible, map argument was not a list"

  -------------------------------------------------------------
  -- Handle Idents needed by body, which are not mapped over --
  -------------------------------------------------------------
  let reallyNeeded = filter (\i -> not $ HS.member (identName i) $
                                   HS.unions $ map freeIn idents) argsNeeded
  --
  -- Intermediate results needed
  --
  itmResIdents <- filterM (\i -> isJust <$> findTarget mapInfo (identName i)) reallyNeeded

  -- Need to rename so our intermediate result will not be found in
  -- other calls (through mapLetArray)
  itmResIdents' <- mapM (newIdent' id) itmResIdents
  itmResArrs <- mapM (findTarget1 mapInfo) itmResIdents

  --
  -- Distribute and flatten idents needed (from above)
  --
  let extraLamdaParams = itmResArrs
  (distBnds, distArrIdents) <- mapAndUnzipM (distributeExtraArg innerMapSize)
                                            extraLamdaParams
  (flatDistBnds, flatDistArrIdents) <- mapAndUnzipM (flattenArg mapInfo) $
                                         Left <$> distArrIdents


  -----------------------------------------
  -- Merge information and update lambda --
  -----------------------------------------
  let newInnerIdents = flatIdents ++ flatDistArrIdents
  let lambdaParams' = okLambdaParams ++ loopInvLambdaParams
                      ++ extraLamdaParams

  let lambdaBody' = substituteNames
                    (HM.fromList $ zip (map identName itmResIdents)
                                       (map identName itmResIdents'))
                    $ lambdaBody lambda

  let lambda' = lambda { lambdaParams = lambdaParams'
                       , lambdaBody = lambdaBody'
                       }

  let mapBnd' = Let (Pattern pats') letlore
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
                          BindVar patLore) = do
      flatSize <- getFlattenedDims (mapSize mapInfo, outer)
      let flatTp = Array bt (Shape $ flatSize:rest) uniq
      flatResArr <- newIdent (baseString vn ++ "_sd") flatTp
      let flatResArrPat = PatElem flatResArr BindVar patLore

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

pullOutOfMap mapinfo _ (Let (Pattern [PatElem ident1 BindVar _]) _
                      (PrimOp (SubExp (Var name)))) = do
  ident2 <- toIdent name
  addMapLetArray (identName ident1) =<< findTarget1 mapinfo ident2
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
flattenArg :: MapInfo -> Either Ident Ident -> FlatM (Binding, Ident)
flattenArg mapInfo targInfo = do
  target <- case targInfo of
              Left targ -> return targ
              Right innerMapArg -> findTarget1 mapInfo innerMapArg

  -- tod = Target Outer Dimension
  (tod1, tod2, rest, bt, uniq) <- case target of
    (Ident _ (Array bt (Shape (tod1:tod2:rest)) uniq)) ->
              return (tod1, tod2, rest, bt, uniq)
    _ -> flatError $ Error $ "trying to flatten less than 2D array: " ++ show target

  newSize <- getFlattenedDims (tod1, tod2)

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
findTarget mapInfo i =
  case L.elemIndex i $ map identName $ lamParams mapInfo of
    Just n -> return . Just $ mapListArgs mapInfo !! n
    Nothing -> if i `notElem` map identName (mapLets mapInfo)
               -- this argument is loop invariant
               then return Nothing
               else liftM Just $ getMapLetArray' i

findTarget1 :: MapInfo -> Ident -> FlatM Ident
findTarget1 mapInfo i =
  findTarget mapInfo (identName i) >>= \case
    Just iArr -> return iArr
    Nothing -> flatError $ Error $ "findTarget': couldn't find expected arr for "
                                   ++ pretty i

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

identDimentionality :: Ident -> Maybe Int
identDimentionality (Ident _ (Array _ (Shape dims) _)) = Just $ length dims
identDimentionality _ = Nothing

-- XXX: use of this function probably means that there is a design
-- flaw.
toIdent :: HasTypeEnv m => VName -> m Ident
toIdent name = Ident name <$> lookupType name
