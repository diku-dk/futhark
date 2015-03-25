{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, LambdaCase #-}
module Futhark.Flattening ( flattenProg )
  where

import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.List as L

import Futhark.MonadFreshNames
import Futhark.Representation.Basic
--import Futhark.Representation.AST.Attributes.Names

--------------------------------------------------------------------------------

data FlatState = FlatState {
    vnameSource   :: VNameSource
  , mapLetArrays   :: M.Map Ident Ident
  -- ^ arrays for let values in arrays
  -- ie @map (\x -> let y = x+2 in z = y*x in z) xs@
  -- would give @let ys = map (\x -> x+2) xs in
  --             let zs = map (\x y -> y*x) xs ys in
  --                 zs@
  -- so we would create the mapping [y -> ys, z -> zs]

  , flattenedDims :: M.Map (SubExp,SubExp) SubExp
  }

newtype FlatM a = FlatM (StateT FlatState (Either Error) a)
                deriving ( MonadState FlatState
                         , Monad, Applicative, Functor
                         )

instance MonadFreshNames FlatM where
  getNameSource = gets vnameSource
  putNameSource newSrc = modify $ \s -> s { vnameSource = newSrc }

--------------------------------------------------------------------------------

-- TODO: Add SrcLoc
data Error = Error String
           | MemTypeFound

instance Show Error where
  show (Error msg) = msg
  show MemTypeFound = "encountered Mem as Type"

runFlatM :: FlatState -> FlatM a -> Either Error (a, FlatState)
runFlatM s (FlatM a) = runStateT a s

flatError :: Error -> FlatM a
flatError e = FlatM . lift $ Left e

--------------------------------------------------------------------------------

getMapLetArray :: Ident -> FlatM (Maybe Ident)
getMapLetArray ident = do
  letArrs <- gets mapLetArrays
  return $ M.lookup ident letArrs

getMapLetArray' :: Ident -> FlatM Ident
getMapLetArray' ident = do
  letArrs <- gets mapLetArrays
  case M.lookup ident letArrs of
    Just letArr -> return letArr
    Nothing -> flatError $ Error $ "getMapLetArray': Couldn't find Ident "
                                   ++ show ident
                                   ++ " in mapLetArrays table"

addMapLetArray :: Ident -> Ident -> FlatM ()
addMapLetArray ident letArr = do
  letArrs <- gets mapLetArrays
  case M.lookup ident letArrs of
    (Just _) -> flatError $ Error $ "addMapLetArray: Ident " ++ show ident
                         ++ " already present in mapLetArrays table"
    Nothing -> do
      let letArrs' = M.insert ident letArr letArrs
      modify (\s -> s{mapLetArrays = letArrs'})

--------------------------------------------------------------------------------

getFlattenedDims :: (SubExp, SubExp) -> FlatM SubExp
getFlattenedDims (outer,inner) = do
  fds <- gets flattenedDims
  case M.lookup (outer,inner) fds of
    Just sz -> return sz
    Nothing -> do
      new <- liftM Var $ newIdent "size" (Basic Int)
      let fds' = M.insert (outer,inner) new fds
      modify (\s -> s{flattenedDims = fds'})
      return new

--------------------------------------------------------------------------------

flattenProg :: Prog -> Either Error Prog
flattenProg p@(Prog funs) = do
  (funs', _) <- runFlatM initState (mapM transformFun funs)
  return $ Prog (funs ++ funs')
  where
    initState = FlatState { vnameSource = newNameSourceForProg p
                          , mapLetArrays = M.empty
                          , flattenedDims = M.empty
                          }

--------------------------------------------------------------------------------

transformFun :: FunDec -> FlatM FunDec
transformFun (FunDec name retType params body) = do
  body' <- transformBody body
  return $ FunDec name' retType params body'
  where
    name' = nameFromString $ nameToString name ++ "_flattrans"

transformBody :: Body -> FlatM Body
transformBody (Body lore bindings (Result ses)) = do
  bindings' <- concat <$> mapM transformBinding bindings
  -- TODO: should ses be transformed?
  return $ Body lore bindings' (Result ses)

-- | Transform a function to use parallel operations.
-- Only maps needs to be transformed, @map f xs@ ~~> @f^ xs@
transformBinding :: Binding -> FlatM [Binding]
transformBinding topBnd@(Let (Pattern pats) ()
                             (LoopOp (Map certs lambda idents))) = do
  -- TODO: pass on certs
  okLamBnds <- mapM isSafeToMapBinding lamBnds
  let grouped = foldr group [] $ zip okLamBnds lamBnds

  outerSize <- case idents of
                 Ident _ (Array _ (Shape (outer:_)) _):_ -> return outer
                 _ -> flatError $ Error "impossible, map argument was not a list"

  let mapInfo = MapInfo { mapListArgs = idents
                        , lamParams = lambdaParams lambda
                        , mapLets = letBoundIdentsInLambda lambda
                        , mapSize = outerSize
                        }

  case grouped of
   [Right _] -> return [topBnd]
   _ -> do
     grouped' <- mapM
       (\v -> case v of
-- TODO: If the result of a "ok mapable" binding is used outside its
-- own "block", then it must be part of the result of the block. If it
-- is only an intermediate result, it should /not/ be part of the result
                Right _ -> flatError $ Error
                              "can't handle any of the simple cases"
                Left bnd ->  liftM Left
                             $ pullOutOfMap mapInfo bnd
       ) grouped

     res' <- forM (resultSubExps . bodyResult $ lambdaBody lambda) $
             \se -> case se of
                      (Constant bv) -> return $ Constant bv
                      (Var ident) -> liftM Var $ getMapLetArray' ident

     let resBnds =
           zipWith (\pe se -> Let (Pattern [pe]) () (PrimOp $ SubExp se))
                   pats res'

     return $ concatMap (either id id) grouped' ++ resBnds

  where
    lamBnds = bodyBindings $ lambdaBody lambda

    group :: (Bool, Binding)
             -> [Either Binding [Binding]]
             -> [Either Binding [Binding]]
    group (True, bnd) (Right bnds : list) = (Right $ bnd:bnds) : list
    group (True, bnd) list                = Right [bnd] : list
    group (False, bnd) list               = Left bnd : list

transformBinding bnd = return [bnd]

letBoundIdentsInLambda :: Lambda -> [Ident]
letBoundIdentsInLambda lambda =
   concatMap (\(Let (Pattern pats) _ _) ->map (\(PatElem i _ _) -> i)  pats)
             (bodyBindings $ lambdaBody lambda)

--------------------------------------------------------------------------------

data MapInfo = MapInfo {
    mapListArgs :: [Ident]
    -- ^ the lists supplied to the map, ie [xs,ys] in @map f {xs,ys}@
  , lamParams :: [Ident]
    -- ^ the idents parmas in the map, ie [x,y] in
    -- @map (\x y -> let z = x+y in z) {xs,ys}@
  , mapLets :: [Ident]
    -- ^ the idents that are bound in the let, ie [z] in
    -- @map (\x y -> let z = x+y in z) {xs,ys}@
  , mapSize :: SubExp
    -- ^ the number of elements being mapped over
  }

-- | 2nd arg is a _single_ binding that we need to take out of a map, ie
-- either @y = reduce(+,0,xs)@ or @z = iota(y)@ in
-- @map (\xs -> let y = reduce(+,0,xs) in let z = iota(y) in z) xss@
pullOutOfMap :: MapInfo -> Binding -> FlatM [Binding]
pullOutOfMap mapInfo
                  (Let (Pattern [PatElem resIdent BindVar patlore]) letlore
                       (PrimOp (Reshape certs dimSes reshapeIdent))) = do
  -- TODO: Handle reshape on loop invariant array (ie, must be replicated)
  target <- findTarget mapInfo reshapeIdent

  newResIdent <-
    case resIdent of
      (Ident vn (Array bt (Shape shpdms) uniq)) -> do
        vn' <- newID (baseName vn)
        return $ Ident vn' (Array bt (Shape (mapSize mapInfo:shpdms)) uniq)
      _ -> flatError $ Error "impossible, result of reshape not list"

  addMapLetArray resIdent newResIdent

  let newReshape = PrimOp $ Reshape certs (mapSize mapInfo:dimSes) target


  return [Let (Pattern [PatElem newResIdent BindVar patlore])
              letlore newReshape]

pullOutOfMap mapInfo (Let (Pattern pats) letlore
                          (LoopOp (Map certs lambda idents))) = do

  -- FIXME: Handle idents that are loop invariant (these must still be arrays)
  (flattenIdents, idents') <- mapAndUnzipM flattenArg idents
  (unflattenPats, pats') <- mapAndUnzipM unflattenRes pats

  let mapBnd' = Let (Pattern pats') letlore
                    (LoopOp (Map certs lambda idents'))

  mapBnd'' <- transformBinding mapBnd'

  return $ flattenIdents ++ mapBnd'' ++ unflattenPats

  where
    needFlattening :: Ident -> Bool
    needFlattening ident@(Ident _ (Array _ (Shape [_]) _)) =
      ident `elem` lamParams mapInfo || ident `elem` mapLets mapInfo
    needFlattening (Ident _ (Array _ (Shape _) _)) = True
    needFlattening _ = False

    -- | preparation steps to enter nested map, meaning we
    -- step-down/flatten/concat the outer array.
    --
    -- What is supplied is the Ident for the inner map, so we need to
    -- find the "parent" which the Ident is a subarray of
    -- TODO: does not currently handle loop invariant arrays
    flattenArg :: Ident -> FlatM (Binding, Ident)
    flattenArg innerMapArg@(Ident _ (Array bt (Shape _) uniq)) = do
      target <- findTarget mapInfo innerMapArg

      -- tod = Target Outer Dimension
      (tod1, tod2, rest) <- case target of
        (Ident _ (Array _ (Shape (tod1:tod2:rest)) _)) ->
                  return (tod1, tod2, rest)
        _ -> flatError $ Error "trying to flatten less than 2D array"

      newSize <- getFlattenedDims (tod1, tod2)

      let flatTp = Array bt (Shape (newSize : rest)) uniq
      flatIdent <- newIdent (baseString (identName target) ++ "_d") flatTp
      let flattenExp = Apply (nameFromString "stepdown")
                             [(Var target, Observe)] -- TODO: I guess
                                                     -- Observe is okay
                                                     -- for now
                             (basicRetType Int) -- FIXME

      let flatBnd = Let (Pattern [PatElem flatIdent BindVar ()]) () flattenExp

      return (flatBnd, flatIdent)

    flattenArg ident = flatError $ Error $ "flattenArg applied to " ++ pretty ident

    -- | Steps for exiting a nested map, meaning we step-up/unflatten the result
    unflattenRes :: PatElem -> FlatM (Binding, PatElem)
    unflattenRes (PatElem i@(Ident vn (Array bt (Shape (outer:rest)) uniq))
                                BindVar patLore) = do
      flatSize <- getFlattenedDims (mapSize mapInfo, outer)
      let flatTp = Array bt (Shape $ flatSize:rest) uniq
      flatResArr <- newIdent (baseString vn ++ "_d") flatTp
      let flatResArrPat = PatElem flatResArr BindVar patLore

      let finalTp = Array bt (Shape $ mapSize mapInfo :outer:rest) uniq
      finalResArr <- newIdent (baseString vn) finalTp

      addMapLetArray i finalResArr

      let unflattenExp = Apply (nameFromString "stepup")
                          [(Var flatResArr, Observe)] -- TODO: I guess
                                                  -- Observe is okay
                                                  -- for now
                          (basicRetType Int) -- FIXME
      let unflattenBnd = Let (Pattern [PatElem finalResArr BindVar ()])
                             () unflattenExp

      return (unflattenBnd, flatResArrPat)
    unflattenRes pe = flatError $ Error $ "unflattenRes applied to " ++ pretty pe

pullOutOfMap _ binding =
  flatError $ Error $ "pullOutOfMap not implemented for " ++ pretty binding



findTarget :: MapInfo -> Ident -> FlatM Ident
findTarget mapInfo i =
  case L.elemIndex i (lamParams mapInfo) of
    Just n -> return $ mapListArgs mapInfo !! n
    Nothing -> if i `notElem` mapLets mapInfo
               -- this argument is loop invariant
               then flatError . Error $
                    "TODO: should replicate " ++ pretty i
               else getMapLetArray' i

--------------------------------------------------------------------------------

isSafeToMapLambda :: Lambda -> FlatM Bool
isSafeToMapLambda (Lambda _ body _) = isSafeToMapBody body

isSafeToMapBody :: Body -> FlatM Bool
isSafeToMapBody (Body _ bindings _) = and <$> mapM isSafeToMapBinding bindings

isSafeToMapBinding :: Binding -> FlatM Bool
isSafeToMapBinding (Let _ _ e) = isSafeToMapExp e

-- | Is it safe to put this expression @e@ in a flat map ?
-- ie. @map(fn x -> e, xs)@
-- Else we need to apply a segmented operator on it
isSafeToMapExp :: Exp -> FlatM Bool
isSafeToMapExp (PrimOp po) = do
  let ts = primOpType po
  and <$> mapM isSafeToMapType ts
-- DoLoop/Map/ConcatMap/Reduce/Scan/Filter/Redomap
isSafeToMapExp (LoopOp _) = return False
isSafeToMapExp (If _ e1 e2 _) =
  liftM2 (&&) (isSafeToMapBody e1) (isSafeToMapBody e2)
isSafeToMapExp (Apply{}) =
  flatError $ Error "TODO: isSafeToMap not implemented for Apply"

isSafeToMapType :: Type -> FlatM Bool
isSafeToMapType (Mem{}) = flatError MemTypeFound
isSafeToMapType (Basic _) = return True
isSafeToMapType (Array{}) = return False

--------------------------------------------------------------------------------
