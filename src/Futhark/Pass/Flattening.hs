{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, ScopedTypeVariables, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
-- | This module performs flattening, which is a transformation aimed
-- at making data-level paralleisation easy.
--
-- It relies heavily on dark magic, so please familarise yourself with
-- this before trying to understand the code.
--
-- The main feature is that we can turn @map (scan + 0) xss@ into a
-- single data-parallel operation.
--
-- This is achived by representing @n@-dimensional array by a /flat/
-- (single dimensional) data array, and @n-1@ segment descriptors.
--
-- For example the array
-- @ [ [ [1,2] , [3,4,5] ]
--   , [ [6]             ]
--   , []
--   ]
-- @
--
-- Can be represented as
-- @ data  = [1,2, 3,4,5, 6    ]
--   seg_1 = [2,   3,     1    ]
--   seg_0 = [2,          1,  0]
-- @
--
-- If you are interested in knowing more details, see futher comments
-- in the source code!
module Futhark.Pass.Flattening ( flattenProg )
  where

import Control.Arrow (second)
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Text.PrettyPrint.Mainland as PP

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.Basic
import Futhark.Transform.Substitute
import Futhark.Tools
import Futhark.Pass

{- -----------------------------------------------------------------------------
                       Understanding the code / transformation

1. Segment descriptors

  If we have the variables
    cs = [[[char,k],n],m]
    xs = [[int,n],m]
    ys = [[int,k],n]

  @cs@ and @xs@ should share segments descriptor for the outer
  dimension (m,n), however @cs@ and @ys@ should /not/ share the segment
  descriptor for (k,n)!

  The way this is handled is by doing the following:
    xs = { [int,m]xs_seg0, [int,n*m]xs_data }

      added SegDescriptors
      [m*n] -> [xs_seg0]

    cs = { [int,m]xs_seg0, [int,m*n]cs_seg1, [char,m*n*k]cs_data }

      added SegDescriptors
      [m,n,k] -> [xs_seg0, cs_seg1]
      [m*n,k] -> [cs_seg1]

    ys = { [int,n]ys_seg0, [int,k]ys_data }

      added SegDescriptors
      [n,k] -> [ys_seg0]

1.b Data arrays

  Every array (also flat) points to its data array (through FlatState)

2. Overview of how the code works

  We transform function from the outside in. We go through each top level
  binding, transforming them so they do not use multidimensional arrays, but
  instead segment descriptors and data array. A @map f {xs_1,..,xs_n}@ will be
  transformed by specific rules, covered later. Most other types of bindings
  are not so bad. For example will @let [[char,n],m] ys = reshape (m,n) xs@
  need to be turned into

  @ let mn = m*n
    let [int,m] ys_seg0 = replicate m n
    let [char,mn] ys_data = reshape (mn) xs
  @

  ---

  To be continued ... FIXME

----------------------------------------------------------------------------- -}
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

  , typetab :: HM.HashMap VName Type

  , flattenedDims :: M.Map (SubExp,SubExp) SubExp

  , dataArrays :: M.Map VName Ident

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

instance PP.Pretty Regularity where
  ppr Regular = PP.text "Regular"
  ppr Irregular = PP.text "Irregular"

type SegDescp = (Ident, Regularity)


newtype FlatM a = FlatM (StateT FlatState (ExceptT Error (Writer FlatLog)) a)
                deriving ( MonadState FlatState
                         , MonadWriter FlatLog
                         , Monad, Applicative, Functor
                         )

instance MonadFreshNames FlatM where
  getNameSource = gets vnameSource
  putNameSource newSrc = modify $ \s -> s { vnameSource = newSrc }

instance HasTypeEnv FlatM where
  lookupType name = do
    val <- HM.lookup name <$> askTypeEnv
    case val of
      Nothing -> flatError $ Error $
                             "TypeEnv.lookupType: Name " ++ textual name ++
                             " not found in type environment."
      Just tp -> return tp
  askTypeEnv = gets typetab

----------------------------------------

runFlatM :: FlatState -> FlatM a -> Either (Error,FlatLog) a
runFlatM s (FlatM a) =
  let (res,flatlog) = runWriter $ runExceptT $ runStateT a s
  in case res of
       Left e -> Left (e, flatlog)
       Right (v,_) -> Right v

flatError :: Error -> FlatM a
flatError e = FlatM . lift $ throwError e

----------------------------------------

logMsg :: String -> FlatM ()
logMsg s = tell [LogMsg s]

--------------------------------------------------------------------------------

data Error = Error String
           | MemTypeFound
           | ArrayNoDims Ident

instance Show Error where
  show (Error msg) = msg
  show MemTypeFound = "encountered Mem as Type"
  show (ArrayNoDims i) = "found array without any dimensions " ++ pretty i

data FlatMsg = TransformsBinding [Binding] [Binding]
             | LogMsg String
             | StartFun Name
             | StartBnd String Binding
             deriving (Eq)

type FlatLog = [FlatMsg]

instance Show FlatMsg where
  show (TransformsBinding origbnds newbnds) =
    pretty origbnds ++ "=>" ++ pretty newbnds
  show (LogMsg msg) = msg
  show (StartFun name) = "Start function " ++ show name
  show (StartBnd fname bnd) =
    unwords [ "Start binding in", fname
            , "let", pretty $ bindingPattern bnd, "= ..."
            ]

prettyLog :: FlatLog -> PlainString
prettyLog flog = PlainString $ "\n" ++ unlines (map show flog)

newtype PlainString = PlainString String
instance Show PlainString where
  show (PlainString s) = s

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
  logMsg $ unwords ["addMapLetArray", pretty vn, "->", pretty letArr]
  letArrs <- gets mapLetArrays
  case M.lookup vn letArrs of
    (Just _) -> flatError $ Error $ "addMapLetArray: " ++
                                    pretty vn ++
                                    " already present in table"
    Nothing -> do
      let letArrs' = M.insert vn letArr letArrs
      modify (\s -> s{mapLetArrays = letArrs'})

----------------------------------------

addType :: VName -> Type -> FlatM ()
addType vn tp = do
  tt <- gets typetab
  case HM.lookup vn tt of
    (Just _) -> flatError $ Error $ "addType: " ++
                                    pretty vn ++
                                    " already present in table"
    Nothing -> do
      let tt' = HM.insert vn tp tt
      modify (\s -> s{typetab = tt'})

addTypeIdent :: Ident -> FlatM ()
addTypeIdent (Ident vn tp) = addType vn tp

addTypePatElem :: PatElem -> FlatM ()
addTypePatElem (PatElem i BindVar _) = addTypeIdent i
addTypePatElem pat =
  flatError . Error $ "addTypePatElem: cannot handle other than BindVar " ++
                      pretty pat

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

addFlattenedDims :: (SubExp, SubExp) -> SubExp -> FlatM ()
addFlattenedDims key val = do
  fds <- gets flattenedDims
  case M.lookup key fds of
    Just _ -> flatError $ Error $ "addFlattenedDims: " ++
                                  pretty key ++
                                  " already present in table"
    Nothing -> do
      let fds' = M.insert key val fds
      modify (\s -> s{flattenedDims = fds'})

----------------------------------------

getDataArray1 :: VName -> FlatM Ident
getDataArray1 vn = do
  da <- gets dataArrays
  case M.lookup vn da of
    Just val -> return val
    Nothing -> flatError $ Error $ "getDataArray1 not created for" ++
                                    show vn

addDataArray :: VName -> Ident -> FlatM ()
addDataArray key val = do
  logMsg $ unwords ["addDataArray", pretty key, "->", pretty val]
  da <- gets dataArrays
  case M.lookup key da of
    Just _ -> flatError $ Error $ "addDataArray: " ++
                                  pretty key ++
                                  " already present in table"
    Nothing -> do
      let da' = M.insert key val da
      -- Automatically make a data array point to itself
      let da'' = case M.lookup (identName val) da' of
                   Just _ -> da'
                   Nothing ->
                     M.insert (identName val) val da'
      modify (\s -> s{dataArrays = da''})

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
  logMsg $ unwords ["addSegDescriptors", pretty subs, pretty segs]
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
                   let segname = baseString vn ++ "_seg" ++ show n
                   seg <- newIdent segname (Array Int (Shape [dimouter]) Nonunique)
                   addTypeIdent seg
                   segsize <- createFlattenedDims (n+1) (dimouter, diminner)
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
             addFlattenedDims (outer,inner) new_subexp
             return new_subexp

createSegDescsForArray i =
  flatError . Error $ "createSegDescsForArray not multidimensional array: " ++
                      show i

setupDataArray :: Ident -> FlatM ()
setupDataArray (Ident vn tp@(Array _ (Shape [_]) _)) =
  addDataArray vn (Ident vn tp)
setupDataArray (Ident vn tp@(Array _ (Shape (dim0:dims)) _)) = do
  data_size <- foldM (curry getFlattenedDims1) dim0 dims
  let data_tp = setArrayDims tp [data_size]
  data_ident <- newIdent (baseString vn ++ "_data") data_tp
  addDataArray vn data_ident
  addTypeIdent data_ident
setupDataArray i =
  flatError . Error $ "setupDataArray on non array: " ++
                      show i

--------------------------------------------------------------------------------

flattenProg :: Pass Basic Basic
flattenProg =
  Pass { passName = "flattening"
       , passDescription = "Perform flattening transformation"
       , passFunction = liftEither .
                        either (Left . second prettyLog) Right .
                        flattenProg'
       }

flattenProg' :: Prog -> Either (Error,FlatLog) Prog
flattenProg' p@(Prog funs) = do
  let funs' = map renameToOld funs
  funsTrans <- mapM (runFlatM initState . transformFun) funs
  let Just main = funDecByName defaultEntryPoint p
  main' <- runFlatM initState $ transformMain main
  return $ Prog (main' : funsTrans ++ funs')
  where
    initState = FlatState { vnameSource = newNameSourceForProg p
                          , mapLetArrays = M.empty
                          , flattenedDims = M.empty
                          , dataArrays = M.empty
                          , segDescriptors = M.empty
                          , typetab = HM.empty
                          }
    renameToOld (FunDec name retType params body) =
      FunDec name' retType params body
      where
        name' = nameFromString $ nameToString name ++ "_orig"

--------------------------------------------------------------------------------

-- | @convertToFlat idents@ transforms multidimensional arrays into
-- their flat representation. Used when calling functions. Returns
-- size- and array idents seperately.
convertToFlat :: [Ident] -> FlatM ([Ident],[Ident])
convertToFlat idents = do
  --let orig_arr_sizes = concat $ mapMaybe (variableDimensionsAsIdents . identType) idents
  --let idents' = filter (`notElem` orig_arr_sizes) idents

  (sizes, arrs) <- liftM unzip $ forM idents $ \i@(Ident vn tp) ->
    case dimentionality tp of
      Nothing -> return ([], [i])
      Just 1 -> do
        dataarr <- getDataArray1 vn
        return ([], [dataarr])
      Just _ -> do
        (segdescp_arrs,_) <- liftM unzip $ getSegDescriptors1Ident i
        dataarr <- getDataArray1 vn
        let arrs = segdescp_arrs ++ [dataarr]
        let sizes = mapMaybe ( (\case
                                  [Var szvn] -> Just $ Ident szvn (Basic Int)
                                  _ -> Nothing
                               ) . arrayDims . identType)
                             (drop 1 arrs)
        return (sizes, arrs)

  return (L.nub $ concat sizes, L.nub $ concat arrs)

convertExtRetType :: ExtRetType -> ExtRetType
convertExtRetType (ExtRetType ex_tps) =
  let tmp = concatMap convertExtType ex_tps
  in ExtRetType $ zipWith changeToUniqFree tmp [0..]
  where
    -- If we already know the sizes of the resulting segment
    -- descriptos, use those otherwise just create a not of free
    -- stuff.
    --
    -- If we know input is [[int,n],m] and output is [[int,m],n] we
    -- are not able to keep that information, and will generate
    -- [[int,?1],?0]
    --
    -- However, input [[[int,x],y],z] and output [[int,x],y] should
    -- work TODO ^^ not implemented
    convertExtType (Array bt (ExtShape extshps) uniq) =
      let segs = replicate (length extshps -1)
                           (Array Int (ExtShape [Ext 0]) Nonunique)
          datatp = Array bt (ExtShape [Ext 0]) uniq
      in segs ++ [datatp]
    convertExtType tp = [tp]

    changeToUniqFree (Array bt _ uniq) free =
      Array bt (ExtShape [Ext free]) uniq
    changeToUniqFree tp _ = tp


-- | Transform the entry function so it converts array arguments to
-- flat representation, calls the flattening transformed version, and
-- finally converts the flat representation back to normal
-- representation
transformMain :: FunDec -> FlatM FunDec
transformMain (FunDec name (ExtRetType rettypes) params _) = do
  let arr_params = filter (maybe False (>=2) . dimentionality . identType)
                          (map paramIdent params)

  mapM_ createSegDescsForArray arr_params
  mapM_ setupDataArray $ filter (isJust . dimentionality . identType)
                                (map paramIdent params)

  multandsegs_bnds <- mkMultAndSegs arr_params
  reshape_bnds <- mapM mkDataReshape arr_params

  flat_params <-
    liftM (uncurry (++)) $ convertToFlat $ map paramIdent params
  let flat_args = map ((\vn -> (Var vn, Observe)) . identName) flat_params
  let (ExtRetType flatex_tps) = convertExtRetType (ExtRetType rettypes)
  let call_exp = Apply (transformFunName name)
                       flat_args
                       (ExtRetType flatex_tps)
  (flatrestps, extra_sizes) <- instantiateShapes' flatex_tps
  callresidents <- mapM (newIdent "tmpres") flatrestps
  let callpat = patternFromIdents extra_sizes callresidents
  let call_bnd = Let callpat () call_exp

  (realrestps, _) <- instantiateShapes' rettypes
  realresidents <- mapM (newIdent "res") realrestps
  -- This groups the flattened results returned from the function
  -- call, putting segment descriptors and data arrays together in one group
  let callresidents_grouped =
        reverse $ snd $
          foldl (\(takefrom,res) n -> (drop n takefrom, take n takefrom : res))
                (callresidents,[])
                (map (fromMaybe 1 . dimentionality) realrestps)
  fromflat_bnds <-
    liftM concat $ zipWithM fromFlat realresidents callresidents_grouped

  let body' = Body { bodyLore = ()
                   , bodyBindings = multandsegs_bnds ++ reshape_bnds ++
                                    [call_bnd] ++ fromflat_bnds
                   , bodyResult = map (Var . identName) realresidents
                   }
  return $ FunDec name (ExtRetType rettypes) params body'

  where
    -- | @mkMultAndSegs [idents]@ creates the bindings for initializing the
    -- segment descriptors (and their size variable) belonging to
    -- @ident \in idents@. Must be called on a multidimensional array.
    --
    -- Makes sure to /only/ create each binding /once/.
    mkMultAndSegs :: [Ident] -> FlatM [Binding]
    mkMultAndSegs arr_params = do
      (all_mult_bnds, all_segbnds) <- liftM unzip $
        forM arr_params $ \ident -> do
          let dim0:dims = arrayDims $ identType ident
          mult_bnds <-
            liftM (reverse . concat . snd) $ foldM makeMult (dim0, []) dims
          segdescps <- getSegDescriptors1Ident ident
          rep_bnds <-
            liftM reverse $ foldM makeRep [] $ zip dims segdescps
          return (mult_bnds, rep_bnds)
      return $ L.nub (concat all_mult_bnds) ++
               L.nub (concat all_segbnds)
      where


        -- | Folding function which creates a binding for
        -- @'getFlattenedDims1 (outer,inner)@ by multiplying @outer@
        -- and @inner@
        makeMult (outer, res) inner = do
          merged <- getFlattenedDims1 (outer,inner)
          let merge_bnd =
                case merged of
                  Constant _ -> Nothing
                  Var vn ->
                    let i = Ident vn (Basic Int)
                        i_exp = PrimOp $ BinOp Times outer inner Int
                        i_bnd = Let (Pattern [] [PatElem i BindVar ()]) () i_exp
                    in Just i_bnd
          return (merged, catMaybes [merge_bnd] : res)

        makeRep res (content, (seg, Regular)) = do
          let [count] = arrayDims $ identType seg
          let rep_exp = PrimOp $ Replicate count content
          let bnd = Let (Pattern [] [PatElem seg BindVar()]) () rep_exp
          return $ bnd : res
        makeRep _ _ =
          flatError $ Error "transformMain, makeRep: SegDescriptor was not Regular"

    -- | @mkDataReshape ident@ created the @reshape@ for the data array.
    mkDataReshape :: Ident -> FlatM Binding
    mkDataReshape (Ident vn _) = do
      data_ident <- getDataArray1 vn
      let [data_size] = arrayDims $ identType data_ident
      let data_exp = PrimOp $ Reshape [] [DimNew data_size] vn
      let data_bnd = Let (Pattern [] [PatElem data_ident BindVar()]) () data_exp
      return data_bnd

    -- | @fromFlat res tmps@ create a binding for the result to be
    -- returned @res@, from the arrays (segs + data) in @tmps@. For
    -- multidimentional arrays, this will end in a 'Reshape'
    fromFlat :: Ident -> [Ident] -> FlatM [Binding]
    fromFlat _ [] =
      error "transformMain fromFlat: callresidents_grouped gave an empty list"
    fromFlat res [Ident vn (Array _ (Shape [dim0]) _)] = do
      let resdims = arrayDims $ identType res
      case resdims of
        [Var szvn] -> do
          let sze = PrimOp $ SubExp dim0
          let szpat = patternFromIdents [] [Ident szvn (Basic Int)]
          let szbnd = Let szpat () sze
          let reshape_e = PrimOp $ Reshape [] [DimNew $ Var szvn] vn
          let reshape_pat = patternFromIdents [] [res]
          let reshape_bnd = Let reshape_pat () reshape_e
          return [szbnd, reshape_bnd]
        [Constant _] ->
          return []
        _ -> flatError $ Error "transformMain fromFlat, trying to bind a multidimensional array to a 1D array"
    fromFlat res [Ident vn (Basic _)] = do
      let e = PrimOp $ SubExp $ Var vn
      return [Let (Pattern [] [PatElem res BindVar ()]) () e]
    fromFlat res tmps = do
      let (segs,dataarr) = (\(d:sgs) -> (reverse sgs, d)) $ reverse tmps
      let [datasize] = arrayDims $ identType dataarr

      cmp_i <- newIdent "cmp" (Basic Bool)
      let cmp_exp = PrimOp $ BinOp Equal datasize (Constant $ IntVal 0) Bool
      let cmp_bnd = Let (Pattern [] [PatElem cmp_i BindVar ()]) () cmp_exp

      let resdimidents = mapMaybe (\case
                                      Var vn -> Just $ Ident vn (Basic Int)
                                      Constant _ -> error "transformMain, fromFlat: size of a result array was constant"
                                  ) (arrayDims $ identType res)
      let truebody = Body { bodyLore = ()
                          , bodyBindings = []
                          , bodyResult = replicate (length resdimidents)
                                                   (Constant $ IntVal 0)
                          }
      falsebody <- createFalseBody segs
      let if_exp = If (Var $ identName cmp_i)
                   truebody
                   falsebody
                   (replicate (length resdimidents) (Basic Int))
      let if_bnd = Let (patternFromIdents [] resdimidents) () if_exp

      let reshape_e = PrimOp $ Reshape [] (map DimNew $ arrayDims $ identType res) (identName dataarr)
      let reshape_bnd = Let (Pattern [] [PatElem res BindVar ()]) () reshape_e

      return [cmp_bnd, if_bnd, reshape_bnd]
      where
        createFalseBody :: [Ident] -> FlatM Body
        createFalseBody [] =
          flatError $ Error "transformMain, createFalseBody: called on empty array"
        createFalseBody (seg0:segs) = do
          tmpids <- mapM (\_ -> newIdent "tmp" (Basic Int)) (seg0:segs)
          let bnds :: [Binding] =
                zipWith (\i seg -> Let (Pattern [] [PatElem i BindVar()]) ()
                                       (PrimOp $ Index [] (identName seg)
                                                       [Constant $ IntVal 0])
                        )  tmpids (seg0:segs)
          let [seg0sz] = arrayDims $ identType seg0
          return Body { bodyLore = ()
                      , bodyBindings = bnds
                      , bodyResult = seg0sz : map (Var . identName) tmpids
                      }

--------------------------------------------------------------------------------

transformFunName :: Name -> Name
transformFunName name = nameFromString $ nameToString name ++ "_flattrans"

transformFun :: FunDec -> FlatM FunDec
transformFun (FunDec name rettype params body) = do
  tell [StartFun name]
  mapM_ (addTypeIdent . paramIdent) params
  mapM_ createSegDescsForArray $
    filter (maybe False (>=2) . dimentionality . identType)
           (map paramIdent params)
  mapM_ setupDataArray $ filter (isJust . dimentionality . identType)
                                (map paramIdent params)
  body' <- transformBody body
  idents' <- liftM (uncurry (++)) $ convertToFlat $ map paramIdent params
  let params' = map (\i -> Param { paramIdent = i
                                 , paramLore = ()
                                 }
                    ) idents'
  return $ FunDec name' rettype' params' body'
  where
    rettype' = convertExtRetType rettype
    name' = transformFunName name

transformBody :: Body -> FlatM Body
transformBody (Body () bindings ses) = do
  bindings' <- concat <$> mapM transformBinding bindings
  ses' <- concat <$> mapM subexpToFlatrepresentation ses
  return $ Body () bindings' ses'
  where
    subexpToFlatrepresentation (Constant v) =
      return [Constant v]
    subexpToFlatrepresentation (Var vn) = do
      tp <- lookupType vn
      (_,arrs) <- convertToFlat [Ident vn tp]
      return $ map (Var . identName) arrs

-- | Transform a function to use parallel operations.
-- Only maps needs to be transformed, @map f xs@ ~~> @f^ xs@
transformBinding :: Binding -> FlatM [Binding]
transformBinding topBnd@(Let (Pattern [] pats) ()
                             (LoopOp (Map certs w lambda arrs))) = do
  tell [StartBnd "transformBinding" topBnd]
  -- Checking if a Variable use is safe requires knowledge of the type
  -- Consider writing isSafeToMap??? differently. TODO
  mapM_ addTypePatElem $ concatMap (patternElements . bindingPattern) lamBnds
  mapM_ (addTypeIdent . paramIdent) $ lambdaParams lambda

  okLamBnds <- mapM isSafeToMapBinding lamBnds
  let grouped = foldr group [] $ zip okLamBnds lamBnds

  arrtps <- mapM lookupType arrs
  outerSize <- case arrtps of
                 Array _ (Shape (outer:_)) _:_ -> return outer
                 _ -> flatError $ Error "impossible, map argument was not a list"

  case grouped of
   [Right _] -> do
     forM_ pats $ \(PatElem i _ ()) -> do
       addTypeIdent i
       addDataArray (identName i) i
     arrs' <- mapM (liftM identName . getDataArray1) arrs
     return [Let (Pattern [] pats) ()
                 (LoopOp (Map certs w lambda arrs'))]
   _ -> do
     loopinv_arrvns <-
       filter (`notElem` arrs) <$> filterM (liftM isJust . vnDimentionality)
       (HS.toList $ freeInExp (LoopOp $ Map certs w lambda arrs))
     unless (null loopinv_arrvns) $
       flatError $ Error $ "We only handle intermediate results currently" ++
                           "but these where loop invariant: " ++
                           pretty loopinv_arrvns
--     (loopinv_repbnds, loopinv_repidents) <-
--       mapAndUnzipM (replicateVName outerSize) loopinv_vns

     let mapInfo = MapInfo { mapListArgs = arrs --FIXME ++ map identName loopinv_repidents
                           , lamParamVNs = lambdaParamVNs --FIXME ++ loopinv_vns
                           , mapLets = letBoundIdentsInLambda
                           , mapSize = outerSize
                           , mapCerts = certs
                           }

     let mapResNeed = HS.unions $ map freeIn $ bodyResult $ lambdaBody lambda
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
                Right bnds -> do
                  mapbnd <- wrapRightInMap mapInfo bndInfo bnds
                  tell [bnds `TransformsBinding` [mapbnd]]
                  return $ Right mapbnd
                Left bnd ->  do
                  tell [StartBnd "pullOutOfMap" bnd]
                  bnd' <- pullOutOfMap mapInfo bndInfo bnd
                  tell [[bnd] `TransformsBinding` bnd']
                  return $ Left bnd'
       ) grouped (zip argsNeeded shouldReturn)

     res' <- forM (bodyResult $ lambdaBody lambda) $
             \se -> case se of
                      (Constant bv) -> return $ Constant bv
                      (Var vn) -> Var <$> identName <$> getMapLetArray' vn

     zipWithM_ fixDataArrStuff pats res'

     return $ --FIXME loopinv_repbnds ++
              concatMap (either id (: [])) grouped'

  where
    fixDataArrStuff (PatElem (Ident patvn pattp) BindVar ()) (Var resvn) = do
      addType patvn pattp
      addDataArray patvn =<< getDataArray1 resvn
    fixDataArrStuff _ (Constant _) = return ()
    fixDataArrStuff _ _ =
      flatError $ Error  "transformBinding(map) fixDataArrStuff: BindVar used"

    lamBnds = bodyBindings $ lambdaBody lambda
    letBoundIdentsInLambda =
      concatMap (map (identName . patElemIdent) . patternElements . bindingPattern)
                (bodyBindings $ lambdaBody lambda)

    lambdaParamVNs = map paramName $ lambdaParams lambda

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
                return $ Just (Param (Ident arg val_tp) (), identName val)
              Nothing -> return Nothing
      argarrs' <- mapM (liftM identName . getDataArray1) argarrs

      pat <- liftM (Pattern [] . map (\i -> PatElem i BindVar () ))
             $ forM toreturn_vns $ \sr -> do
                 iarr <- wrapInArrVName (mapSize mapInfo) sr
                 addMapLetArray sr iarr
                 addDataArray (identName iarr) iarr
                 return iarr

      let lambody = Body { bodyLore = ()
                         , bodyBindings = bnds
                         , bodyResult = map Var toreturn_vns
                         }

      toreturn_tps <- mapM lookupType toreturn_vns
      let wrappedlambda = Lambda { lambdaIndex = lambdaIndex lambda
                                 , lambdaParams = mapparams
                                 , lambdaBody = lambody
                                 , lambdaReturnType = toreturn_tps
                                 }

      let theMapExp = LoopOp $ Map certs w wrappedlambda argarrs'
      return $ Let pat () theMapExp

transformBinding topbnd@(Let (Pattern [] [PatElem resident BindVar ()]) ()
                        (PrimOp (Reshape certs (dim0_c:dims_c) reshapearr))) = do
  let dim0 = newDim dim0_c
      dims = map newDim dims_c
  tell [StartBnd "transformBinding" topbnd]
  -- FIXME: For the case where reshape is only to check map sizes,
  -- where will those certifications go once we remove this reshape?
  -- ... shouldn't the reshape certifications be on the map as well in
  -- the first place? (ie, more than in one place)

  extra_bnds <-
    case dims of
      [] -> return []
      _ -> do
        hackyhacky <- liftM (reverse . snd) $
          foldM (\(m_outer,res) inner -> case m_outer of
                                     Nothing -> return (Nothing, Nothing:res)
                                     Just outer -> do
                                       merged <- getFlattenedDims (outer,inner)
                                       return (merged, merged:res))
                (Just dim0, []) dims
        createSegDescsForArray resident
        liftM (reverse . concat . snd) $ foldM makeMult (dim0, [])
                                                        (zip dims hackyhacky)


   -- FIXME: add segment descriptor creation bindings

  addTypeIdent resident
  (Ident data_identvn data_identtp) <- getDataArray1 reshapearr

  newdata_size <- foldM (curry getFlattenedDims1) dim0 dims
  let newdata_tp = setArrayDims data_identtp [newdata_size]
  newdata_ident <-
    newIdent (baseString (identName resident) ++ "_data") newdata_tp
  addTypeIdent newdata_ident
  addDataArray (identName resident) newdata_ident

  let reshape_exp' = PrimOp $ Reshape certs [DimNew newdata_size] data_identvn
  let reshape_bnd = Let (Pattern [] [PatElem newdata_ident BindVar ()]) () reshape_exp'
  return $ extra_bnds ++ [reshape_bnd]
  where
    makeMult (_, res) (_, Just oldmerged) = return (oldmerged, res)
    makeMult (outer, res) (inner, Nothing) = do
      merged <- getFlattenedDims1 (outer,inner)
      let merge_bnd =
            case merged of
              Constant _ -> Nothing
              Var vn ->
                let i = Ident vn (Basic Int)
                    i_exp = PrimOp $ BinOp Times outer inner Int
                    i_bnd = Let (Pattern [] [PatElem i BindVar ()]) () i_exp
                in Just i_bnd
      return (merged, catMaybes [merge_bnd] : res)


transformBinding topbnd@(Let pat () (LoopOp (Redomap certs w lam1 lam2 accs arrs))) = do
  tell [StartBnd "transformBinding" topbnd]
  (map_bnd, red_bnd) <-
    redomapToMapAndReduce pat () (certs, w, lam1, lam2, accs, arrs)
  map_bnd' <- transformBinding map_bnd
  red_bnd' <- transformBinding red_bnd
  return $ map_bnd' ++ red_bnd'

transformBinding bnd@(Let (Pattern _ pats) () _) = do
  tell [StartBnd "transformBinding" bnd]
  -- FIXME: magically construct segment descriptors as needed
  mapM_ addTypePatElem pats
  return [bnd]

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
                  (Let (Pattern [] [PatElem resident BindVar ()]) ()
                       (PrimOp (Reshape certs dimses_c reshapearr))) = do
  let dimses = newDims dimses_c
  Just target <- findTarget mapInfo reshapearr

  loopdep_dim_subexps <- filterM (\case
                                  Var i -> liftM isJust $ findTarget mapInfo i
                                  Constant _ -> return False
                               ) dimses

  unless (null loopdep_dim_subexps) $
    flatError $ Error $ "pullOutOfMap Reshape: loop dependant variable used " ++
                       show (map pretty loopdep_dim_subexps) ++
                       " ^ TODO: implement SegReshape thingy"

  newresident <-
    case resident of
      (Ident vn (Array bt (Shape shpdms) uniq)) -> do
        vn' <- newID (baseName vn)
        return $ Ident vn' (Array bt (Shape (mapSize mapInfo:shpdms)) uniq)
      _ -> flatError $ Error "impossible, result of reshape not list"

  addMapLetArray (identName resident) newresident

  let reshape_exp = PrimOp $ Reshape (certs ++ mapCerts mapInfo)
                                     (map DimNew $ mapSize mapInfo:dimses) $
                    identName target
  let reshape_bnd = Let (Pattern [] [PatElem newresident BindVar ()]) () reshape_exp
  -- TODO: this trick probably only works for regular arrays
  -- ... unless we specifically create the segment descriptors
  -- beforehand, then it will probably work
  transformBinding reshape_bnd

pullOutOfMap mapInfo (argsneeded, _)
                     (Let (Pattern [] pats) ()
                          (LoopOp (Map certs w lambda arrs))) = do
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

  flatIdents <-
    mapM (flattenArg mapInfo) $
                 (Right <$> okIdents) ++ (Left <$> loopInvIdentsArrs)

  -- We need this later on
  arrs_tps <- mapM lookupType arrs
  innerMapSize <- case arrs_tps of
                    Array _ (Shape (outer:_)) _:_ -> return outer
                    _ -> flatError $ Error "impossible, map argument was not a list"
  [innerMapSeg] <- getSegDescriptors1 [mapSize mapInfo, innerMapSize]

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
  let intmres_params = map (`Param` ()) $ zipWith Ident intmres_vns' intmres_tps
  intmres_arrs <- mapM (findTarget1 mapInfo) intmres_vns

  --
  -- Distribute and flatten idents needed (from above)
  --
  (distBnds, flatDistArrIdents) <-
    mapAndUnzipM (distributeExtraArg innerMapSize innerMapSeg) intmres_arrs

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

  -- FIXME: Should call transformBinding on inner before calling unflattenRes
  pats' <- mapM unflattenRes pats

  let mapBnd' = Let (Pattern [] pats') ()
                    (LoopOp (Map (certs ++ mapCerts mapInfo)
                                 w
                                 lambda'
                                 (map identName newInnerIdents)))

  mapBnd'' <- transformBinding mapBnd'

  zipWithM_ evenMoreExtraSutff pats pats'

  return $ concat distBnds ++
           loopInvRepBnds ++
           mapBnd''


  where
    -- | The inner map apparently depends on some variable that does
    -- not come from the lists mapped over, so we'll need to add that
    --
    -- 1st arg is "size" of the inner map
    -- 2nd arg is the segment descriptor for the sizes of the inner map
    distributeExtraArg :: SubExp -> (Ident,Regularity) -> Ident -> FlatM ([Binding], Ident)
    distributeExtraArg sz (Ident counts _,_) i@(Ident vn tp) = do
      dist_tp <- case tp of
                 Mem{} -> flatError MemTypeFound
                 Array _ (Shape []) _ -> flatError $ ArrayNoDims i
                 (Basic _) ->
                   flatError $
                       Error $ "distributeExtraArg: " ++
                               "trying to distribute basic type"
                 (Array bt (Shape (out:rest)) uniq) -> do
                   when (out /= mapSize mapInfo) $
                     flatError $
                       Error $ "distributeExtraArg: " ++
                               "trying to distribute array with incorrect outer size " ++
                               pretty i
                   unless (null rest) $
                     flatError $ Error "distributeExtraArg: TODO: handle replicating arrays"
                   newsz <- getFlattenedDims1 (out,sz)
                   return $ Array bt (Shape $ newsz:rest) uniq

      distident <- newIdent (baseString vn ++ "_dist") dist_tp
      let [flatsz] = arrayDims dist_tp
      addTypeIdent distident
      addDataArray (identName distident) distident
      dataarr <- getDataArray1 vn

      tmpsz_ident <- newIdent "size" (Basic Int)
      let tmpident_tp = setArrayDims dist_tp [Var $ identName tmpsz_ident]
      tmpident <- newIdent "tmp" tmpident_tp

      let segrep_exp = SegOp $ SegReplicate [] counts (identName dataarr) Nothing
      let segrep_bnd = Let (patternFromIdents [tmpsz_ident] [tmpident])
                           () segrep_exp
      let reshape_exp = PrimOp $ Reshape [] [DimNew flatsz] (identName tmpident)
      let reshape_bnd = Let (patternFromIdents [] [distident]) () reshape_exp
      return ([segrep_bnd, reshape_bnd], distident)

    -- | Steps for exiting a nested map, meaning we step-up/unflatten the result
    unflattenRes :: PatElem -> FlatM PatElem
    unflattenRes (PatElem (Ident vn (Array bt (Shape (outer:rest)) uniq))
                          BindVar ()) = do
      flatSize <- getFlattenedDims1 (mapSize mapInfo, outer)
      let flatTp = Array bt (Shape $ flatSize:rest) uniq
      flatResArr <- newIdent (textual vn ++ "_sd") flatTp
      let flatResArrPat = PatElem flatResArr BindVar ()
      let finalTp = Array bt (Shape $ mapSize mapInfo :outer:rest) uniq
      finalResArr <- newIdent (baseString vn) finalTp
      addTypeIdent finalResArr
      addMapLetArray vn finalResArr
      return flatResArrPat
    unflattenRes pe = flatError $ Error $ "unflattenRes applied to " ++ pretty pe

    evenMoreExtraSutff :: PatElem -> PatElem -> FlatM ()
    evenMoreExtraSutff (PatElem (Ident origvn _) BindVar ())
                       (PatElem resident BindVar ()) = do
      --addMapLetArray origvn resident
      -- FIXME: create seg descps if they do not already exists ...?
      logMsg $ unwords ["evenMoreExtraSutff", pretty origvn, pretty resident]
      data_arr <- getDataArray1 $ identName resident
      fakemutlidimarray <- getMapLetArray' origvn
      addDataArray (identName fakemutlidimarray) data_arr
    evenMoreExtraSutff pe1 pe2 =
      flatError $ Error $ "evenMoreExtraSutff applied to " ++ pretty pe1 ++
                          " , " ++ pretty pe2


pullOutOfMap mapInfo _
                     topBnd@(Let (Pattern [] pats) letlore
                                 (LoopOp (Reduce certs w lambda args))) = do
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
      addDataArray (identName arr) arr
      return $ PatElem arr BindVar ()

    -- FIXME: This does not handle completely loop invariant things
    flatargs <-
      forM args $ \(ne,i) -> do
        flatarr <- flattenArg mapInfo $ Right i
        return (ne, identName flatarr)

    seginfo <- case args of
                 (_,argarr):_ ->
                   getSegDescriptors1Ident =<< findTarget1 mapInfo argarr
                 [] -> flatError $ Error "Reduce on empty array, not possible"
    segdescp <- case seginfo of
                  [(descp,_)] -> return $ identName descp
                  _ -> flatError $ Error $ "pullOutOfMap Reduce: argument array was not two dimensional"
                                         ++ pretty topBnd

    let redBnd' = Let (Pattern [] pats') letlore
                      (SegOp (SegReduce certs w lambda flatargs segdescp))

    return [redBnd']

pullOutOfMap mapinfo _ (Let (Pattern [] [PatElem resident BindVar ()]) ()
                            (PrimOp (Iota subexp))) = do
  segdescp <- case subexp of
                Constant _ ->
                  flatError $ Error "FIXME: replicate that son of a bitch"
                Var ident ->
                  findTarget1 mapinfo ident

  (segsum, sumbnd) <- getFlattenedDims (mapSize mapinfo, subexp) >>= (\mbi ->
    case mbi of
      Just i -> return (i, [])
      Nothing -> do
        sumident <- newIdent "segiota_sum" (Basic Int)
        addTypeIdent sumident
        sumexp <- reducePlus (arraySize 0 $ identType segdescp) (identName segdescp)
        let sumbnd = Let (Pattern [] [PatElem sumident BindVar ()]) () sumexp

        addFlattenedDims (mapSize mapinfo, subexp) (Var $ identName sumident)
        addSegDescriptors [mapSize mapinfo, subexp] [(segdescp, Irregular)]

        return (Var $ identName sumident, [sumbnd]))

  let tmptp = Array Int (Shape [segsum]) Nonunique

  repident <- newIdent "segiota_rep" tmptp
  let repexp = PrimOp $ Replicate segsum (Constant $ IntVal 1)
  let repbnd = Let (Pattern [] [PatElem repident BindVar ()]) () repexp

  scanident <- newIdent "segiota_segscan" tmptp
  scanexp <- segscanPlus (arraySize 0 $ identType repident)
             ScanExclusive (identName repident) (identName segdescp)
  let scanbnd = Let (Pattern [] [PatElem scanident BindVar ()]) () scanexp

  resarr <- newIdent (baseString (identName resident) ++ "_arr")
                     (Array Int (Shape [mapSize mapinfo, subexp]) Nonunique)

  addMapLetArray (identName resident) resarr
  addTypeIdent resarr
  addDataArray (identName resarr) scanident
  addTypeIdent scanident

  return $ sumbnd ++ [repbnd, scanbnd]


pullOutOfMap mapinfo bndinfo
             (Let pat () (LoopOp (Redomap certs w lam1 lam2 accs arrs))) = do
  -- Remember that reduce function must be @a -> a -> a@
  -- This means that the result of the map must be of type @a@.
  (map_bnd, red_bnd) <-
    redomapToMapAndReduce pat () (certs, w, lam1, lam2, accs, arrs)

  let newidents = patternValueIdents $ bindingPattern map_bnd
  mapM_ addTypeIdent newidents
  let mapinfo' = mapinfo{ mapLets = mapLets mapinfo ++ map identName newidents }
  map_bnds' <- pullOutOfMap mapinfo' bndinfo map_bnd
  red_bnds' <- pullOutOfMap mapinfo' bndinfo red_bnd
  return $ map_bnds' ++ red_bnds'

pullOutOfMap mapinfo _ (Let (Pattern [] [PatElem ident1 BindVar _]) _
                        (PrimOp (SubExp (Var name)))) = do
  target <- findTarget1 mapinfo name
  addMapLetArray (identName ident1) target
  addDataArray (identName ident1) =<< getDataArray1 (identName target)
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
flattenArg :: MapInfo -> Either Ident VName -> FlatM Ident
flattenArg mapInfo targInfo = do
  target <- case targInfo of
              Left targ -> return targ
              Right innerMapArg -> findTarget1 mapInfo innerMapArg
  logMsg $ unwords ["flattenArg", "target=",pretty target, "targetInfo=",show targInfo]

  -- tod = Target Outer Dimension
  (tod1, tod2, rest, bt, uniq) <- case target of
    (Ident _ (Array bt (Shape (tod1:tod2:rest)) uniq)) ->
              return (tod1, tod2, rest, bt, uniq)
    _ -> flatError $ Error $ "trying to flatten less than 2D array: " ++ pretty target

  newsize <- getFlattenedDims1 (tod1, tod2)

  case rest of
    [] -> getDataArray1 $ identName target
    _ -> do let flatTp = Array bt (Shape (newsize : rest)) uniq
            i <- newIdent (baseString (identName target) ++ "_sd") flatTp
            logMsg $ unwords ["flattenArg", "created new ident", pretty i]
            addTypeIdent i
            addDataArray (identName i) =<< getDataArray1 (identName target)
            return i



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
    Mem {} -> flatError MemTypeFound
  i <- newIdent (baseString vn ++ "_arr") arrtp
  addTypeIdent i
  return i

replicateIdent :: SubExp -> Ident -> FlatM (Binding, Ident)
replicateIdent sz i = do
  arrRes <- wrapInArrIdent sz i
  let repExp = PrimOp $ Replicate sz $ Var $ identName i
      repBnd = Let (Pattern [] [PatElem arrRes BindVar ()]) () repExp
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

--------------------------------------------------------------------------------

segscanPlus :: SubExp -> ScanType -> VName -> VName -> FlatM Exp
segscanPlus w st arr segdescp = do
  lambda <- binOpLambda Plus Int
  return $ SegOp $ SegScan [] w st lambda [(Constant $ IntVal 0, arr)] segdescp

reducePlus :: SubExp -> VName -> FlatM Exp
reducePlus w arr = do
  lambda <- binOpLambda Plus Int
  return $ LoopOp $ Reduce [] w lambda [(Constant $ IntVal 0, arr)]

--------------------------------------------------------------------------------

patternFromIdents :: [Ident] -> [Ident] -> Pattern
patternFromIdents ctx vals =
  Pattern (map addBindVar ctx) (map addBindVar vals)
  where addBindVar i = PatElem i BindVar ()

--------------------------------------------------------------------------------
