{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.CodeGen.ImpGen
  ( compileProg
  , compileProgSimply
  -- * Pluggable compiler
  , ExpCompiler
  , ExpCompilerResult (..)
  -- * Monadic compiler interface
  , ImpM
  )
  where

import Control.Applicative
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Writer

import Data.Either
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Data.List

import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Representation.ExplicitMemory
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun
import Futhark.MonadFreshNames

-- | A substitute expression compiler, tried before the main
-- expression compilation function.
type ExpCompiler op = [VName] -> Exp -> ImpM op (ExpCompilerResult op)

-- | The result of the substitute expression compiler.
data ExpCompilerResult op =
      CompileBindings [Binding]
    -- ^ New bindings.  Note that the bound expressions will
    -- themselves be compiled using the expression compiler.
    | CompileExp Exp
    -- ^ A new expression (or possibly the same as the input) - this
    -- will not be passed back to the expression compiler, but instead
    -- processed with the default action.
    | Done
    -- ^ Some code was added via the monadic interface.

-- | When an array is declared, this is where it is stored.
data MemLocation = MemLocation
                   VName -- ^ Name of memory block.
                   Imp.DimSize -- ^ Offset into block.
                   deriving (Show)

data ArrayEntry = ArrayEntry {
    entryArrayLocation :: MemLocation
  , entryArrayElemType :: BasicType
  , entryArrayShape :: [Imp.DimSize]
  }

data MemEntry = MemEntry {
    entryMemSize :: Imp.DimSize
  }

-- ^ Every non-scalar variable must be associated with an entry.
data VarEntry = ArrayVar ArrayEntry
              | MemVar MemEntry

-- ^ When compiling a body, this is a description of where the result
-- should end up.
newtype Destination = Destination [ValueDestination]

data ValueDestination = ScalarDestination VName
                      | MemoryDestination VName (Maybe VName)
                      | ArrayDestination ArrayMemoryDestination [Maybe VName]

data ArrayMemoryDestination = SetMemory VName (Maybe VName)
                            | CopyIntoMemory VName

data Env op = Env {
    envVtable :: HM.HashMap VName VarEntry
  , envExpCompiler :: ExpCompiler op
  }

newEnv :: ExpCompiler op -> Env op
newEnv ec = Env { envVtable = HM.empty
                , envExpCompiler = ec
                }

newtype ImpM op a = ImpM (RWST (Env op) (Imp.Code op) VNameSource (Either String) a)
  deriving (Functor, Applicative, Monad,
            MonadState VNameSource,
            MonadReader (Env op),
            MonadWriter (Imp.Code op))

instance MonadFreshNames (ImpM op) where
  getNameSource = get
  putNameSource = put

runImpM :: ImpM op a -> ExpCompiler op -> VNameSource -> Either String (a, VNameSource, Imp.Code op)
runImpM (ImpM m) = runRWST m . newEnv

-- bad :: String -> ImpM op a

collect :: ImpM op () -> ImpM op (Imp.Code op)
collect m = pass $ do
  ((), code) <- listen m
  return (code, const mempty)

mapAccumLM :: Monad m =>
              (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumLM _ acc [] = return (acc, [])
mapAccumLM f acc (x:xs) = do
  (acc', x') <- f acc x
  (acc'', xs') <- mapAccumLM f acc' xs
  return (acc'', x':xs')

compileProg :: ExpCompiler op -> Prog -> Either String (Imp.Program op)
compileProg ec prog =
  Imp.Program <$> snd <$> mapAccumLM (compileFunDec ec) src (progFunctions prog)
  where src = newNameSourceForProg prog

-- | 'compileProg' with an 'ExpCompiler' that always returns 'CompileExp'.
compileProgSimply :: Prog -> Either String (Imp.Program op)
compileProgSimply = compileProg $ const $ return . CompileExp

compileInParam :: FParam -> ImpM op (Either Imp.Param ArrayDecl)
compileInParam fparam = case t of
  Basic bt -> return $ Left $ Imp.ScalarParam name bt
  Mem size -> Left <$> Imp.MemParam name <$> subExpToDimSize size
  Array bt shape _ -> do
    unless (IxFun.isLinear ixfun) $
      fail "Can only handle linear (simple) allocation for now."
    shape' <- mapM subExpToDimSize $ shapeDims shape
    return $ Right $ ArrayDecl name bt shape' $
      MemLocation (identName mem) $ Imp.ConstSize 0
  where name = bindeeName fparam
        t    = bindeeType fparam
        MemSummary mem ixfun = bindeeLore fparam

data ArrayDecl = ArrayDecl VName BasicType [Imp.DimSize] MemLocation

fparamSizes :: FParam -> HS.HashSet VName
fparamSizes fparam
  | Mem (Var size) <- bindeeType fparam = HS.singleton $ identName size
  | otherwise = HS.fromList $ mapMaybe name $ arrayDims $ bindeeType fparam
  where name (Var v) = Just $ identName v
        name _       = Nothing

compileInParams :: [FParam]
                -> ImpM op ([Imp.Param], [ArrayDecl], [Imp.ValueDecl])
compileInParams params = do
  (inparams, arraydecls) <- liftM partitionEithers $ mapM compileInParam params
  let findArray x = find (isArrayDecl x) arraydecls
      sizes = mconcat $ map fparamSizes params
      mkArg fparam =
        case (findArray $ bindeeName fparam, bindeeType fparam) of
          (Just (ArrayDecl _ bt shape (MemLocation mem _)), _) ->
            Just $ Imp.ArrayValue mem bt shape
          (_, Basic bt)
            | bindeeName fparam `HS.member` sizes ->
              Nothing
            | otherwise ->
              Just $ Imp.ScalarValue bt $ bindeeName fparam
          _ ->
            Nothing
      args = mapMaybe mkArg params
  return (inparams, arraydecls, args)
  where isArrayDecl x (ArrayDecl y _ _ _) = x == y

compileOutParams :: ResType
                 -> ImpM op ([Imp.ValueDecl], [Imp.Param], Destination)
compileOutParams rts = do
  ((valdecls, dests), outparams) <-
    runWriterT $ evalStateT (mapAndUnzipM mkParam rts) (HM.empty, HM.empty)
  return (valdecls, outparams, Destination dests)
  where imp = lift . lift

        mkParam (ReturnsScalar t) = do
          out <- imp $ newVName "scalar_out"
          tell [Imp.ScalarParam out t]
          return (Imp.ScalarValue t out, ScalarDestination out)
        mkParam (ReturnsArray t shape _ lore) = do
          (memout, memdest) <- case lore of
            ReturnsNewBlock x -> do
              memout <- imp $ newVName "out_mem"
              (sizeout, destmemsize) <- ensureMemSizeOut x
              tell [Imp.MemParam memout $ Imp.VarSize sizeout]
              return (memout, SetMemory memout destmemsize)
            ReturnsInBlock memout ->
              return (identName memout, CopyIntoMemory $ identName memout)
          (resultshape, destresultshape) <-
            mapAndUnzipM inspectExtDimSize $ extShapeDims shape
          return (Imp.ArrayValue memout t resultshape,
                  ArrayDestination memdest destresultshape)

        inspectExtDimSize (Ext x) = do
          (memseen,arrseen) <- get
          case HM.lookup x arrseen of
            Nothing -> do
              out <- imp $ newVName "out_arrsize"
              tell [Imp.ScalarParam out Int]
              put (memseen, HM.insert x out arrseen)
              return (Imp.VarSize out, Just out)
            Just out ->
              return (Imp.VarSize out, Nothing)
        inspectExtDimSize (Free se) = do
          se' <- imp $ subExpToDimSize se
          return (se', Nothing)

        -- | Return the name of the out-parameter for the memory size
        -- 'x', creating it if it does not already exist.
        ensureMemSizeOut x = do
          (memseen, arrseen) <- get
          case HM.lookup x memseen of
            Nothing      -> do sizeout <- imp $ newVName "out_memsize"
                               tell [Imp.ScalarParam sizeout Int]
                               put (HM.insert x sizeout memseen, arrseen)
                               return (sizeout, Just sizeout)
            Just sizeout -> return (sizeout, Nothing)

compileFunDec :: ExpCompiler op -> VNameSource -> FunDec
              -> Either String (VNameSource, (Name, Imp.Function op))
compileFunDec ec src (FunDec fname rettype params body _) = do
  ((outparams, inparams, results, args), src', body') <-
    runImpM compile ec src
  return (src',
          (fname,
           Imp.Function outparams inparams body' results args))
  where compile = do
          (inparams, arraydecls, args) <- compileInParams params
          (results, outparams, dests) <- compileOutParams rettype
          withParams inparams $
            withArrays arraydecls $
            compileExtBody dests body
          return (outparams, inparams, results, args)

compileExtBody :: Destination -> Body -> ImpM op ()
compileExtBody (Destination dest) (Body _ bnds (Result _ ses _)) =
  compileBindings bnds $ zipWithM_ compileResultSubExp dest ses

compileBody :: [VName] -> Body -> ImpM op ()
compileBody targets (Body _ bnds (Result _ ses _)) =
  compileBindings bnds $ forM_ (zip targets ses) $ \(d,se) ->
    when (subExpNotArray se) $
    compileSubExpTo d se

compileResultSubExp :: ValueDestination -> SubExp -> ImpM op ()
compileResultSubExp (ScalarDestination name) se =
  compileSubExpTo name se
compileResultSubExp (MemoryDestination mem memsizetarget) (Var v) = do
  MemEntry memsize <- lookupMemory vname
  tell $ Imp.SetMem mem $ identName v
  case memsizetarget of
    Nothing ->
      return ()
    Just memsizetarget' ->
      tell $ Imp.SetScalar memsizetarget' $ dimSizeToExp memsize
  where vname = identName v
compileResultSubExp (MemoryDestination {}) (Constant {}) =
  fail "Memory destination result subexpression cannot be a constant."
compileResultSubExp (ArrayDestination memdest shape) (Var v) = do
  arr <- lookupArray $ identName v
  let MemLocation arrmem arroffset = entryArrayLocation arr
  arrmemsize <- entryMemSize <$> lookupMemory arrmem
  case memdest of
    CopyIntoMemory mem -> when (mem /= arrmem) $ tell $
      Imp.Copy mem (Imp.Constant $ IntVal 0)
               arrmem (dimSizeToExp arroffset) $
               dimSizeToExp arrmemsize
    SetMemory mem memsize -> do
      tell $ Imp.SetMem mem arrmem
      case memsize of Nothing -> return ()
                      Just memsize' -> tell $ Imp.SetScalar memsize' $
                                       dimSizeToExp arrmemsize
  zipWithM_ maybeSetShape shape $ entryArrayShape arr
  where maybeSetShape Nothing _ =
          return ()
        maybeSetShape (Just dim) size =
          tell $ Imp.SetScalar dim $ dimSizeToExp size
compileResultSubExp (ArrayDestination {}) (Constant {}) =
  fail "Array destination result subexpression cannot be a constant."

compileBindings :: [Binding] -> ImpM op a -> ImpM op a
compileBindings []     m = m
compileBindings (b:bs) m = do
  let pat = bindingPattern b
  declaringVars (patternBindees pat) $
    compileExp (patternNames pat) (bindingExp b) $
    compileBindings bs m

compileExp :: [VName] -> Exp -> ImpM op a -> ImpM op a
compileExp targets e m = do
  ec <- asks envExpCompiler
  res <- ec targets e
  case res of
    CompileBindings bnds -> compileBindings bnds m
    CompileExp e'        -> do defCompileExp targets e'
                               m
    Done                 -> m

defCompileExp :: [VName] -> Exp -> ImpM op ()

defCompileExp targets (If cond tbranch fbranch restype _) = do
  dest <- destinationFromTargets targets $ resTypeValues restype
  tcode <- collect $ compileExtBody dest tbranch
  fcode <- collect $ compileExtBody dest fbranch
  tell $ Imp.If (compileSubExp cond) tcode fcode

defCompileExp targets (Apply fname args _ _) = do
  targets' <- sanitiseTargets targets
  tell $ Imp.Call targets' fname $
    map compileSubExp $ filter subExpNotArray $ map fst args

defCompileExp targets (PrimOp op) = defCompilePrimOp targets op

defCompileExp targets (LoopOp op) = defCompileLoopOp targets op

defCompilePrimOp :: [VName] -> PrimOp -> ImpM op ()

defCompilePrimOp [target] (SubExp se) =
  compileSubExpTo target se

defCompilePrimOp [target] (Not e _) =
  writeExp target $ Imp.UnOp Imp.Not $ compileSubExp e

defCompilePrimOp [target] (Negate e _) =
  writeExp target $ Imp.UnOp Imp.Negate $ compileSubExp e

defCompilePrimOp [target] (BinOp bop x y _ _) =
  writeExp target $ Imp.BinOp bop (compileSubExp x) (compileSubExp y)

defCompilePrimOp [_] (Assert e loc) =
  tell $ Imp.Assert (compileSubExp e) loc

defCompilePrimOp [target] (Alloc e _) =
  tell $ Imp.Allocate target $ compileSubExp e

defCompilePrimOp [target] (Index _ src idxs _)
  | length idxs == arrayRank t = do
    (srcmem, srcoffset, _) <- indexArray (identName src) $ map compileSubExp idxs
    tell $ Imp.SetScalar target $ Imp.Index srcmem srcoffset $ elemType t
    where t = identType src

defCompilePrimOp [target] (Index _ src idxs _) = do
  MemLocation destmem destoffset <- arrayLocation target
  (srcmem, srcoffset, size) <- indexArray (identName src) $ map compileSubExp idxs
  tell $ Imp.Copy
    destmem (dimSizeToExp destoffset)
    srcmem (srcoffset `impTimes` Imp.SizeOf (elemType (identType src)))
    size

defCompilePrimOp [_] (Conjoin {}) =
  return ()

defCompilePrimOp [target] (Update _ src idxs val _) = do
  srcentry <- lookupArray $ identName src
  MemLocation destmem destoffset <- arrayLocation target
  let MemLocation srcmem srcoffset = entryArrayLocation srcentry
  (_, elemoffset, size) <- indexArray (identName src) $ map compileSubExp idxs
  unless (destmem == srcmem && destoffset == srcoffset) $
    -- Not in-place, so we have to copy all of src to target...
    tell $ Imp.Copy
      destmem (dimSizeToExp destoffset)
      srcmem (dimSizeToExp srcoffset) $
      arrayByteSizeExp srcentry
  if length idxs == arrayRank srct then
    tell $ Imp.Write destmem elemoffset (elemType srct) $ compileSubExp val
    else case val of
    Constant {} -> fail "Array-value in update cannot be constant."
    Var v -> do
      valentry <- lookupArray $ identName v
      let MemLocation valmem valoffset = entryArrayLocation valentry
      tell $ Imp.Copy
        destmem (elemoffset `impTimes` Imp.SizeOf (elemType srct))
        valmem (dimSizeToExp valoffset)
        size
  where srct = identType src

defCompilePrimOp [target] (Replicate n se _) = do
  i <- newVName "i"
  (targetmem, elemoffset, rowsize) <- indexArray target [Imp.ScalarVar i]
  if basicType set then
    tell $ Imp.For i (compileSubExp n) $
    Imp.Write targetmem elemoffset (elemType set) $ compileSubExp se
    else case se of
    Constant {} ->
      fail "Array value in replicate cannot be constant."
    Var v -> do
      MemLocation vmem voffset <- arrayLocation $ identName v
      tell $ Imp.For i (compileSubExp n) $
        Imp.Copy
        targetmem (elemoffset `impTimes` Imp.SizeOf (elemType set))
        vmem (dimSizeToExp voffset)
        rowsize
  where set = subExpType se

defCompilePrimOp [target] (Iota n _) = do
  i <- newVName "i"
  (targetmem, elemoffset, _) <- indexArray target [Imp.ScalarVar i]
  tell $ Imp.For i (compileSubExp n) $
    Imp.Write targetmem elemoffset Int $ Imp.ScalarVar i

defCompilePrimOp [target] (Copy src@(Constant {}) _) =
  compileSubExpTo target src

defCompilePrimOp [target] (Copy (Var src) _)
  | basicType srct =
    compileSubExpTo target $ Var src
  | otherwise = do
    srcentry <- lookupArray $ identName src
    MemLocation destmem destoffset <- arrayLocation target
    let MemLocation srcmem srcoffset = entryArrayLocation srcentry
    tell $ Imp.Copy
      destmem (dimSizeToExp destoffset)
      srcmem (dimSizeToExp srcoffset) $
      arrayByteSizeExp srcentry
  where srct = identType src

defCompilePrimOp [target1, target2] (Split _ n (Var src) restsize _) = do
  srcentry <- lookupArray $ identName src
  let MemLocation srcmem srcoffset = entryArrayLocation srcentry
  MemLocation target1mem target1offset <- arrayLocation target1
  MemLocation target2mem target2offset <- arrayLocation target2
  let n' = compileSubExp n
      rowsize = arrayRowByteSizeExp srcentry
      restsize' = compileSubExp restsize
  tell $ Imp.Copy
    target1mem (dimSizeToExp target1offset)
    srcmem (dimSizeToExp srcoffset) $
    rowsize `impTimes` n'
  tell $ Imp.Copy
    target2mem (dimSizeToExp target2offset)
    srcmem (dimSizeToExp srcoffset `impPlus`
            (rowsize `impTimes` n')) $
    rowsize `impTimes` restsize'

defCompilePrimOp [target] (Concat _ (Var x) (Var y) _ _) = do
  xentry <- lookupArray $ identName x
  let MemLocation xmem xoffset = entryArrayLocation xentry
  yentry <- lookupArray $ identName y
  let MemLocation ymem yoffset = entryArrayLocation yentry
  MemLocation destmem destoffset <- arrayLocation target
  tell $ Imp.Copy
    destmem (dimSizeToExp destoffset)
    xmem (dimSizeToExp xoffset) $
    arrayByteSizeExp xentry
  tell $ Imp.Copy
    destmem (dimSizeToExp destoffset `impPlus`
             arrayByteSizeExp xentry)
    ymem (dimSizeToExp yoffset) $
    arrayByteSizeExp yentry

defCompilePrimOp [target] (ArrayLit es rt _) = do
  targetEntry <- lookupArray target
  let MemLocation mem offset = entryArrayLocation targetEntry
  unless (offset == Imp.ConstSize 0) $
    fail "Cannot handle offset in ArrayLit"
  forM_ (zip [0..] es) $ \(i,e) ->
    if basicType rt then
      tell $ Imp.Write mem (Imp.Constant $ IntVal i) et $ compileSubExp e
    else case e of
      Constant {} ->
        fail "defCompilePrimOp ArrayLit: Cannot have array constants."
      Var v -> do
        let bytesPerElem = foldl impTimes (Imp.SizeOf et) $
                           map dimSizeToExp $ drop 1 $
                           entryArrayShape targetEntry
        ventry <- lookupArray $ identName v
        let MemLocation vmem voffset = entryArrayLocation ventry
        tell $ Imp.Copy
          mem (dimSizeToExp offset `impPlus`
               (Imp.Constant (IntVal i) `impTimes`
                bytesPerElem))
          vmem (dimSizeToExp voffset)
          bytesPerElem
  where et = elemType rt

defCompilePrimOp [target] (Rearrange _ perm (Var src) _) = do
  is <- replicateM (length perm) (newVName "i")
  let ivars = map Imp.ScalarVar is
  (mem, offset, _) <- indexArray target $ permuteShape perm ivars
  (srcmem, srcoffset, _) <- indexArray (identName src) ivars
  let sizes = map compileSubExp $ arrayDims srct
  tell $ foldl (.) id (zipWith Imp.For is sizes) $
         Imp.Write mem offset et $
         Imp.Index srcmem srcoffset et
  where srct = identType src
        et = elemType srct

defCompilePrimOp [target] (Reshape _ _ src loc) =
  defCompilePrimOp [target] $ Copy src loc

defCompilePrimOp [target] (Rotate _ n (Var src) _) = do
  let size = compileSubExp $ arraySize 0 srct
      n'   = Imp.Constant $ IntVal n
  i <- newVName "i"
  (destmem, destoffset, rowsize) <-
    indexArray target [Imp.BinOp Mod (Imp.BinOp Plus n' $ Imp.ScalarVar i) size]
  (srcmem, srcoffset, _) <-
    indexArray (identName src) [Imp.ScalarVar i]
  tell $ Imp.For i size $
         Imp.Copy destmem (destoffset `impTimes` Imp.SizeOf (elemType srct))
         srcmem (srcoffset `impTimes` Imp.SizeOf (elemType srct))
         rowsize
  where srct = identType src

defCompilePrimOp targets@(_:_) e =
  fail $ "ImpGen.defCompilePrimOp: Incorrect number of targets\n  " ++
  intercalate ", " (map pretty targets) ++
  "\nfor expression\n  " ++ pretty e

defCompilePrimOp [] _ = return () -- No arms, no cake.

defCompileLoopOp :: [VName] -> LoopOp -> ImpM op ()

defCompileLoopOp targets loop@(DoLoop res merge i bound body _) =
  declaringVars mergepat $ do
    forM_ merge $ \(p, se) ->
      when (subExpNotArray se) $ compileSubExpTo (bindeeName p) se
    body' <- collect $ compileBody mergenames body
    tell $ Imp.For (identName i) (compileSubExp bound) body'
    let restype = expExtType $ LoopOp loop
    Destination dest <- destinationFromTargets targets restype
    zipWithM_ compileResultSubExp dest $ map Var res
    where mergepat = map fst merge
          mergenames = map bindeeName mergepat

defCompileLoopOp [_] (Map {}) = soacError

defCompileLoopOp [_] (Filter {}) = soacError

defCompileLoopOp [_] (Reduce {}) = soacError

defCompileLoopOp [_] (Scan {}) = soacError

defCompileLoopOp [_] (Redomap {}) = soacError

defCompileLoopOp [] _ = return () -- No arms, no cake.

defCompileLoopOp (_:_:_) _ = fail "ImpGen.compileLoopOp: Incorrect number of targets"

soacError :: ImpM op a
soacError = fail "SOAC encountered in code generator; should have been removed by first-order transform."

writeExp :: VName -> Imp.Exp -> ImpM op ()
writeExp target = tell . Imp.SetScalar target

insertInVtable :: VName -> VarEntry -> Env op -> Env op
insertInVtable name entry env =
  env { envVtable = HM.insert name entry $ envVtable env }

withArray :: ArrayDecl -> ImpM op a -> ImpM op a
withArray (ArrayDecl name bt shape location) m = do
  let entry = ArrayVar ArrayEntry {
          entryArrayLocation = location
        , entryArrayElemType = bt
        , entryArrayShape    = shape
        }
  local (insertInVtable name entry) m

withArrays :: [ArrayDecl] -> ImpM op a -> ImpM op a
withArrays = flip $ foldr withArray

withParams :: [Imp.Param] -> ImpM op a -> ImpM op a
withParams = flip $ foldr withParam

withParam :: Imp.Param -> ImpM op a -> ImpM op a
withParam (Imp.MemParam name memsize) =
  let entry = MemVar MemEntry {
        entryMemSize = memsize
        }
  in local $ insertInVtable name entry
withParam (Imp.ScalarParam {}) = id

declaringVars :: [PatBindee] -> ImpM op a -> ImpM op a
declaringVars = flip $ foldr declaringVar

declaringVar :: PatBindee -> ImpM op a -> ImpM op a
declaringVar bindee m =
  case bindeeType bindee of
    Basic bt -> do
      tell $ Imp.DeclareScalar name bt
      m
    Mem size -> do
      size' <- subExpToDimSize size
      tell $ Imp.DeclareMem name
      let entry = MemVar MemEntry {
            entryMemSize = size'
            }
      local (insertInVtable name entry) m
    Array bt shape _ -> do
      shape' <- mapM subExpToDimSize $ shapeDims shape
      let MemSummary mem ixfun = bindeeLore bindee
          location = MemLocation (identName mem) $ Imp.ConstSize 0
          entry = ArrayVar ArrayEntry {
              entryArrayLocation = location
            , entryArrayElemType = bt
            , entryArrayShape    = shape'
            }
      unless (IxFun.isLinear ixfun) $
        fail "Can only handle linear (simple) allocation for now."
      local (insertInVtable name entry) m
  where name = bindeeName bindee

-- | Remove the array targets.
sanitiseTargets :: [VName] -> ImpM op [VName]
sanitiseTargets = filterM $ liftM not . isArray

subExpToDimSize :: SubExp -> ImpM op Imp.DimSize
subExpToDimSize (Var v) =
  return $ Imp.VarSize $ identName v
subExpToDimSize (Constant (IntVal i) _) =
  return $ Imp.ConstSize i
subExpToDimSize (Constant {}) =
  fail "Size subexp is not a non-integer constant."

dimSizeToExp :: Imp.DimSize -> Imp.Exp
dimSizeToExp (Imp.VarSize v)   = Imp.ScalarVar v
dimSizeToExp (Imp.ConstSize x) = Imp.Constant $ IntVal x

compileSubExpTo :: VName -> SubExp -> ImpM op ()
compileSubExpTo target (Var v)
  | not (basicType $ identType v) = do
    MemLocation targetmem targetoffset <- arrayLocation target
    MemLocation srcmem srcoffset <- arrayLocation $ identName v
    if targetmem == srcmem then
       when (targetoffset /= srcoffset) $
         fail $ "Mismatching offsets when compiling subexp " ++
         pretty (identName v) ++ " (offset " ++ pretty srcoffset ++
         ") to " ++ pretty target ++ " (offset " ++ pretty targetoffset ++
         ")"
      else defCompilePrimOp [target] $ Copy (Var v) $ identSrcLoc v
compileSubExpTo target se =
  writeExp target $ compileSubExp se

compileSubExp :: SubExp -> Imp.Exp
compileSubExp (Constant v _) =
  Imp.Constant v
compileSubExp (Var v) =
  Imp.ScalarVar (identName v)

isArray :: VName -> ImpM op Bool
isArray name = do
  res <- asks $ HM.lookup name . envVtable
  case res of
    Just (ArrayVar {}) -> return True
    _                  -> return False

lookupArray :: VName -> ImpM op ArrayEntry
lookupArray name = do
  res <- asks $ HM.lookup name . envVtable
  case res of
    Just (ArrayVar entry) -> return entry
    _                    -> fail $ "Unknown array: " ++ textual name

arrayLocation :: VName -> ImpM op MemLocation
arrayLocation name = entryArrayLocation <$> lookupArray name

lookupMemory :: VName -> ImpM op MemEntry
lookupMemory name = do
  res <- asks $ HM.lookup name . envVtable
  case res of
    Just (MemVar entry) -> return entry
    _                   -> fail $ "Unknown memory block: " ++ textual name

destinationFromTargets :: [VName] -> [ExtType] -> ImpM op Destination
destinationFromTargets targets ts =
  Destination <$> mapM inspect valtargets
  where (ctxtargets,valtargets) = splitAt (length targets - length ts) targets
        isctx = (`elem` ctxtargets)
        inspect name = do
          entry <- asks $ HM.lookup name . envVtable
          case entry of
            Just (ArrayVar (ArrayEntry (MemLocation mem _) _ shape)) -> do
              let nullifyFreeDim (Imp.ConstSize _) = Nothing
                  nullifyFreeDim (Imp.VarSize v)
                    | isctx v   = Just v
                    | otherwise = Nothing
              memsize <- entryMemSize <$> lookupMemory mem
              let shape' = map nullifyFreeDim shape
                  memdest
                    | isctx mem = SetMemory mem $ nullifyFreeDim memsize
                    | otherwise = CopyIntoMemory mem
              return $ ArrayDestination memdest shape'
            Just (MemVar (MemEntry memsize))
              | Imp.VarSize memsize' <- memsize, isctx memsize' ->
                return $ MemoryDestination name $ Just memsize'
              | otherwise ->
                return $ MemoryDestination name Nothing
            Nothing ->
              return $ ScalarDestination name

indexArray :: VName -> [Imp.Exp] -> ImpM op (VName, Imp.Exp, Imp.Exp)
indexArray name indices = do
  entry <- lookupArray name
  let arrshape = map dimSizeToExp $ entryArrayShape entry
      expProduct = foldl impTimes $ Imp.Constant $ IntVal 1
      expSum = foldl impPlus $ Imp.Constant $ IntVal 0
      dimsizes = map expProduct $ drop 1 $ tails arrshape
      ixoffset = expSum $ zipWith impTimes indices dimsizes
      MemLocation arrmem arroffset = entryArrayLocation entry
  return (arrmem,
          impPlus (dimSizeToExp arroffset) ixoffset,
          expProduct $
            Imp.SizeOf (entryArrayElemType entry) :
            drop (length indices) arrshape)

impTimes :: Imp.Exp -> Imp.Exp -> Imp.Exp
impTimes (Imp.Constant (IntVal 1)) e = e
impTimes e (Imp.Constant (IntVal 1)) = e
impTimes (Imp.Constant (IntVal 0)) _ = Imp.Constant $ IntVal 0
impTimes _ (Imp.Constant (IntVal 0)) = Imp.Constant $ IntVal 0
impTimes x y                         = Imp.BinOp Times x y

impPlus :: Imp.Exp -> Imp.Exp -> Imp.Exp
impPlus (Imp.Constant (IntVal 0)) e = e
impPlus e (Imp.Constant (IntVal 0)) = e
impPlus x y                         = Imp.BinOp Plus x y

subExpNotArray :: SubExp -> Bool
subExpNotArray se = case subExpType se of
  Array {} -> False
  _        -> True

arrayByteSizeExp :: ArrayEntry -> Imp.Exp
arrayByteSizeExp entry =
  foldl impTimes (Imp.SizeOf $ entryArrayElemType entry) $
  map dimSizeToExp $ entryArrayShape entry

arrayRowByteSizeExp :: ArrayEntry -> Imp.Exp
arrayRowByteSizeExp entry =
  foldl impTimes (Imp.SizeOf $ entryArrayElemType entry) $
  drop 1 $ map dimSizeToExp $ entryArrayShape entry
