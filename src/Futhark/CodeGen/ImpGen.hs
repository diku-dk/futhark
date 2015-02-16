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

import Futhark.Analysis.ScalExp as SE
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
data MemLocation = MemLocation VName IxFun.IxFun
                   deriving (Show)

data ArrayEntry = ArrayEntry {
    entryArrayLocation :: MemLocation
  , entryArrayElemType :: BasicType
  , entryArrayShape :: [Imp.DimSize]
  }

data MemEntry = MemEntry {
    entryMemSize :: Imp.DimSize
  }

-- | Every non-scalar variable must be associated with an entry.
data VarEntry = ArrayVar ArrayEntry
              | MemVar MemEntry

-- | When compiling a body, this is a description of where the result
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
    shape' <- mapM subExpToDimSize $ shapeDims shape
    return $ Right $ ArrayDecl name bt shape' $
      MemLocation (identName mem) ixfun
  where name = fparamName fparam
        t    = fparamType fparam
        MemSummary mem ixfun = fparamLore fparam

data ArrayDecl = ArrayDecl VName BasicType [Imp.DimSize] MemLocation

fparamSizes :: FParam -> HS.HashSet VName
fparamSizes fparam
  | Mem (Var size) <- fparamType fparam = HS.singleton $ identName size
  | otherwise = HS.fromList $ mapMaybe name $ arrayDims $ fparamType fparam
  where name (Var v) = Just $ identName v
        name _       = Nothing

compileInParams :: [FParam]
                -> ImpM op ([Imp.Param], [ArrayDecl], [Imp.ValueDecl])
compileInParams params = do
  (inparams, arraydecls) <- liftM partitionEithers $ mapM compileInParam params
  let findArray x = find (isArrayDecl x) arraydecls
      sizes = mconcat $ map fparamSizes params
      mkArg fparam =
        case (findArray $ fparamName fparam, fparamType fparam) of
          (Just (ArrayDecl _ bt shape (MemLocation mem _)), _) ->
            Just $ Imp.ArrayValue mem bt shape
          (_, Basic bt)
            | fparamName fparam `HS.member` sizes ->
              Nothing
            | otherwise ->
              Just $ Imp.ScalarValue bt $ fparamName fparam
          _ ->
            Nothing
      args = mapMaybe mkArg params
  return (inparams, arraydecls, args)
  where isArrayDecl x (ArrayDecl y _ _ _) = x == y

compileOutParams :: RetType
                 -> ImpM op ([Imp.ValueDecl], [Imp.Param], Destination)
compileOutParams rts = do
  ((valdecls, dests), outparams) <-
    runWriterT $ evalStateT (mapAndUnzipM mkParam rts) (HM.empty, HM.empty)
  return (valdecls, outparams, Destination dests)
  where imp = lift . lift

        mkParam (ReturnsMemory _) =
          fail "Functions may not explicitly return memory blocks."
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
            ReturnsInBlock memout ixfun -> do
              unless (IxFun.isDirect ixfun) $
                fail "compileOutParams: can only handle direct index functions."
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
compileFunDec ec src (FunDec fname rettype params body) = do
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
compileExtBody (Destination dest) (Body _ bnds (Result ses)) =
  compileBindings bnds $ zipWithM_ compileResultSubExp dest ses

compileBody :: [VName] -> Body -> ImpM op ()
compileBody targets (Body _ bnds (Result ses)) =
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
      tell $ Imp.SetScalar memsizetarget' $
      innerExp $ dimSizeToExp memsize
  where vname = identName v

compileResultSubExp (MemoryDestination {}) (Constant {}) =
  fail "Memory destination result subexpression cannot be a constant."

compileResultSubExp (ArrayDestination memdest shape) (Var v) = do
  arr <- lookupArray $ identName v
  let MemLocation arrmem arrixfun = entryArrayLocation arr
  arroffset <- ixFunOffset arrixfun
  arrmemsize <- entryMemSize <$> lookupMemory arrmem
  case memdest of
    CopyIntoMemory mem -> when (mem /= arrmem) $ tell $
      copy mem (bytes $ Imp.Constant $ IntVal 0)
      arrmem (arroffset `withElemType` elemType (identType v))
      (memSizeToExp arrmemsize)
    SetMemory mem memsize -> do
      tell $ Imp.SetMem mem arrmem
      case memsize of Nothing -> return ()
                      Just memsize' -> tell $ Imp.SetScalar memsize' $
                                       innerExp $ memSizeToExp arrmemsize
  zipWithM_ maybeSetShape shape $ entryArrayShape arr
  where maybeSetShape Nothing _ =
          return ()
        maybeSetShape (Just dim) size =
          tell $ Imp.SetScalar dim $ innerExp $ dimSizeToExp size

compileResultSubExp (ArrayDestination {}) (Constant {}) =
  fail "Array destination result subexpression cannot be a constant."

compileBindings :: [Binding] -> ImpM op a -> ImpM op a
compileBindings []     m = m
compileBindings (b:bs) m = do
  let pat = bindingPattern b
  declaringVars (patternElements pat) $
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

defCompileExp targets (If cond tbranch fbranch restype) = do
  dest <- destinationFromTargets targets restype
  tcode <- collect $ compileExtBody dest tbranch
  fcode <- collect $ compileExtBody dest fbranch
  tell $ Imp.If (compileSubExp cond) tcode fcode

defCompileExp targets (Apply fname args _) = do
  targets' <- sanitiseTargets targets
  tell $ Imp.Call targets' fname $
    map compileSubExp $ filter subExpNotArray $ map fst args

defCompileExp targets (PrimOp op) = defCompilePrimOp targets op

defCompileExp targets (LoopOp op) = defCompileLoopOp targets op

defCompilePrimOp :: [VName] -> PrimOp -> ImpM op ()

defCompilePrimOp [target] (SubExp se) =
  compileSubExpTo target se

defCompilePrimOp [target] (Not e) =
  writeExp target $ Imp.UnOp Imp.Not $ compileSubExp e

defCompilePrimOp [target] (Negate e) =
  writeExp target $ Imp.UnOp Imp.Negate $ compileSubExp e

defCompilePrimOp [target] (BinOp bop x y _) =
  writeExp target $ Imp.BinOp bop (compileSubExp x) (compileSubExp y)

defCompilePrimOp [_] (Assert e loc) =
  tell $ Imp.Assert (compileSubExp e) loc

defCompilePrimOp [target] (Alloc e) =
  tell $ Imp.Allocate target $ compileSubExp e

defCompilePrimOp [target] (Index _ src idxs)
  | length idxs == arrayRank t = do
    (srcmem, srcoffset, _) <-
      indexArray (identName src) $ map (elements . compileSubExp) idxs
    tell $ Imp.SetScalar target $ index srcmem srcoffset $ elemType t
  | otherwise = return ()
  where t = identType src

defCompilePrimOp [_] (Conjoin {}) =
  return ()

defCompilePrimOp [target] (Update _ src idxs val) = do
  srcentry <- lookupArray $ identName src
  MemLocation destmem destixfun <- arrayLocation target
  destoffset <- ixFunOffset destixfun
  let MemLocation srcmem srcixfun = entryArrayLocation srcentry
  srcoffset <- ixFunOffset srcixfun
  (_, elemoffset, size) <-
    indexArray (identName src) $ map (elements . compileSubExp) idxs
  unless (destmem == srcmem && destixfun == srcixfun) $
    -- Not in-place, so we have to copy all of src to target...
    tell $ copy
      destmem (destoffset `withElemType` srcet)
      srcmem (srcoffset `withElemType` srcet) $
      arrayByteSizeExp srcentry
  if length idxs == arrayRank srct then
    tell $ write destmem elemoffset (elemType srct) $ compileSubExp val
    else case val of
    Constant {} -> fail "Array-value in update cannot be constant."
    Var v -> do
      valentry <- lookupArray $ identName v
      let MemLocation valmem valixfun = entryArrayLocation valentry
      valoffset <- ixFunOffset valixfun
      tell $ copy
        destmem (elemoffset `withElemType` srcet)
        valmem (valoffset `withElemType` srcet)
        size
  where srct = identType src
        srcet = elemType srct

defCompilePrimOp [target] (Replicate n se) = do
  i <- newVName "i"
  (targetmem, elemoffset, rowsize) <-
    indexArray target [elements $ Imp.ScalarVar i]
  if basicType set then
    tell $ Imp.For i (compileSubExp n) $
    write targetmem elemoffset (elemType set) $ compileSubExp se
    else case se of
    Constant {} ->
      fail "Array value in replicate cannot be constant."
    Var v -> do
      MemLocation vmem vixfun <- arrayLocation $ identName v
      voffset <- ixFunOffset vixfun
      tell $ Imp.For i (compileSubExp n) $
        copy
        targetmem (elemoffset `withElemType` elemType set)
        vmem (voffset `withElemType` elemType set)
        rowsize
  where set = subExpType se

defCompilePrimOp [_] (Scratch {}) =
  return ()

defCompilePrimOp [target] (Iota n) = do
  i <- newVName "i"
  (targetmem, elemoffset, _) <-
    indexArray target [elements $ Imp.ScalarVar i]
  tell $ Imp.For i (compileSubExp n) $
    write targetmem elemoffset Int $ Imp.ScalarVar i

defCompilePrimOp [target] (Copy src@(Constant {})) =
  compileSubExpTo target src

defCompilePrimOp [target] (Copy (Var src))
  | basicType srct =
    compileSubExpTo target $ Var src
  | otherwise = do
    srcentry <- lookupArray $ identName src
    MemLocation destmem destixfun <- arrayLocation target
    let et = entryArrayElemType srcentry
    destoffset <- ixFunOffset destixfun
    let MemLocation srcmem srcixfun = entryArrayLocation srcentry
    srcoffset <- ixFunOffset srcixfun
    tell $ copy
      destmem (destoffset `withElemType` et)
      srcmem (srcoffset `withElemType` et) $
      arrayByteSizeExp srcentry
  where srct = identType src

defCompilePrimOp _ (Split {}) =
  return () -- Yes, really.

defCompilePrimOp [target] (Concat _ x y _) = do
  xentry <- lookupArray $ identName x
  let MemLocation xmem xixfun = entryArrayLocation xentry
  xoffset <- ixFunOffset xixfun
  yentry <- lookupArray $ identName y
  let MemLocation ymem yixfun = entryArrayLocation yentry
  yoffset <- ixFunOffset yixfun
  MemLocation destmem destixfun <- arrayLocation target
  destoffset <- ixFunOffset destixfun
  tell $ copy
    destmem (destoffset `withElemType` et)
    xmem (xoffset `withElemType` et) $
    arrayByteSizeExp xentry
  tell $ copy
    destmem (destoffset `withElemType` et
             `plus` arrayByteSizeExp xentry)
    ymem (yoffset `withElemType` et) $
    arrayByteSizeExp yentry
  where et = elemType $ identType x

defCompilePrimOp [target] (ArrayLit es rt) = do
  targetEntry <- lookupArray target
  let MemLocation mem ixfun = entryArrayLocation targetEntry
  offset <- ixFunOffset ixfun
  unless (offset == elements (Imp.Constant (IntVal 0))) $
    fail "Cannot handle offset in ArrayLit"
  forM_ (zip [0..] es) $ \(i,e) ->
    if basicType rt then
      tell $ Imp.Write mem (Imp.Constant $ IntVal i) et $ compileSubExp e
    else case e of
      Constant {} ->
        fail "defCompilePrimOp ArrayLit: Cannot have array constants."
      Var v -> do
        let bytesPerElem =
              impProduct (map dimSizeToExp $ drop 1 $ entryArrayShape targetEntry)
              `withElemType` et
        ventry <- lookupArray $ identName v
        let MemLocation vmem vixfun = entryArrayLocation ventry
        voffset <- ixFunOffset vixfun
        tell $ copy
          mem (offset `withElemType` et
               `plus`
                (bytes (Imp.Constant (IntVal i)) `times`
                 bytesPerElem))
          vmem (voffset `withElemType` et)
          bytesPerElem
  where et = elemType rt

defCompilePrimOp [target] (Rearrange _ perm src) = do
  is <- replicateM (length perm) (newVName "i")
  let ivars = map (elements . Imp.ScalarVar) is
  (mem, offset, _) <- indexArray target $ permuteShape perm ivars
  (srcmem, srcoffset, _) <- indexArray (identName src) ivars
  let sizes = map compileSubExp $ arrayDims srct
  tell $ foldl (.) id (zipWith Imp.For is sizes) $
         write mem offset et $
         index srcmem srcoffset et
  where srct = identType src
        et = elemType srct

defCompilePrimOp [_] (Reshape {}) =
  return ()

defCompilePrimOp [target] (Rotate _ n src) = do
  let size = compileSubExp $ arraySize 0 srct
      n'   = Imp.Constant $ IntVal n
  i <- newVName "i"
  (destmem, destoffset, rowsize) <-
    indexArray target [elements $ Imp.BinOp Mod (Imp.BinOp Plus n' $ Imp.ScalarVar i) size]
  (srcmem, srcoffset, _) <-
    indexArray (identName src) [elements $ Imp.ScalarVar i]
  tell $ Imp.For i size $
         copy destmem (destoffset `withElemType` srcet)
         srcmem (srcoffset `withElemType` srcet)
         rowsize
  where srct = identType src
        srcet = elemType srct

defCompilePrimOp targets@(_:_) e =
  fail $ "ImpGen.defCompilePrimOp: Incorrect number of targets\n  " ++
  intercalate ", " (map pretty targets) ++
  "\nfor expression\n  " ++ pretty e

defCompilePrimOp [] _ = return () -- No arms, no cake.

defCompileLoopOp :: [VName] -> LoopOp -> ImpM op ()

defCompileLoopOp targets loop@(DoLoop res merge i bound body) =
  declaringFParams mergepat $ do
    forM_ merge $ \(p, se) ->
      when (subExpNotArray se) $ compileSubExpTo (fparamName p) se
    body' <- collect $ compileBody mergenames body
    tell $ Imp.For (identName i) (compileSubExp bound) body'
    let restype = expExtType $ LoopOp loop
    Destination dest <- destinationFromTargets targets restype
    zipWithM_ compileResultSubExp dest $ map Var res
    where mergepat = map fst merge
          mergenames = map fparamName mergepat

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

declaringVars :: [PatElem] -> ImpM op a -> ImpM op a
declaringVars = flip $ foldr declaringVar

declaringFParams :: [FParam] -> ImpM op a -> ImpM op a
declaringFParams = flip $ foldr $ declaringVar . toPatElem
  where toPatElem fparam = BindVar (fparamIdent fparam) (fparamLore fparam)

declaringVar :: PatElem -> ImpM op a -> ImpM op a
declaringVar patElem m =
  case patElemType patElem of
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
      let MemSummary mem ixfun = patElemLore patElem
          location = MemLocation (identName mem) ixfun
          entry = ArrayVar ArrayEntry {
              entryArrayLocation = location
            , entryArrayElemType = bt
            , entryArrayShape    = shape'
            }
      local (insertInVtable name entry) m
  where name = patElemName patElem

-- | Remove the array targets.
sanitiseTargets :: [VName] -> ImpM op [VName]
sanitiseTargets = filterM $ liftM not . isArray

subExpToDimSize :: SubExp -> ImpM op Imp.DimSize
subExpToDimSize (Var v) =
  return $ Imp.VarSize $ identName v
subExpToDimSize (Constant (IntVal i)) =
  return $ Imp.ConstSize i
subExpToDimSize (Constant {}) =
  fail "Size subexp is not a non-integer constant."

dimSizeToExp :: Imp.DimSize -> Count Elements
dimSizeToExp = elements . sizeToExp

memSizeToExp :: Imp.MemSize -> Count Bytes
memSizeToExp = bytes . sizeToExp

sizeToExp :: Imp.Size -> Imp.Exp
sizeToExp (Imp.VarSize v)   = Imp.ScalarVar v
sizeToExp (Imp.ConstSize x) = Imp.Constant $ IntVal x

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
      else defCompilePrimOp [target] $ Copy $ Var v
compileSubExpTo target se =
  writeExp target $ compileSubExp se

compileSubExp :: SubExp -> Imp.Exp
compileSubExp (Constant v) =
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
            Just (ArrayVar (ArrayEntry (MemLocation mem ixfun) _ shape)) -> do
              let nullifyFreeDim (Imp.ConstSize _) = Nothing
                  nullifyFreeDim (Imp.VarSize v)
                    | isctx v   = Just v
                    | otherwise = Nothing
              unless (IxFun.isDirect ixfun) $
                fail "destinationFromTargets: can only handle direct index functions."
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

indexArray :: VName -> [Count Elements]
           -> ImpM op (VName, Count Elements, Count Bytes)
indexArray name indices = do
  entry <- lookupArray name
  let arrshape = map dimSizeToExp $ entryArrayShape entry
      expSum = foldl plus $ elements $ Imp.Constant $ IntVal 0
      dimsizes = map impProduct $ drop 1 $ tails arrshape
      ixoffset = expSum $ zipWith times indices dimsizes
      MemLocation arrmem ixfun = entryArrayLocation entry
  arroffset <- ixFunOffset ixfun
  return (arrmem,
          arroffset `plus` ixoffset,
          impProduct (drop (length indices) arrshape)
          `withElemType`
          entryArrayElemType entry)

subExpNotArray :: SubExp -> Bool
subExpNotArray se = case subExpType se of
  Array {} -> False
  _        -> True

arrayByteSizeExp :: ArrayEntry -> Count Bytes
arrayByteSizeExp entry =
  impProduct (map dimSizeToExp $ entryArrayShape entry)
  `withElemType` entryArrayElemType entry

ixFunOffset :: Monad m => IxFun.IxFun -> m (Count Elements)
ixFunOffset ixfun =
  case IxFun.linearWithOffset ixfun of
    Just offset
      | Just offset' <- scalExpToImpExp offset ->
        return $ elements offset'
      | otherwise ->
        fail $ "Cannot turn " ++ pretty offset ++ " into an Imp.Exp."
    Nothing -> fail $ "Index function " ++ pretty ixfun ++
               " is not linear with an offset."

  where scalExpToImpExp (SE.Val x) =
          Just $ Imp.Constant x
        scalExpToImpExp (SE.Id v) =
          Just $ Imp.ScalarVar $ identName v
        scalExpToImpExp (SE.SPlus e1 e2) =
          Imp.BinOp Plus <$> scalExpToImpExp e1 <*> scalExpToImpExp e2
        scalExpToImpExp (SE.STimes e1 e2) =
          Imp.BinOp Times <$> scalExpToImpExp e1 <*> scalExpToImpExp e2
        scalExpToImpExp _ =
          Nothing

-- A wrapper around 'Imp.Exp' that maintains a unit as a phantom type.
newtype Count u = Count { innerExp :: Imp.Exp }
                deriving (Eq, Show)

data Elements
data Bytes

elements :: Imp.Exp -> Count Elements
elements = Count

bytes :: Imp.Exp -> Count Bytes
bytes = Count

withElemType :: Count Elements -> BasicType -> Count Bytes
withElemType (Count e) t = bytes $ e `times'` Imp.SizeOf t

copy :: VName -> Count Bytes -> VName -> Count Bytes -> Count Bytes
     -> Imp.Code a
copy dest (Count destoffset) src (Count srcoffset) (Count n) =
  Imp.Copy dest destoffset src srcoffset n

index :: VName -> Count Elements -> BasicType -> Imp.Exp
index name (Count e) = Imp.Index name e

write :: VName -> Count Elements -> BasicType -> Imp.Exp -> Imp.Code a
write name (Count i) = Imp.Write name i

plus :: Count u -> Count u -> Count u
plus (Count x) (Count y) = Count $ x `plus'` y

plus' :: Imp.Exp -> Imp.Exp -> Imp.Exp
plus' (Imp.Constant (IntVal 0)) e = e
plus' e (Imp.Constant (IntVal 0)) = e
plus' x y                         = Imp.BinOp Plus x y

times :: Count u -> Count u -> Count u
times (Count x) (Count y) = Count $ x `times'` y

times' :: Imp.Exp -> Imp.Exp -> Imp.Exp
times' (Imp.Constant (IntVal 1)) e = e
times' e (Imp.Constant (IntVal 1)) = e
times' (Imp.Constant (IntVal 0)) _ = Imp.Constant $ IntVal 0
times' _ (Imp.Constant (IntVal 0)) = Imp.Constant $ IntVal 0
times' x y                         = Imp.BinOp Times x y

impProduct :: [Count u] -> Count u
impProduct = foldl times $ Count $ Imp.Constant $ IntVal 1
