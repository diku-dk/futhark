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
import Futhark.Util
import qualified Text.PrettyPrint.Mainland as PP

-- | A substitute expression compiler, tried before the main
-- expression compilation function.
type ExpCompiler op = Destination -> Exp -> ImpM op (ExpCompilerResult op)

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
  , entryArrayShape    :: [Imp.DimSize]
  }

data MemEntry = MemEntry {
    entryMemSize :: Imp.DimSize
  }

data ScalarEntry = ScalarEntry {
    entryScalarType    :: BasicType
  }

-- | Every non-scalar variable must be associated with an entry.
data VarEntry = ArrayVar ArrayEntry
              | ScalarVar ScalarEntry
              | MemVar MemEntry

-- | When compiling a body, this is a description of where the result
-- should end up.
newtype Destination = Destination [ValueDestination]
                    deriving (Show)

data ValueDestination = ScalarDestination VName
                      | ArrayElemDestination VName BasicType (Count Elements)
                      | MemoryDestination VName (Maybe VName)
                      | ArrayDestination ArrayMemoryDestination [Maybe VName]
                      deriving (Show)

data ArrayMemoryDestination = SetMemory VName (Maybe VName)
                            | CopyIntoMemory MemLocation
                            deriving (Show)

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

emit :: Imp.Code op -> ImpM op ()
emit = tell

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
            ReturnsInBlock memout ixfun ->
              return (identName memout,
                      CopyIntoMemory $ MemLocation (identName memout) ixfun)
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

compileLoopBody :: [VName] -> Body -> ImpM op ()
compileLoopBody targets (Body _ bnds (Result ses)) =
  compileBindings bnds $ forM_ (zip targets ses) $ \(d,se) ->
  case subExpType se of
    Basic _  -> compileScalarSubExpTo (ScalarDestination d) se
    Mem _    -> compileResultSubExp (MemoryDestination d Nothing) se
    Array {} -> return ()

compileBindings :: [Binding] -> ImpM op a -> ImpM op a
compileBindings []     m = m
compileBindings (Let pat _ e:bs) m =
  declaringVars (patternElements pat) $ do
    dest <- destinationFromPattern pat (expExtType e)
    compileExp dest e $ compileBindings bs m

compileExp :: Destination -> Exp -> ImpM op a -> ImpM op a
compileExp targets e m = do
  ec <- asks envExpCompiler
  res <- ec targets e
  case res of
    CompileBindings bnds -> compileBindings bnds m
    CompileExp e'        -> do defCompileExp targets e'
                               m
    Done                 -> m

defCompileExp :: Destination -> Exp -> ImpM op ()

defCompileExp dest (If cond tbranch fbranch _) = do
  tcode <- collect $ compileExtBody dest tbranch
  fcode <- collect $ compileExtBody dest fbranch
  emit $ Imp.If (compileSubExp cond) tcode fcode

defCompileExp dest (Apply fname args _) = do
  targets <- funcallTargets dest
  emit $ Imp.Call targets fname $
    map compileSubExp $ filter subExpNotArray $ map fst args

defCompileExp targets (PrimOp op) = defCompilePrimOp targets op

defCompileExp targets (LoopOp op) = defCompileLoopOp targets op

defCompilePrimOp :: Destination -> PrimOp -> ImpM op ()

defCompilePrimOp (Destination [target]) (SubExp se) =
  compileResultSubExp target se

defCompilePrimOp (Destination [target]) (Not e) =
  writeExp target $ Imp.UnOp Imp.Not $ compileSubExp e

defCompilePrimOp (Destination [target]) (Negate e) =
  writeExp target $ Imp.UnOp Imp.Negate $ compileSubExp e

defCompilePrimOp (Destination [target]) (BinOp bop x y _) =
  writeExp target $ Imp.BinOp bop (compileSubExp x) (compileSubExp y)

defCompilePrimOp (Destination [_]) (Assert e loc) =
  emit $ Imp.Assert (compileSubExp e) loc

defCompilePrimOp (Destination [MemoryDestination mem size]) (Alloc e) = do
  emit $ Imp.Allocate mem e'
  case size of Just size' -> emit $ Imp.SetScalar size' e'
               Nothing    -> return ()
  where e' = compileSubExp e

defCompilePrimOp (Destination [target]) (Index _ src idxs)
  | length idxs == arrayRank t = do
    (srcmem, srcoffset, _) <-
      indexArray (identName src) $ map (elements . compileSubExp) idxs
    writeExp target $ index srcmem srcoffset $ elemType t
  | otherwise = return ()
  where t = identType src

defCompilePrimOp (Destination [_]) (Conjoin {}) =
  return ()

defCompilePrimOp
  (Destination [ArrayDestination (CopyIntoMemory memlocation) _])
  (Replicate n se) = do
    i <- newVName "i"
    let shape' = map (elements . compileSubExp) $ n : arrayDims set
    (targetmem, elemoffset, rowsize) <-
      indexArray' memlocation shape' elemt [elements $ Imp.ScalarVar i]
    if basicType set then
      emit $ Imp.For i (compileSubExp n) $
      write targetmem elemoffset (elemType set) $ compileSubExp se
      else case se of
      Constant {} ->
        fail "Array value in replicate cannot be constant."
      Var v -> do
        MemLocation vmem vixfun <- arrayLocation $ identName v
        voffset <- ixFunOffset vixfun
        emit $ Imp.For i (compileSubExp n) $
          copy
          targetmem (elemoffset `withElemType` elemType set)
          vmem (voffset `withElemType` elemType set)
          rowsize
  where set = subExpType se
        elemt = elemType set

defCompilePrimOp (Destination [_]) (Scratch {}) =
  return ()

defCompilePrimOp
  (Destination [ArrayDestination (CopyIntoMemory memlocation) _])
  (Iota n) = do
    i <- newVName "i"
    let shape = [elements $ compileSubExp n]
    (targetmem, elemoffset, _) <-
      indexArray' memlocation shape Int [elements $ Imp.ScalarVar i]
    emit $ Imp.For i (compileSubExp n) $
      write targetmem elemoffset Int $ Imp.ScalarVar i

defCompilePrimOp (Destination [target]) (Copy src) =
  compileResultSubExp target src

defCompilePrimOp _ (Split {}) =
  return () -- Yes, really.

defCompilePrimOp
  (Destination [ArrayDestination (CopyIntoMemory (MemLocation destmem destixfun)) _])
  (Concat _ x ys _) = do
    destoffset <- ixFunOffset destixfun
    offs_glb <- newVName "tmp_offs"
    emit $ Imp.DeclareScalar offs_glb Int
    emit $ Imp.SetScalar offs_glb $ innerExp (destoffset `withElemType` et)

    forM_ (x:ys) $ \y -> do
        yentry <- lookupArray $ identName y
        let MemLocation ymem yixfun = entryArrayLocation yentry
        yoffset <- ixFunOffset yixfun
        emit $ copy
                destmem (bytes (Imp.ScalarVar offs_glb))
                ymem (yoffset `withElemType` et)
                (arrayByteSizeExp yentry)
        emit $ Imp.SetScalar offs_glb $
               Imp.BinOp Plus (Imp.ScalarVar offs_glb) $
               innerExp (arrayByteSizeExp yentry)
  where et = elemType $ identType x

defCompilePrimOp
  (Destination [ArrayDestination (CopyIntoMemory memlocation) _])
  (ArrayLit es rt) = do
    let dims = Constant (IntVal 0) : arrayDims rt
        shape = map (elements . compileSubExp) dims
    forM_ (zip [0..] es) $ \(i,e) -> do
      (targetmem, elemoffset, _) <-
        indexArray' memlocation shape et [elements $ Imp.Constant $ IntVal i]
      if basicType rt then
        emit $ write targetmem elemoffset et $ compileSubExp e
      else case e of
        Constant {} ->
          fail "defCompilePrimOp ArrayLit: Cannot have array constants."
        Var v -> do
          (srcmem, srcoffset, srcsize) <- indexArray (identName v) []
          emit $ copy
            targetmem (elemoffset `withElemType` et)
            srcmem (srcoffset `withElemType` et)
            srcsize
  where et = elemType rt

defCompilePrimOp _ (Rearrange {}) =
    return ()

{-
  is <- replicateM (length perm) (newVName "i")
  let ivars = map (elements . Imp.ScalarVar) is
      newshape = permuteShape perm $ arrayDims srct
  (mem, offset, _) <-
    indexArray' memlocation
    (map (elements . compileSubExp) newshape) et $
    permuteShape perm ivars
  (srcmem, srcoffset, _) <- indexArray (identName src) ivars
  let sizes = map compileSubExp $ arrayDims srct
  emit $ foldl (.) id (zipWith Imp.For is sizes) $
         write mem offset et $
         index srcmem srcoffset et
  where srct = identType src
        et = elemType srct
-}

defCompilePrimOp _ (Reshape {}) =
  return ()

defCompilePrimOp
  (Destination [ArrayDestination (CopyIntoMemory memlocation) _])
  (Rotate _ n src) = do
    let size = compileSubExp $ arraySize 0 srct
        n'   = Imp.Constant $ IntVal n
        shape = map (elements . compileSubExp) $ arrayDims srct
    i <- newVName "i"
    (destmem, destoffset, rowsize) <-
      indexArray' memlocation shape srcet
      [elements $ Imp.BinOp Mod (Imp.BinOp Plus n' $ Imp.ScalarVar i) size]
    (srcmem, srcoffset, _) <-
      indexArray (identName src) [elements $ Imp.ScalarVar i]
    emit $ Imp.For i size $
           copy destmem (destoffset `withElemType` srcet)
           srcmem (srcoffset `withElemType` srcet)
           rowsize
  where srct = identType src
        srcet = elemType srct

defCompilePrimOp (Destination []) _ = return () -- No arms, no cake.

defCompilePrimOp target e =
  fail $ "ImpGen.defCompilePrimOp: Invalid target\n  " ++
  show target ++ "\nfor expression\n  " ++ pretty e

defCompileLoopOp :: Destination -> LoopOp -> ImpM op ()

defCompileLoopOp (Destination dest) (DoLoop res merge i bound body) =
  declaringFParams mergepat $ do
    forM_ merge $ \(p, se) ->
      when (subExpNotArray se) $
      compileScalarSubExpTo (ScalarDestination $ fparamName p) se
    body' <- collect $ compileLoopBody mergenames body
    emit $ Imp.For (identName i) (compileSubExp bound) body'
    zipWithM_ compileResultSubExp dest $ map Var res
    where mergepat = map fst merge
          mergenames = map fparamName mergepat

defCompileLoopOp _ (Map {}) = soacError

defCompileLoopOp _ (ConcatMap {}) = soacError

defCompileLoopOp _ (Filter {}) = soacError

defCompileLoopOp _ (Reduce {}) = soacError

defCompileLoopOp _ (Scan {}) = soacError

defCompileLoopOp _ (Redomap {}) = soacError

soacError :: ImpM op a
soacError = fail "SOAC encountered in code generator; should have been removed by first-order transform."

writeExp :: ValueDestination -> Imp.Exp -> ImpM op ()
writeExp (ScalarDestination target) e =
  emit $ Imp.SetScalar target e
writeExp (ArrayElemDestination destmem bt elemoffset) e =
  emit $ write destmem elemoffset bt e
writeExp target e =
  fail $ "Cannot write " ++ pretty e ++ " to " ++ show target

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
withParam (Imp.ScalarParam name bt) =
  let entry = ScalarVar ScalarEntry { entryScalarType = bt
                                    }
  in local $ insertInVtable name entry

declaringVars :: [PatElem] -> ImpM op a -> ImpM op a
declaringVars = flip $ foldr declaringVar

declaringFParams :: [FParam] -> ImpM op a -> ImpM op a
declaringFParams = flip $ foldr $ declaringVar . toPatElem
  where toPatElem fparam = PatElem (fparamIdent fparam) BindVar (fparamLore fparam)

declaringVar :: PatElem -> ImpM op a -> ImpM op a
declaringVar patElem m =
  case patElemType patElem of
    Basic bt -> do
      emit $ Imp.DeclareScalar name bt
      let entry = ScalarVar ScalarEntry { entryScalarType    = bt
                                        }
      local (insertInVtable name entry) m
    Mem size -> do
      size' <- subExpToDimSize size
      emit $ Imp.DeclareMem name
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
funcallTargets :: Destination -> ImpM op [VName]
funcallTargets (Destination dests) =
  liftM concat $ mapM funcallTarget dests
  where funcallTarget (ScalarDestination name) =
          return [name]
        funcallTarget (ArrayElemDestination {}) =
          fail "Cannot put scalar function return in-place yet." -- FIXME
        funcallTarget (ArrayDestination (CopyIntoMemory _) shape) =
          return $ catMaybes shape
        funcallTarget (ArrayDestination (SetMemory mem memsize) shape) =
          return $ maybeToList memsize ++ [mem] ++ catMaybes shape
        funcallTarget (MemoryDestination name size) =
          return $ maybeToList size ++ [name]

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

compileResultSubExp :: ValueDestination -> SubExp -> ImpM op ()

compileResultSubExp (ScalarDestination name) se =
  compileScalarSubExpTo (ScalarDestination name) se

compileResultSubExp (ArrayElemDestination destmem bt elemoffset) se =
  emit $ write destmem elemoffset bt $ compileSubExp se

compileResultSubExp (MemoryDestination mem memsizetarget) (Var v) = do
  MemEntry memsize <- lookupMemory vname
  emit $ Imp.SetMem mem $ identName v
  case memsizetarget of
    Nothing ->
      return ()
    Just memsizetarget' ->
      emit $ Imp.SetScalar memsizetarget' $
      innerExp $ dimSizeToExp memsize
  where vname = identName v

compileResultSubExp (MemoryDestination {}) (Constant {}) =
  fail "Memory destination result subexpression cannot be a constant."

compileResultSubExp (ArrayDestination memdest shape) (Var v) = do
  arr <- lookupArray $ identName v
  let MemLocation srcmem srcixfun = entryArrayLocation arr
      arrsize = arrayByteSizeExp arr
  srcmemsize <- entryMemSize <$> lookupMemory srcmem
  case memdest of
    CopyIntoMemory (MemLocation destmem destixfun)
      | destmem == srcmem && destixfun == srcixfun ->
        return ()
      | otherwise ->
          copyIxFun et
            (MemLocation destmem destixfun)
            (MemLocation srcmem srcixfun)
            arrsize
    SetMemory mem memsize -> do
      emit $ Imp.SetMem mem srcmem
      case memsize of Nothing -> return ()
                      Just memsize' -> emit $ Imp.SetScalar memsize' $
                                       innerExp $ memSizeToExp srcmemsize
  zipWithM_ maybeSetShape shape $ entryArrayShape arr
  where et = elemType $ identType v
        maybeSetShape Nothing _ =
          return ()
        maybeSetShape (Just dim) size =
          emit $ Imp.SetScalar dim $ innerExp $ dimSizeToExp size

compileResultSubExp (ArrayDestination {}) (Constant {}) =
  fail "Array destination result subexpression cannot be a constant."

compileScalarSubExpTo :: ValueDestination -> SubExp -> ImpM op ()

compileScalarSubExpTo target se =
  writeExp target $ compileSubExp se

compileSubExp :: SubExp -> Imp.Exp
compileSubExp (Constant v) =
  Imp.Constant v
compileSubExp (Var v) =
  Imp.ScalarVar (identName v)

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

destinationFromPattern :: Pattern -> [ExtType] -> ImpM op Destination
destinationFromPattern (Pattern patElems) ts =
  Destination <$> mapM inspect valElems
  where (ctxElems, valElems) = splitAt (length patElems - length ts) patElems
        ctxNames = map patElemName ctxElems
        isctx = (`elem` ctxNames)
        inspect patElem = do
          let name = patElemName patElem
          entry <- asks $ HM.lookup name . envVtable
          case entry of
            Just (ArrayVar (ArrayEntry (MemLocation mem ixfun) bt shape)) -> do
              case patElemBindage patElem of
                BindVar -> do
                  let nullifyFreeDim (Imp.ConstSize _) = Nothing
                      nullifyFreeDim (Imp.VarSize v)
                        | isctx v   = Just v
                        | otherwise = Nothing
                  memsize <- entryMemSize <$> lookupMemory mem
                  let shape' = map nullifyFreeDim shape
                      memdest
                        | isctx mem = SetMemory mem $ nullifyFreeDim memsize
                        | otherwise = CopyIntoMemory $ MemLocation mem ixfun
                  return $ ArrayDestination memdest shape'
                BindInPlace _ _ is -> do
                  case patElemRequires patElem of
                    Basic _ -> do
                      (_, elemOffset, _) <-
                        indexArray'
                        (MemLocation mem ixfun)
                        (map dimSizeToExp shape)
                        bt $
                        map (elements . compileSubExp) is
                      return $ ArrayElemDestination mem bt elemOffset
                    Array _ shape' _ ->
                      let memdest = sliceArray (MemLocation mem ixfun) $
                                    map SE.subExpToScalExp is
                      in return $
                         ArrayDestination (CopyIntoMemory memdest) $
                         replicate (shapeRank shape') Nothing
                    Mem {} ->
                      fail "destinationFromPattern: cannot do an in-place bind of a memory block."

            Just (MemVar (MemEntry memsize))
              | Imp.VarSize memsize' <- memsize, isctx memsize' ->
                return $ MemoryDestination name $ Just memsize'
              | otherwise ->
                return $ MemoryDestination name Nothing

            Just (ScalarVar (ScalarEntry _)) ->
              return $ ScalarDestination name

            Nothing ->
              fail $ "destinationFromPattern: unknown target " ++ pretty name

indexArray :: VName -> [Count Elements]
           -> ImpM op (VName, Count Elements, Count Bytes)
indexArray name indices = do
  entry <- lookupArray name
  let MemLocation mem ixfun = entryArrayLocation entry
  indexArray'
    (MemLocation mem ixfun)
    (map dimSizeToExp $ entryArrayShape entry)
    (entryArrayElemType entry)
    indices

indexArray' :: MemLocation -> [Count Elements] -> BasicType -> [Count Elements]
            -> ImpM op (VName, Count Elements, Count Bytes)
indexArray' (MemLocation arrmem ixfun) arrshape elemtype indices = do
  arroffset <- ixFunOffset ixfun
  let expSum = foldl plus $ elements $ Imp.Constant $ IntVal 0
      dimsizes = map impProduct $ drop 1 $ tails arrshape
      ixoffset = expSum $ zipWith times indices dimsizes
  return (arrmem,
          arroffset `plus` ixoffset,
          impProduct (drop (length indices) arrshape)
          `withElemType`
          elemtype)

sliceArray :: MemLocation
           -> [SE.ScalExp]
           -> MemLocation
sliceArray (MemLocation mem ixfun) indices =
  MemLocation mem $ IxFun.applyInd ixfun indices

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

-- A wrapper around 'Imp.Exp' that maintains a unit as a phantom type.
newtype Count u = Count { innerExp :: Imp.Exp }
                deriving (Eq, Show)

instance PP.Pretty (Count u) where
  ppr = PP.ppr . innerExp

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

-- More complicated read/write operations that use index functions.

copyIxFun :: BasicType -> MemLocation -> MemLocation -> Count Bytes
          -> ImpM op ()
copyIxFun bt (MemLocation destmem destIxFun) (MemLocation srcmem srcIxFun) n
  | Just destoffset <- scalExpToImpExp =<< IxFun.linearWithOffset destIxFun,
    Just srcoffset  <- scalExpToImpExp =<< IxFun.linearWithOffset srcIxFun =
      emit $ memCopy
      destmem (elements destoffset `withElemType` bt)
      srcmem (elements srcoffset `withElemType` bt)
      n
  | otherwise = do
    is <- replicateM (IxFun.rank destIxFun) (newVName "i")
    let ivars = map (SE.Id . flip Ident (Basic Int)) is
        oldshape = map compileSubExp $ IxFun.shape srcIxFun
        destidx = IxFun.index destIxFun ivars
        srcidx = IxFun.index srcIxFun ivars
    emit $ foldl (.) id (zipWith Imp.For is oldshape) $
      write destmem (elements $ fromJust $ scalExpToImpExp destidx) bt $
      index srcmem (elements $ fromJust $ scalExpToImpExp srcidx) bt


memCopy :: VName -> Count Bytes -> VName -> Count Bytes -> Count Bytes
        -> Imp.Code a
memCopy dest (Count destoffset) src (Count srcoffset) (Count n) =
  Imp.Copy dest destoffset src srcoffset n

scalExpToImpExp :: ScalExp -> Maybe Imp.Exp
scalExpToImpExp (SE.Val x) =
  Just $ Imp.Constant x
scalExpToImpExp (SE.Id v) =
  Just $ Imp.ScalarVar $ identName v
scalExpToImpExp (SE.SPlus e1 e2) =
  Imp.BinOp Plus <$> scalExpToImpExp e1 <*> scalExpToImpExp e2
scalExpToImpExp (SE.STimes e1 e2) =
  Imp.BinOp Times <$> scalExpToImpExp e1 <*> scalExpToImpExp e2
scalExpToImpExp _ =
  Nothing
