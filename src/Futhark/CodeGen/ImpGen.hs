{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
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
import Control.Monad.RWS    hiding (forM)
import Control.Monad.State  hiding (forM)
import Control.Monad.Writer hiding (forM)

import Data.Either
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Data.List
import Data.Traversable (forM)
import qualified Futhark.Analysis.AlgSimplify as AlgSimplify

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
data MemLocation = MemLocation VName [Imp.DimSize] IxFun.IxFun
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

-- | If the given value destination if a 'ScalarDestination', return
-- the variable name.  Otherwise, 'Nothing'.
fromScalarDestination :: ValueDestination -> Maybe VName
fromScalarDestination (ScalarDestination name) = Just name
fromScalarDestination _                        = Nothing

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
      MemLocation (identName mem) shape' ixfun
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
          (Just (ArrayDecl _ bt shape (MemLocation mem _ _)), _) ->
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
          (memout, memdestf) <- case lore of
            ReturnsNewBlock x -> do
              memout <- imp $ newVName "out_mem"
              (sizeout, destmemsize) <- ensureMemSizeOut x
              tell [Imp.MemParam memout $ Imp.VarSize sizeout]
              return (memout, const $ SetMemory memout destmemsize)
            ReturnsInBlock memout ixfun ->
              return (identName memout,
                      \resultshape ->
                      CopyIntoMemory $
                      MemLocation (identName memout) resultshape ixfun)
          (resultshape, destresultshape) <-
            mapAndUnzipM inspectExtDimSize $ extShapeDims shape
          let memdest = memdestf resultshape
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
    (srcmem, srcoffset) <-
      fullyIndexArray (identName src) $ map SE.subExpToScalExp idxs
    writeExp target $ index srcmem srcoffset $ elemType t
  | otherwise = return ()
  where t = identType src

defCompilePrimOp
  (Destination [ArrayDestination (CopyIntoMemory destlocation) _])
  (Replicate n se) = do
    i <- newVName "i"
    let shape' = map (elements . compileSubExp) $ n : arrayDims set
    if basicType set then do
      (targetmem, targetoffset) <-
        fullyIndexArray' destlocation [varIndex i]
      emit $ Imp.For i (compileSubExp n) $
        write targetmem targetoffset (elemType set) $ compileSubExp se
      else case se of
      Constant {} ->
        fail "Array value in replicate cannot be constant."
      Var v -> do
        targetloc <-
          indexArray destlocation [varIndex i]
        srcloc <- arrayLocation $ identName v
        let rowsize = impProduct (drop 1 shape')
                      `withElemType` elemt
        emit =<< (Imp.For i (compileSubExp n) <$>
          copyIxFun elemt targetloc srcloc rowsize)
  where set = subExpType se
        elemt = elemType set

defCompilePrimOp (Destination [_]) (Scratch {}) =
  return ()

defCompilePrimOp
  (Destination [ArrayDestination (CopyIntoMemory memlocation) _])
  (Iota n) = do
    i <- newVName "i"
    (targetmem, targetoffset) <-
      fullyIndexArray' memlocation [varIndex i]
    emit $ Imp.For i (compileSubExp n) $
      write targetmem targetoffset Int $ Imp.ScalarVar i

defCompilePrimOp (Destination [target]) (Copy src) =
  compileResultSubExp target src

defCompilePrimOp _ (Split {}) =
  return () -- Yes, really.

defCompilePrimOp
  (Destination [ArrayDestination (CopyIntoMemory (MemLocation destmem destshape destixfun)) _])
  (Concat _ x ys _) = do
    offs_glb <- newVName "tmp_offs"
    emit $ Imp.DeclareScalar offs_glb Int
    emit $ Imp.SetScalar offs_glb $ Imp.Constant $ IntVal 0
    let destloc = MemLocation destmem destshape
                  (IxFun.offset destixfun $ SE.Id $ Ident offs_glb $ Basic Int)

    forM_ (x:ys) $ \y -> do
        yentry <- lookupArray $ identName y
        let srcloc = entryArrayLocation yentry
        emit =<< copyIxFun et destloc srcloc (arrayByteSizeExp yentry)
        emit $ Imp.SetScalar offs_glb $
               Imp.BinOp Plus (Imp.ScalarVar offs_glb) $
               innerExp (arrayElemSizeExp yentry)
  where et = elemType $ identType x

defCompilePrimOp
  (Destination [ArrayDestination (CopyIntoMemory memlocation) _])
  (ArrayLit es rt) = do
    let rowshape = map (elements . compileSubExp) $ arrayDims rt
        rowsize = impProduct (drop 1 rowshape) `withElemType` et
    forM_ (zip [0..] es) $ \(i,e) ->
      if basicType rt then do
        (targetmem, targetoffset) <-
          fullyIndexArray' memlocation [constIndex i]
        emit $ write targetmem targetoffset et $ compileSubExp e
      else case e of
        Constant {} ->
          fail "defCompilePrimOp ArrayLit: Cannot have array constants."
        Var v -> do
          targetloc <-
            indexArray memlocation [SE.Val $ IntVal i]
          srcloc <-
            arrayLocation $ identName v
          emit =<< copyIxFun et targetloc srcloc rowsize
  where et = elemType rt

defCompilePrimOp _ (Rearrange {}) =
    return ()

defCompilePrimOp _ (Reshape {}) =
  return ()

defCompilePrimOp (Destination dests) (Partition _ n flags values)
  | (sizedests, arrdest) <- splitAt n dests,
    Just sizenames <- mapM fromScalarDestination sizedests,
    [ArrayDestination (CopyIntoMemory destloc) _] <- arrdest = do
  i <- newVName "i"
  let outer_dim = compileSubExp $ arraySize 0 $ identType flags
  -- We will use 'i' to index the flag array and the value array.
  -- Note that they have the same outer size ('outer_dim').
  srcloc <- arrayLocation $ identName values
  (flagmem, flagoffset) <- fullyIndexArray (identName flags) [varIndex i]

  -- First, for each of the 'n' output arrays, we compute the final
  -- size.  This is done by iterating through the flag array, but
  -- first we declare scalars to hold the size.  We do this by
  -- creating a mapping from equivalence classes to the name of the
  -- scalar holding the size.
  let sizes = HM.fromList $ zip [0..n-1] sizenames

  -- We initialise ecah size to zero.
  forM_ sizenames $ \sizename ->
    emit $ Imp.SetScalar sizename $ Imp.Constant $ IntVal 0

  -- Now iterate across the flag array, storing each element in
  -- 'eqclass', then comparing it to the known classes and increasing
  -- the appropriate size variable.
  eqclass <- newVName "eqclass"
  emit $ Imp.DeclareScalar eqclass Int
  let mkSizeLoopBody code c sizevar =
        Imp.If (Imp.BinOp Equal (Imp.ScalarVar eqclass) (Imp.Constant (IntVal c)))
        (Imp.SetScalar sizevar
         (Imp.BinOp Plus (Imp.ScalarVar sizevar) (Imp.Constant (IntVal 1))))
        code
      sizeLoopBody = HM.foldlWithKey' mkSizeLoopBody Imp.Skip sizes
  emit $ Imp.For i outer_dim $
    Imp.SetScalar eqclass (index flagmem flagoffset Int) <>
    sizeLoopBody

  -- We can now compute the starting offsets of each of the
  -- partitions, creating a map from equivalence class to its
  -- corresponding offset.
  offsets <- flip evalStateT (Imp.Constant $ IntVal 0) $ forM sizes $ \size -> do
    cur_offset <- get
    partition_offset <- lift $ newVName "partition_offset"
    lift $ emit $ Imp.DeclareScalar partition_offset Int
    lift $ emit $ Imp.SetScalar partition_offset cur_offset
    put $ Imp.BinOp Plus (Imp.ScalarVar partition_offset) (Imp.ScalarVar size)
    return partition_offset

  -- We create the memory location we use when writing a result
  -- element.  This is basically the index function of 'destloc', but
  -- with a dynamic offset, stored in 'partition_cur_offset'.
  partition_cur_offset <- newVName "partition_cur_offset"
  emit $ Imp.DeclareScalar partition_cur_offset Int

  -- Finally, we iterate through the data array and flag array in
  -- parallel, and put each element where it is supposed to go.  Note
  -- that after writing to a partition, we increase the corresponding
  -- offset.
  copy_element <- copyElem et
                  destloc [varIndex partition_cur_offset]
                  srcloc [varIndex i]
  let mkWriteLoopBody code c offsetvar =
        Imp.If (Imp.BinOp Equal (Imp.ScalarVar eqclass) (Imp.Constant (IntVal c)))
        (Imp.SetScalar partition_cur_offset
           (Imp.ScalarVar offsetvar)
         <>
         copy_element
         <>
         Imp.SetScalar offsetvar
           (Imp.BinOp Plus (Imp.ScalarVar offsetvar) (Imp.Constant (IntVal 1))))
        code
      writeLoopBody = HM.foldlWithKey' mkWriteLoopBody Imp.Skip offsets
  emit $ Imp.For i outer_dim $
    Imp.SetScalar eqclass (index flagmem flagoffset Int) <>
    writeLoopBody
  return ()
  where et = elemType $ identType values

defCompilePrimOp (Destination []) _ = return () -- No arms, no cake.

defCompilePrimOp target e =
  fail $ "ImpGen.defCompilePrimOp: Invalid target\n  " ++
  show target ++ "\nfor expression\n  " ++ pretty e

defCompileLoopOp :: Destination -> LoopOp -> ImpM op ()

defCompileLoopOp (Destination dest) (DoLoop res merge form body) =
  declaringFParams mergepat $ do
    forM_ merge $ \(p, se) ->
      when (subExpNotArray se) $
      compileScalarSubExpTo (ScalarDestination $ fparamName p) se
    body' <- collect $ compileLoopBody mergenames body
    case form of
      ForLoop i bound ->
        emit $ Imp.For (identName i) (compileSubExp bound) body'
      WhileLoop cond ->
        emit $ Imp.While (Imp.ScalarVar $ identName cond) body'
    zipWithM_ compileResultSubExp dest $ map Var res
    where mergepat = map fst merge
          mergenames = map fparamName mergepat

defCompileLoopOp _ (Map {}) = soacError

defCompileLoopOp _ (ConcatMap {}) = soacError

defCompileLoopOp _ (Scan {}) = soacError

defCompileLoopOp _ (Redomap {}) = soacError

defCompileLoopOp _ (Stream {}) = soacError

defCompileLoopOp _ (Reduce {}) = soacError

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
          location = MemLocation (identName mem) shape' ixfun
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
  let MemLocation srcmem srcshape srcixfun = entryArrayLocation arr
      arrsize = arrayByteSizeExp arr
  srcmemsize <- entryMemSize <$> lookupMemory srcmem
  case memdest of
    CopyIntoMemory (MemLocation destmem destshape destixfun)
      | destmem == srcmem && destixfun == srcixfun ->
        return ()
      | otherwise ->
          emit =<< copyIxFun et
          (MemLocation destmem destshape destixfun)
          (MemLocation srcmem srcshape srcixfun)
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

varIndex :: VName -> SE.ScalExp
varIndex name = SE.Id $ Ident name $ Basic Int

constIndex :: Int -> SE.ScalExp
constIndex = SE.Val . IntVal

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
            Just (ArrayVar (ArrayEntry (MemLocation mem _ ixfun) bt shape)) ->
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
                        | otherwise = CopyIntoMemory $ MemLocation mem shape ixfun
                  return $ ArrayDestination memdest shape'
                BindInPlace _ _ is ->
                  case patElemRequires patElem of
                    Basic _ -> do
                      (_, elemOffset) <-
                        fullyIndexArray'
                        (MemLocation mem shape ixfun)
                        (map SE.subExpToScalExp is)
                      return $ ArrayElemDestination mem bt elemOffset
                    Array _ shape' _ ->
                      let memdest = sliceArray (MemLocation mem shape ixfun) $
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

fullyIndexArray :: VName -> [ScalExp]
                -> ImpM op (VName, Count Elements)
fullyIndexArray name indices = do
  loc <- arrayLocation name
  fullyIndexArray' loc indices

fullyIndexArray' :: MemLocation -> [ScalExp]
                 -> ImpM op (VName, Count Elements)
fullyIndexArray' (MemLocation mem _ ixfun) indices =
  case scalExpToImpExp $ IxFun.index ixfun indices of
    Nothing -> fail "fullyIndexArray': Cannot turn scalexp into impexp"
    Just e -> return (mem, elements e)

indexArray :: MemLocation -> [ScalExp]
           -> ImpM op MemLocation
indexArray (MemLocation arrmem dims ixfun) indices =
  return (MemLocation arrmem (drop (length indices) dims) $
          IxFun.applyInd ixfun indices)

sliceArray :: MemLocation
           -> [SE.ScalExp]
           -> MemLocation
sliceArray (MemLocation mem shape ixfun) indices =
  MemLocation mem (drop (length indices) shape) $
  IxFun.applyInd ixfun indices

subExpNotArray :: SubExp -> Bool
subExpNotArray se = case subExpType se of
  Array {} -> False
  _        -> True

arrayByteSizeExp :: ArrayEntry -> Count Bytes
arrayByteSizeExp entry =
  arrayElemSizeExp entry
  `withElemType` entryArrayElemType entry

arrayElemSizeExp :: ArrayEntry -> Count Elements
arrayElemSizeExp entry =
  impProduct (map dimSizeToExp $ entryArrayShape entry)

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

index :: VName -> Count Elements -> BasicType -> Imp.Exp
index name (Count e) = Imp.Index name e

write :: VName -> Count Elements -> BasicType -> Imp.Exp -> Imp.Code a
write name (Count i) = Imp.Write name i

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

copyIxFun :: BasicType
          -> MemLocation
          -> MemLocation
          -> Count Bytes
          -> ImpM op (Imp.Code op)
copyIxFun bt (MemLocation destmem destshape destIxFun) (MemLocation srcmem _ srcIxFun) n
  | Just destoffset <-
      scalExpToImpExp =<<
      IxFun.linearWithOffset destIxFun,
    Just srcoffset  <-
      scalExpToImpExp =<<
      IxFun.linearWithOffset srcIxFun =
        return $ memCopy
        destmem (elements destoffset `withElemType` bt)
        srcmem (elements srcoffset `withElemType` bt)
        n
  | otherwise = do
    is <- replicateM (IxFun.rank destIxFun) (newVName "i")
    let ivars = map (SE.Id . flip Ident (Basic Int)) is
        destidx =
          simplifyScalExp $ IxFun.index destIxFun ivars
        srcidx =
          simplifyScalExp $ IxFun.index srcIxFun ivars
    return $ foldl (.) id (zipWith Imp.For is $
                                   map (innerExp . dimSizeToExp) destshape) $
      write destmem (elements $ fromJust $ scalExpToImpExp destidx) bt $
      index srcmem (elements $ fromJust $ scalExpToImpExp srcidx) bt

memCopy :: VName -> Count Bytes -> VName -> Count Bytes -> Count Bytes
        -> Imp.Code a
memCopy dest (Count destoffset) src (Count srcoffset) (Count n) =
  Imp.Copy dest destoffset src srcoffset n

copyElem :: BasicType
         -> MemLocation -> [SE.ScalExp]
         -> MemLocation -> [SE.ScalExp]
         -> ImpM op (Imp.Code op)
copyElem bt
  destlocation@(MemLocation _ destshape _) destis
  srclocation@(MemLocation _ srcshape _) srcis

  | length srcis == length srcshape, length destis == length destshape = do
  (targetmem, targetoffset) <-
    fullyIndexArray' destlocation destis
  (srcmem, srcoffset) <-
    fullyIndexArray' srclocation srcis
  return $ write targetmem targetoffset bt $ index srcmem srcoffset bt

  | otherwise = do
  destlocation' <- indexArray destlocation destis
  srclocation'  <- indexArray srclocation  srcis
  copyIxFun bt destlocation' srclocation' $
    impProduct (map dimSizeToExp $ drop (length srcis) srcshape) `withElemType` bt

scalExpToImpExp :: ScalExp -> Maybe Imp.Exp
scalExpToImpExp (SE.Val x) =
  Just $ Imp.Constant x
scalExpToImpExp (SE.Id v) =
  Just $ Imp.ScalarVar $ identName v
scalExpToImpExp (SE.SPlus e1 e2) =
  Imp.BinOp Plus <$> scalExpToImpExp e1 <*> scalExpToImpExp e2
scalExpToImpExp (SE.SMinus e1 e2) =
  Imp.BinOp Minus <$> scalExpToImpExp e1 <*> scalExpToImpExp e2
scalExpToImpExp (SE.STimes e1 e2) =
  Imp.BinOp Times <$> scalExpToImpExp e1 <*> scalExpToImpExp e2
scalExpToImpExp (SE.SDivide e1 e2) =
  Imp.BinOp Divide <$> scalExpToImpExp e1 <*> scalExpToImpExp e2
scalExpToImpExp _ =
  Nothing

simplifyScalExp :: ScalExp -> ScalExp
simplifyScalExp se = case AlgSimplify.simplify se mempty of
  Left err  -> error $ show err
  Right se' -> se'
