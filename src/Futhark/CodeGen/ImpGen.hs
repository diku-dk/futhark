{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, LambdaCase #-}
module Futhark.CodeGen.ImpGen
  ( -- * Entry Points
    compileProg
  , compileProgSimply

    -- * Pluggable Compiler
  , ExpCompiler
  , ExpCompilerResult (..)
  , Destination (..)
  , ValueDestination (..)
  , ArrayMemoryDestination (..)
  , MemLocation (..)
  , MemEntry (..)
  , ScalarEntry (..)

    -- * Monadic Compiler Interface
  , ImpM
  , Env (envVtable)
  , emit
  , collect
  , VarEntry (..)

    -- * Lookups
  , lookupArray
  , arrayLocation
  , lookupMemory

    -- * Building Blocks
  , compileSubExp
  , compileResultSubExp
  , subExpToDimSize
  , declaringLParams
  , declaringVarEntry
  , compileBindings
  , writeExp
  , indexArray
  , fullyIndexArray
  , fullyIndexArray'
  , varIndex

    -- * Typed enumerations
  , Count (..)
  , Bytes
  , Elements
  , elements
  , bytes
  , index
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

import Prelude

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
      entryMemSize  :: Imp.DimSize
    , entryMemSpace :: Imp.Space
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
                      | ArrayElemDestination VName BasicType Imp.Space (Count Bytes)
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

instance HasTypeEnv (ImpM op) where
  askTypeEnv = HM.map entryType <$> asks envVtable
    where entryType (MemVar memEntry) =
            Mem $ dimSizeToSubExp $ entryMemSize memEntry
          entryType (ArrayVar arrayEntry) =
            Array
            (entryArrayElemType arrayEntry)
            (Shape $ map dimSizeToSubExp $ entryArrayShape arrayEntry)
            Nonunique -- Arbitrary
          entryType (ScalarVar scalarEntry) =
            Basic $ entryScalarType scalarEntry

          dimSizeToSubExp (Imp.ConstSize n) =
            Constant $ IntVal n
          dimSizeToSubExp (Imp.VarSize v) =
            Var v

runImpM :: ImpM op a -> ExpCompiler op -> VNameSource -> Either String (a, VNameSource, Imp.Code op)
runImpM (ImpM m) = runRWST m . newEnv

-- | Execute a code generation action, returning the code that was
-- emitted.
collect :: ImpM op () -> ImpM op (Imp.Code op)
collect m = pass $ do
  ((), code) <- listen m
  return (code, const mempty)

-- | Emit some generated imperative code.
emit :: Imp.Code op -> ImpM op ()
emit = tell

compileProg :: ExpCompiler op -> Prog -> Either String (Imp.Program op)
compileProg ec prog =
  Imp.Program <$> snd <$> mapAccumLM (compileFunDec ec) src (progFunctions prog)
  where src = newNameSourceForProg prog

-- | 'compileProg' with an 'ExpCompiler' that always returns 'CompileExp'.
compileProgSimply :: Prog -> Either String (Imp.Program ())
compileProgSimply = compileProg $ const $ return . CompileExp

compileInParam :: FParam -> ImpM op (Either Imp.Param ArrayDecl)
compileInParam fparam = case t of
  Basic bt -> return $ Left $ Imp.ScalarParam name bt
  Mem size -> Left <$> (Imp.MemParam name <$> subExpToDimSize size <*> pure Nothing)
  Array bt shape _ -> do
    shape' <- mapM subExpToDimSize $ shapeDims shape
    return $ Right $ ArrayDecl name bt shape' $
      MemLocation mem shape' ixfun
  where name = paramName fparam
        t    = paramType fparam
        MemSummary mem ixfun = paramLore fparam

data ArrayDecl = ArrayDecl VName BasicType [Imp.DimSize] MemLocation

fparamSizes :: FParam -> HS.HashSet VName
fparamSizes fparam
  | Mem (Var size) <- paramType fparam = HS.singleton size
  | otherwise = HS.fromList $ mapMaybe name $ arrayDims $ paramType fparam
  where name (Var v) = Just v
        name _       = Nothing

compileInParams :: [FParam]
                -> ImpM op ([Imp.Param], [ArrayDecl], [Imp.ValueDecl])
compileInParams params = do
  (inparams, arraydecls) <- liftM partitionEithers $ mapM compileInParam params
  let findArray x = find (isArrayDecl x) arraydecls
      sizes = mconcat $ map fparamSizes params
      mkArg fparam =
        case (findArray $ paramName fparam, paramType fparam) of
          (Just (ArrayDecl _ bt shape (MemLocation mem _ _)), _) ->
            Just $ Imp.ArrayValue mem bt shape
          (_, Basic bt)
            | paramName fparam `HS.member` sizes ->
              Nothing
            | otherwise ->
              Just $ Imp.ScalarValue bt $ paramName fparam
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
              tell [Imp.MemParam memout (Imp.VarSize sizeout) Nothing]
              return (memout, const $ SetMemory memout destmemsize)
            ReturnsInBlock memout ixfun ->
              return (memout,
                      \resultshape ->
                      CopyIntoMemory $
                      MemLocation memout resultshape ixfun)
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
compileExtBody (Destination dest) (Body _ bnds ses) =
  compileBindings bnds $ zipWithM_ compileResultSubExp dest ses

compileLoopBody :: [VName] -> Body -> ImpM op (Imp.Code op)
compileLoopBody targets (Body _ bnds ses) =
  collect $ compileBindings bnds $ forM_ (zip targets ses) $ \(d,se) ->
    subExpType se >>= \case
      Basic _  -> compileScalarSubExpTo (ScalarDestination d) se
      Mem _    -> compileResultSubExp (MemoryDestination d Nothing) se
      Array {} -> return ()

compileBindings :: [Binding] -> ImpM op a -> ImpM op a
compileBindings []     m = m
compileBindings (Let pat _ e:bs) m =
  declaringVars (patternElements pat) $ do
    dest <- destinationFromPattern pat
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
  emit =<<
    (Imp.Call targets fname <$>
     map compileSubExp <$>
     filterM subExpNotArray (map fst args))

defCompileExp targets (PrimOp op) = defCompilePrimOp targets op

defCompileExp targets (LoopOp op) = defCompileLoopOp targets op

defCompileExp _ (SegOp op) =
  fail $ "ImpGen called on Segmented Operator, this is not supported. " ++
         pretty (SegOp op)

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

defCompilePrimOp (Destination [target]) (Index _ src idxs) = do
  t <- lookupType src
  when (length idxs == arrayRank t ) $ do
    (srcmem, space, srcoffset) <-
      fullyIndexArray src $ map (`SE.subExpToScalExp` Int) idxs
    writeExp target $ index srcmem srcoffset (elemType t) space

defCompilePrimOp
  (Destination [ArrayDestination (CopyIntoMemory destlocation) _])
  (Replicate n se) = do
    set <- subExpType se
    let elemt = elemType set
    i <- newVName "i"
    declaringLoopVar i $ do
      let shape' = map (elements . compileSubExp) $ n : arrayDims set
      if basicType set then do
        (targetmem, space, targetoffset) <-
          fullyIndexArray' destlocation [varIndex i] $ elemType set
        emit $ Imp.For i (compileSubExp n) $
          write targetmem targetoffset (elemType set) space $ compileSubExp se
        else case se of
        Constant {} ->
          fail "Array value in replicate cannot be constant."
        Var v -> do
          targetloc <-
            indexArray destlocation [varIndex i]
          srcloc <- arrayLocation v
          let rowsize = impProduct (drop 1 shape')
                        `withElemType` elemt
          emit =<< (Imp.For i (compileSubExp n) <$>
            copyIxFun elemt targetloc srcloc rowsize)

defCompilePrimOp (Destination [_]) (Scratch {}) =
  return ()

defCompilePrimOp
  (Destination [ArrayDestination (CopyIntoMemory memlocation) _])
  (Iota n) = do
    i <- newVName "i"
    declaringLoopVar i $ do
      (targetmem, space, targetoffset) <-
        fullyIndexArray' memlocation [varIndex i] Int
      emit $ Imp.For i (compileSubExp n) $
        write targetmem targetoffset Int space $ Imp.ScalarVar i

defCompilePrimOp (Destination [target]) (Copy src) =
  compileResultSubExp target $ Var src

defCompilePrimOp _ (Split {}) =
  return () -- Yes, really.

defCompilePrimOp
  (Destination [ArrayDestination (CopyIntoMemory (MemLocation destmem destshape destixfun)) _])
  (Concat _ x ys _) = do
    et <- elemType <$> lookupType x
    offs_glb <- newVName "tmp_offs"
    emit $ Imp.DeclareScalar offs_glb Int
    emit $ Imp.SetScalar offs_glb $ Imp.Constant $ IntVal 0
    let destloc = MemLocation destmem destshape
                  (IxFun.offsetIndex destixfun $ SE.Id offs_glb Int)

    forM_ (x:ys) $ \y -> do
        yentry <- lookupArray y
        let srcloc = entryArrayLocation yentry
            rows = case entryArrayShape yentry of
                    []  -> error $ "defCompilePrimOp Concat: empty array shape for " ++ pretty y
                    r:_ -> innerExp $ dimSizeToExp r
        emit =<< copyIxFun et destloc srcloc (arrayByteSizeExp yentry)
        emit $ Imp.SetScalar offs_glb $
               Imp.BinOp Plus (Imp.ScalarVar offs_glb) rows

defCompilePrimOp
  (Destination [ArrayDestination (CopyIntoMemory memlocation) _])
  (ArrayLit es rt) = do
    let rowshape = map (elements . compileSubExp) $ arrayDims rt
        rowsize = impProduct (drop 1 rowshape) `withElemType` et
    forM_ (zip [0..] es) $ \(i,e) ->
      if basicType rt then do
        (targetmem, space, targetoffset) <-
          fullyIndexArray' memlocation [constIndex i] $ elemType rt
        emit $ write targetmem targetoffset et space $ compileSubExp e
      else case e of
        Constant {} ->
          fail "defCompilePrimOp ArrayLit: Cannot have array constants."
        Var v -> do
          targetloc <- indexArray memlocation [SE.Val $ IntVal i]
          srcloc <- arrayLocation v
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
  declaringLoopVar i $ do
    et <- elemType <$> lookupType values
    outer_dim <- compileSubExp <$> arraySize 0 <$> lookupType flags
    -- We will use 'i' to index the flag array and the value array.
    -- Note that they have the same outer size ('outer_dim').
    srcloc <- arrayLocation values
    (flagmem, space, flagoffset) <- fullyIndexArray flags [varIndex i]

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
      Imp.SetScalar eqclass (index flagmem flagoffset Int space) <>
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
      Imp.SetScalar eqclass (index flagmem flagoffset Int space) <>
      writeLoopBody
    return ()

defCompilePrimOp (Destination []) _ = return () -- No arms, no cake.

defCompilePrimOp target e =
  fail $ "ImpGen.defCompilePrimOp: Invalid target\n  " ++
  show target ++ "\nfor expression\n  " ++ pretty e

defCompileLoopOp :: Destination -> LoopOp -> ImpM op ()

defCompileLoopOp (Destination dest) (DoLoop res merge form body) =
  declaringFParams mergepat $ do
    forM_ merge $ \(p, se) -> do
      na <- subExpNotArray se
      when na $
        compileScalarSubExpTo (ScalarDestination $ paramName p) se
    let (bindForm, emitForm) =
          case form of
            ForLoop i bound ->
              (declaringLoopVar i,
               emit . Imp.For i (compileSubExp bound))
            WhileLoop cond ->
              (id,
               emit . Imp.While (Imp.ScalarVar cond))

    bindForm $ do
      body' <- compileLoopBody mergenames body
      emitForm body'
    zipWithM_ compileResultSubExp dest $ map Var res
    where mergepat = map fst merge
          mergenames = map paramName mergepat

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
writeExp (ArrayElemDestination destmem bt space elemoffset) e =
  emit $ write destmem elemoffset bt space e
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
withParam (Imp.MemParam name memsize space) =
  let entry = MemVar MemEntry {
          entryMemSize = memsize
        , entryMemSpace = space
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
  where toPatElem fparam = PatElem (paramIdent fparam) BindVar (paramLore fparam)

declaringLParams :: [LParam] -> ImpM op a -> ImpM op a
declaringLParams = flip $ foldr $ declaringVar . toPatElem
  where toPatElem fparam = PatElem (paramIdent fparam) BindVar (paramLore fparam)

declaringVarEntry :: VName -> VarEntry -> ImpM op a -> ImpM op a
declaringVarEntry name entry m = do
  case entry of
    MemVar entry' ->
      emit $ Imp.DeclareMem name $ entryMemSpace entry'
    ScalarVar entry' ->
      emit $ Imp.DeclareScalar name $ entryScalarType entry'
    ArrayVar _ ->
      return ()
  local (insertInVtable name entry) m

declaringVar :: PatElem -> ImpM op a -> ImpM op a
declaringVar patElem m =
  case patElemType patElem of
    Basic bt -> do
      let entry = ScalarVar ScalarEntry { entryScalarType    = bt
                                        }
      declaringVarEntry name entry m
    Mem size -> do
      size' <- subExpToDimSize size
      let entry = MemVar MemEntry {
              entryMemSize = size'
            , entryMemSpace = Nothing
            }
      declaringVarEntry name entry m
    Array bt shape _ -> do
      shape' <- mapM subExpToDimSize $ shapeDims shape
      let MemSummary mem ixfun = patElemLore patElem
          location = MemLocation mem shape' ixfun
          entry = ArrayVar ArrayEntry {
              entryArrayLocation = location
            , entryArrayElemType = bt
            , entryArrayShape    = shape'
            }
      declaringVarEntry name entry m
  where name = patElemName patElem

declaringBasicVar :: VName -> BasicType -> ImpM op a -> ImpM op a
declaringBasicVar name bt =
  local (insertInVtable name $ ScalarVar $ ScalarEntry bt)

declaringLoopVars :: [VName] -> ImpM op a -> ImpM op a
declaringLoopVars = flip $ foldr declaringLoopVar

declaringLoopVar :: VName -> ImpM op a -> ImpM op a
declaringLoopVar name =
  declaringBasicVar name Int

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
  return $ Imp.VarSize v
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

compileResultSubExp (ArrayElemDestination destmem bt space elemoffset) se =
  emit $ write destmem elemoffset bt space $ compileSubExp se

compileResultSubExp (MemoryDestination mem memsizetarget) (Var v) = do
  MemEntry memsize _ <- lookupMemory v
  emit $ Imp.SetMem mem v
  case memsizetarget of
    Nothing ->
      return ()
    Just memsizetarget' ->
      emit $ Imp.SetScalar memsizetarget' $
      innerExp $ dimSizeToExp memsize

compileResultSubExp (MemoryDestination {}) (Constant {}) =
  fail "Memory destination result subexpression cannot be a constant."

compileResultSubExp (ArrayDestination memdest shape) (Var v) = do
  et <- elemType <$> lookupType v
  arr <- lookupArray v
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
  where maybeSetShape Nothing _ =
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
  Imp.ScalarVar v

varIndex :: VName -> SE.ScalExp
varIndex name = SE.Id name Int

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

destinationFromPattern :: Pattern -> ImpM op Destination
destinationFromPattern (Pattern ctxElems valElems) =
  Destination <$> mapM inspect valElems
  where ctxNames = map patElemName ctxElems
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
                      (_, space, elemOffset) <-
                        fullyIndexArray'
                        (MemLocation mem shape ixfun)
                        (map (`SE.subExpToScalExp` Int) is)
                        bt
                      return $ ArrayElemDestination mem bt space elemOffset
                    Array _ shape' _ ->
                      let memdest = sliceArray (MemLocation mem shape ixfun) $
                                    map (`SE.subExpToScalExp` Int) is
                      in return $
                         ArrayDestination (CopyIntoMemory memdest) $
                         replicate (shapeRank shape') Nothing
                    Mem {} ->
                      fail "destinationFromPattern: cannot do an in-place bind of a memory block."

            Just (MemVar (MemEntry memsize _))
              | Imp.VarSize memsize' <- memsize, isctx memsize' ->
                return $ MemoryDestination name $ Just memsize'
              | otherwise ->
                return $ MemoryDestination name Nothing

            Just (ScalarVar (ScalarEntry _)) ->
              return $ ScalarDestination name

            Nothing ->
              fail $ "destinationFromPattern: unknown target " ++ pretty name

fullyIndexArray :: VName -> [ScalExp]
                -> ImpM op (VName, Imp.Space, Count Bytes)
fullyIndexArray name indices = do
  arr <- lookupArray name
  fullyIndexArray' (entryArrayLocation arr) indices $ entryArrayElemType arr

fullyIndexArray' :: MemLocation -> [ScalExp] -> BasicType
                 -> ImpM op (VName, Imp.Space, Count Bytes)
fullyIndexArray' (MemLocation mem _ ixfun) indices bt = do
  space <- entryMemSpace <$> lookupMemory mem
  case scalExpToImpExp $ IxFun.index ixfun indices $ basicScalarSize bt of
    Nothing -> fail "fullyIndexArray': Cannot turn scalexp into impexp"
    Just e -> return (mem, space, bytes e)

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

subExpNotArray :: SubExp -> ImpM op Bool
subExpNotArray se = subExpType se >>= \case
  Array {} -> return False
  _        -> return True

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

index :: VName -> Count Bytes -> BasicType -> Imp.Space -> Imp.Exp
index name (Count e) = Imp.Index name e

write :: VName -> Count Bytes -> BasicType -> Imp.Space -> Imp.Exp -> Imp.Code a
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
      IxFun.linearWithOffset destIxFun bt_size ,
    Just srcoffset  <-
      scalExpToImpExp =<<
      IxFun.linearWithOffset srcIxFun bt_size =
        return $ memCopy
        destmem (bytes destoffset)
        srcmem (bytes srcoffset)
        n
  | otherwise = do
    is <- replicateM (IxFun.rank destIxFun) (newVName "i")
    declaringLoopVars is $ do
      let ivars = map varIndex is
      destidx <- simplifyScalExp $ IxFun.index destIxFun ivars bt_size
      srcidx <- simplifyScalExp $ IxFun.index srcIxFun ivars bt_size
      srcspace <- entryMemSpace <$> lookupMemory srcmem
      destspace <- entryMemSpace <$> lookupMemory destmem
      return $ foldl (.) id (zipWith Imp.For is $
                                     map (innerExp . dimSizeToExp) destshape) $
        write destmem (bytes $ fromJust $ scalExpToImpExp destidx) bt destspace $
        index srcmem (bytes $ fromJust $ scalExpToImpExp srcidx) bt srcspace
  where bt_size = basicScalarSize bt

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
  (targetmem, destspace, targetoffset) <-
    fullyIndexArray' destlocation destis bt
  (srcmem, srcspace, srcoffset) <-
    fullyIndexArray' srclocation srcis bt
  return $ write targetmem targetoffset bt destspace $ index srcmem srcoffset bt srcspace

  | otherwise = do
  destlocation' <- indexArray destlocation destis
  srclocation'  <- indexArray srclocation  srcis
  copyIxFun bt destlocation' srclocation' $
    impProduct (map dimSizeToExp $ drop (length srcis) srcshape) `withElemType` bt

scalExpToImpExp :: ScalExp -> Maybe Imp.Exp
scalExpToImpExp (SE.Val x) =
  Just $ Imp.Constant x
scalExpToImpExp (SE.Id v _) =
  Just $ Imp.ScalarVar v
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

simplifyScalExp :: Monad m => ScalExp -> m ScalExp
simplifyScalExp se =
  case AlgSimplify.simplify se mempty of
    Left err  -> fail $ show err
    Right se' -> return se'

basicScalarSize :: BasicType -> ScalExp
basicScalarSize = SE.Val . IntVal . basicSize
