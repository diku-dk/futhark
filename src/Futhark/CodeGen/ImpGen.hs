{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, LambdaCase, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.CodeGen.ImpGen
  ( -- * Entry Points
    compileProg

    -- * Pluggable Compiler
  , OpCompiler
  , ExpCompiler
  , ExpCompilerResult (..)
  , CopyCompiler
  , BodyCompiler
  , Operations (..)
  , defaultOperations
  , Destination (..)
  , ValueDestination (..)
  , ArrayMemoryDestination (..)
  , MemLocation (..)
  , MemEntry (..)
  , ScalarEntry (..)

    -- * Monadic Compiler Interface
  , ImpM
  , Env (envVtable, envDefaultSpace)
  , subImpM
  , subImpM_
  , emit
  , collect
  , comment
  , VarEntry (..)
  , ArrayEntry (..)

    -- * Lookups
  , lookupArray
  , arrayLocation
  , lookupMemory

    -- * Building Blocks
  , compileSubExp
  , compileSubExpOfType
  , compileSubExpTo
  , compilePrimExp
  , compileAlloc
  , subExpToDimSize
  , declaringLParams
  , declaringFParams
  , declaringVarEntry
  , declaringScope
  , declaringScopes
  , withParams
  , declaringPrimVar
  , declaringPrimVars
  , withPrimVar
  , everythingVolatile
  , compileBody
  , defCompileBody
  , compileStms
  , compileExp
  , sliceArray
  , offsetArray
  , strideArray
  , fullyIndexArray
  , fullyIndexArray'
  , varIndex
  , Imp.dimSizeToExp
  , dimSizeToSubExp
  , dimSizeToPrimExp
  , destinationFromParam
  , destinationFromParams
  , destinationFromPattern
  , funcallTargets
  , copy
  , copyDWIM
  , copyDWIMDest
  , copyElementWise
  )
  where

import Control.Applicative
import Control.Monad.RWS    hiding (mapM, forM)
import Control.Monad.State  hiding (mapM, forM)
import Control.Monad.Writer hiding (mapM, forM)
import Control.Monad.Except hiding (mapM, forM)
import Data.Either
import Data.Traversable
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe
import Data.List

import Prelude hiding (div, quot, mod, rem, mapM)

import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.CodeGen.ImpCode
  (Count (..),
   Bytes, Elements,
   bytes, withElemType)
import Futhark.Representation.ExplicitMemory
import Futhark.Representation.SOACS (SOACS)
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Construct (fullSliceNum)
import Futhark.MonadFreshNames
import Futhark.Util

-- | How to compile an 'Op'.
type OpCompiler lore op = Destination -> Op lore -> ImpM lore op ()

-- | How to compile a 'Body'.
type BodyCompiler lore op = Destination -> Body lore -> ImpM lore op ()

-- | A substitute expression compiler, tried before the main
-- expression compilation function.
type ExpCompiler lore op = Destination -> Exp lore -> ImpM lore op (ExpCompilerResult lore op)

-- | The result of the substitute expression compiler.
data ExpCompilerResult lore op =
      CompileStms [Stm lore]
    -- ^ New bindings.  Note that the bound expressions will
    -- themselves be compiled using the expression compiler.
    | CompileExp (Exp lore)
    -- ^ A new expression (or possibly the same as the input) - this
    -- will not be passed back to the expression compiler, but instead
    -- processed with the default action.
    | Done
    -- ^ Some code was added via the monadic interface.

type CopyCompiler lore op = PrimType
                           -> MemLocation
                           -> MemLocation
                           -> Count Elements -- ^ Number of row elements of the source.
                           -> ImpM lore op ()

data Operations lore op = Operations { opsExpCompiler :: ExpCompiler lore op
                                     , opsOpCompiler :: OpCompiler lore op
                                     , opsBodyCompiler :: BodyCompiler lore op
                                     , opsCopyCompiler :: CopyCompiler lore op
                                     }

-- | An operations set for which the expression compiler always
-- returns 'CompileExp'.
defaultOperations :: ExplicitMemorish lore => OpCompiler lore op -> Operations lore op
defaultOperations opc = Operations { opsExpCompiler = const $ return . CompileExp
                                   , opsOpCompiler = opc
                                   , opsBodyCompiler = defCompileBody
                                   , opsCopyCompiler = defaultCopy
                                   }

-- | When an array is declared, this is where it is stored.
data MemLocation = MemLocation { memLocationName :: VName
                               , memLocationShape :: [Imp.DimSize]
                               , memLocationIxFun :: IxFun
                               }
                   deriving (Eq, Show)

data ArrayEntry = ArrayEntry {
    entryArrayLocation :: MemLocation
  , entryArrayElemType :: PrimType
  }

entryArrayShape :: ArrayEntry -> [Imp.DimSize]
entryArrayShape = memLocationShape . entryArrayLocation

data MemEntry = MemEntry {
      entryMemSize  :: Imp.MemSize
    , entryMemSpace :: Imp.Space
  }

data ScalarEntry = ScalarEntry {
    entryScalarType    :: PrimType
  }

-- | Every non-scalar variable must be associated with an entry.
data VarEntry lore = ArrayVar (Maybe (Exp lore)) ArrayEntry
                   | ScalarVar (Maybe (Exp lore)) ScalarEntry
                   | MemVar (Maybe (Exp lore)) MemEntry

-- | When compiling a body, this is a description of where the result
-- should end up.
newtype Destination = Destination { valueDestinations :: [ValueDestination] }
                    deriving (Show)

instance Monoid Destination where
  Destination ds1 `mappend` Destination ds2 = Destination $ ds1 <> ds2
  mempty = Destination mempty

data ValueDestination = ScalarDestination VName
                      | ArrayElemDestination VName PrimType Imp.Space (Count Bytes)
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

data Env lore op = Env {
    envVtable :: HM.HashMap VName (VarEntry lore)
  , envExpCompiler :: ExpCompiler lore op
  , envBodyCompiler :: BodyCompiler lore op
  , envOpCompiler :: OpCompiler lore op
  , envCopyCompiler :: CopyCompiler lore op
  , envDefaultSpace :: Imp.Space
  , envVolatility :: Imp.Volatility
  }

newEnv :: Operations lore op -> Imp.Space -> Env lore op
newEnv ops ds = Env { envVtable = HM.empty
                    , envExpCompiler = opsExpCompiler ops
                    , envBodyCompiler = opsBodyCompiler ops
                    , envOpCompiler = opsOpCompiler ops
                    , envCopyCompiler = opsCopyCompiler ops
                    , envDefaultSpace = ds
                    , envVolatility = Imp.Nonvolatile
                    }

newtype ImpM lore op a = ImpM (RWST (Env lore op) (Imp.Code op) VNameSource (Either String) a)
  deriving (Functor, Applicative, Monad,
            MonadState VNameSource,
            MonadReader (Env lore op),
            MonadWriter (Imp.Code op),
            MonadError String)

instance MonadFreshNames (ImpM lore op) where
  getNameSource = get
  putNameSource = put

instance HasScope SOACS (ImpM lore op) where
  askScope = HM.map (LetInfo . entryType) <$> asks envVtable
    where entryType (MemVar _ memEntry) =
            Mem (dimSizeToSubExp $ entryMemSize memEntry) (entryMemSpace memEntry)
          entryType (ArrayVar _ arrayEntry) =
            Array
            (entryArrayElemType arrayEntry)
            (Shape $ map dimSizeToSubExp $ entryArrayShape arrayEntry)
            NoUniqueness
          entryType (ScalarVar _ scalarEntry) =
            Prim $ entryScalarType scalarEntry

runImpM :: ImpM lore op a
        -> Operations lore op -> Imp.Space -> VNameSource
        -> Either String (a, VNameSource, Imp.Code op)
runImpM (ImpM m) comp = runRWST m . newEnv comp

subImpM_ :: Operations lore' op' -> ImpM lore' op' a
         -> ImpM lore op (Imp.Code op')
subImpM_ ops m = snd <$> subImpM ops m

subImpM :: Operations lore' op' -> ImpM lore' op' a
        -> ImpM lore op (a, Imp.Code op')
subImpM ops (ImpM m) = do
  env <- ask
  src <- getNameSource
  case runRWST m env { envExpCompiler = opsExpCompiler ops
                     , envBodyCompiler = opsBodyCompiler ops
                     , envCopyCompiler = opsCopyCompiler ops
                     , envOpCompiler = opsOpCompiler ops
                     , envVtable = HM.map scrubExps $ envVtable env
                     }
       src of
    Left err -> throwError err
    Right (x, src', code) -> do
      putNameSource src'
      return (x, code)
  where scrubExps (ArrayVar _ entry) = ArrayVar Nothing entry
        scrubExps (MemVar _ entry) = MemVar Nothing entry
        scrubExps (ScalarVar _ entry) = ScalarVar Nothing entry

-- | Execute a code generation action, returning the code that was
-- emitted.
collect :: ImpM lore op () -> ImpM lore op (Imp.Code op)
collect m = pass $ do
  ((), code) <- listen m
  return (code, const mempty)

-- | Execute a code generation action, wrapping the generated code
-- within a 'Imp.Comment' with the given description.
comment :: String -> ImpM lore op () -> ImpM lore op ()
comment desc m = do code <- collect m
                    emit $ Imp.Comment desc code

-- | Emit some generated imperative code.
emit :: Imp.Code op -> ImpM lore op ()
emit = tell

compileProg :: (ExplicitMemorish lore, MonadFreshNames m) =>
               Operations lore op -> Imp.Space
            -> Prog lore -> m (Either String (Imp.Functions op))
compileProg ops ds prog =
  modifyNameSource $ \src ->
  case mapAccumLM (compileFunDef ops ds) src (progFunctions prog) of
    Left err -> (Left err, src)
    Right (src', funs) -> (Right $ Imp.Functions funs, src')

compileInParam :: ExplicitMemorish lore =>
                  FParam lore -> ImpM lore op (Either Imp.Param ArrayDecl)
compileInParam fparam = case paramAttr fparam of
  Scalar bt ->
    return $ Left $ Imp.ScalarParam name bt
  MemMem size space ->
    Left <$> (Imp.MemParam name <$> subExpToDimSize size <*> pure space)
  ArrayMem bt shape _ mem ixfun -> do
    shape' <- mapM subExpToDimSize $ shapeDims shape
    return $ Right $ ArrayDecl name bt $
      MemLocation mem shape' ixfun
  where name = paramName fparam

data ArrayDecl = ArrayDecl VName PrimType MemLocation

fparamSizes :: Typed attr => Param attr -> HS.HashSet VName
fparamSizes fparam
  | Mem (Var size) _ <- paramType fparam = HS.singleton size
  | otherwise = HS.fromList $ subExpVars $ arrayDims $ paramType fparam

compileInParams :: ExplicitMemorish lore =>
                   [FParam lore]
                -> ImpM lore op ([Imp.Param], [ArrayDecl], [Imp.ValueDecl])
compileInParams params = do
  (inparams, arraydecls) <- partitionEithers <$> mapM compileInParam params
  let findArray x = find (isArrayDecl x) arraydecls
      sizes = mconcat $ map fparamSizes params
      mkArg fparam =
        case (findArray $ paramName fparam, paramType fparam) of
          (Just (ArrayDecl _ bt (MemLocation mem shape _)), _) ->
            Just $ Imp.ArrayValue mem bt shape
          (_, Prim bt)
            | paramName fparam `HS.member` sizes ->
              Nothing
            | otherwise ->
              Just $ Imp.ScalarValue bt $ paramName fparam
          _ ->
            Nothing
      args = mapMaybe mkArg params
  return (inparams, arraydecls, args)
  where isArrayDecl x (ArrayDecl y _ _) = x == y

compileOutParams :: ExplicitMemorish lore =>
                    RetType lore
                 -> ImpM lore op ([Imp.ValueDecl], [Imp.Param], Destination)
compileOutParams rts = do
  ((valdecls, dests), outparams) <-
    runWriterT $ evalStateT (mapAndUnzipM mkParam rts) (HM.empty, HM.empty)
  return (valdecls, outparams, Destination dests)
  where imp = lift . lift

        mkParam ReturnsMemory{} =
          throwError "Functions may not explicitly return memory blocks."
        mkParam (ReturnsScalar t) = do
          out <- imp $ newVName "scalar_out"
          tell [Imp.ScalarParam out t]
          return (Imp.ScalarValue t out, ScalarDestination out)
        mkParam (ReturnsArray t shape _ lore) = do
          space <- asks envDefaultSpace
          (memout, memdestf) <- case lore of
            ReturnsNewBlock x _ -> do
              memout <- imp $ newVName "out_mem"
              (sizeout, destmemsize) <- ensureMemSizeOut x
              tell [Imp.MemParam memout (Imp.VarSize sizeout) space]
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
              tell [Imp.ScalarParam out int32]
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
                               tell [Imp.ScalarParam sizeout int32]
                               put (HM.insert x sizeout memseen, arrseen)
                               return (sizeout, Just sizeout)
            Just sizeout -> return (sizeout, Nothing)

compileFunDef :: ExplicitMemorish lore =>
                 Operations lore op -> Imp.Space
              -> VNameSource
              -> FunDef lore
              -> Either String (VNameSource, (Name, Imp.Function op))
compileFunDef ops ds src (FunDef entry fname rettype params body) = do
  ((outparams, inparams, results, args), src', body') <-
    runImpM compile ops ds src
  return (src',
          (fname,
           Imp.Function entry outparams inparams body' results args))
  where compile = do
          (inparams, arraydecls, args) <- compileInParams params
          (results, outparams, dests) <- compileOutParams rettype
          withParams inparams $
            withArrays arraydecls $
            compileBody dests body
          return (outparams, inparams, results, args)

compileBody :: ExplicitMemorish lore => Destination -> Body lore -> ImpM lore op ()
compileBody dest body = do
  cb <- asks envBodyCompiler
  cb dest body

defCompileBody :: ExplicitMemorish lore => Destination -> Body lore -> ImpM lore op ()
defCompileBody (Destination dest) (Body _ bnds ses) =
  compileStms bnds $ zipWithM_ compileSubExpTo dest ses

compileLoopBody :: ExplicitMemorish lore =>
                   [VName] -> Body lore -> ImpM lore op (Imp.Code op)
compileLoopBody mergenames (Body _ bnds ses) = do
  -- We cannot write the results to the merge parameters immediately,
  -- as some of the results may actually *be* merge parameters, and
  -- would thus be clobbered.  Therefore, we first copy to new
  -- variables mirroring the merge parameters, and then copy this
  -- buffer to the merge parameters.  This is efficient, because the
  -- operations are all scalar operations.
  tmpnames <- mapM (newVName . (++"_tmp") . baseString) mergenames
  collect $ compileStms bnds $ do
    copy_to_merge_params <- forM (zip3 mergenames tmpnames ses) $ \(d,tmp,se) ->
      subExpType se >>= \case
        Prim bt  -> do
          se' <- compileSubExp se
          emit $ Imp.DeclareScalar tmp bt
          emit $ Imp.SetScalar tmp se'
          return $ emit $ Imp.SetScalar d $ Imp.var tmp bt
        Mem _ space | Var v <- se -> do
          emit $ Imp.DeclareMem tmp space
          emit $ Imp.SetMem tmp v space
          return $ emit $ Imp.SetMem d tmp space
        _ -> return $ return ()
    sequence_ copy_to_merge_params

compileStms :: ExplicitMemorish lore => [Stm lore] -> ImpM lore op a -> ImpM lore op a
compileStms []     m = m
compileStms (Let pat _ e:bs) m =
  declaringVars (Just e) (patternElements pat) $ do
    dest <- destinationFromPattern pat
    compileExp dest e $ compileStms bs m

compileExp :: ExplicitMemorish lore =>
              Destination -> Exp lore -> ImpM lore op a -> ImpM lore op a
compileExp targets e m = do
  ec <- asks envExpCompiler
  res <- ec targets e
  case res of
    CompileStms bnds -> compileStms bnds m
    CompileExp e'        -> do defCompileExp targets e'
                               m
    Done                 -> m

defCompileExp :: ExplicitMemorish lore => Destination -> Exp lore -> ImpM lore op ()

defCompileExp dest (If cond tbranch fbranch _) = do
  cond' <- compileSubExp cond
  tcode <- collect $ compileBody dest tbranch
  fcode <- collect $ compileBody dest fbranch
  emit $ Imp.If cond' tcode fcode

defCompileExp dest (Apply fname args _) = do
  targets <- funcallTargets dest
  args' <- catMaybes <$> mapM compileArg args
  emit $ Imp.Call targets fname args'
  where compileArg (se, _) = do
          t <- subExpType se
          case (se, t) of
            (_, Prim pt)    -> return $ Just $ Imp.ExpArg $ compileSubExpOfType pt se
            (Var v, Mem{}) -> return $ Just $ Imp.MemArg v
            _              -> return Nothing

defCompileExp targets (BasicOp op) = defCompileBasicOp targets op

defCompileExp (Destination dest) (DoLoop ctx val form body) =
  declaringFParams mergepat $ do
    forM_ merge $ \(p, se) -> do
      na <- subExpNotArray se
      when na $
        copyDWIM (paramName p) [] se []
    (bindForm, emitForm) <-
      case form of
        ForLoop i it bound -> do
          bound' <- compileSubExp bound
          return (declaringLoopVar i it, emit . Imp.For i it bound')
        WhileLoop cond ->
          return (id, emit . Imp.While (Imp.var cond Bool))

    bindForm $ do
      body' <- compileLoopBody mergenames body
      emitForm body'
    zipWithM_ compileSubExpTo dest $ map (Var . paramName . fst) val
    where merge = ctx ++ val
          mergepat = map fst merge
          mergenames = map paramName mergepat

defCompileExp dest (Op op) = do
  opc <- asks envOpCompiler
  opc dest op

defCompileBasicOp :: Destination -> BasicOp lore -> ImpM lore op ()

defCompileBasicOp (Destination [target]) (SubExp se) =
  compileSubExpTo target se

defCompileBasicOp (Destination [target]) (UnOp op e) = do
  e' <- compileSubExp e
  writeExp target $ Imp.UnOpExp op e'

defCompileBasicOp (Destination [target]) (ConvOp conv e) = do
  e' <- compileSubExp e
  writeExp target $ Imp.ConvOpExp conv e'

defCompileBasicOp (Destination [target]) (BinOp bop x y) = do
  x' <- compileSubExp x
  y' <- compileSubExp y
  writeExp target $ Imp.BinOpExp bop x' y'

defCompileBasicOp (Destination [target]) (CmpOp bop x y) = do
  x' <- compileSubExp x
  y' <- compileSubExp y
  writeExp target $ Imp.CmpOpExp bop x' y'

defCompileBasicOp (Destination [_]) (Assert e loc) = do
  e' <- compileSubExp e
  emit $ Imp.Assert e' loc

defCompileBasicOp (Destination [target]) (Index _ src slice)
  | Just idxs <- sliceIndices slice =
      copyDWIMDest target [] (Var src) $ map (primExpFromSubExp int32) idxs

defCompileBasicOp _ Index{} =
  return ()

defCompileBasicOp (Destination [dest]) (Replicate (Shape ds) se) = do
  is <- replicateM (length ds) (newVName "i")
  ds' <- mapM compileSubExp ds
  declaringLoopVars Int32 is $ do
    copy_elem <- collect $ copyDWIMDest dest (map varIndex is) se []
    emit $ foldl (.) id (zipWith (`Imp.For` Int32) is ds') copy_elem

defCompileBasicOp (Destination [_]) Scratch{} =
  return ()

defCompileBasicOp (Destination [dest]) (Iota n e s et) = do
  i <- newVName "i"
  x <- newVName "x"
  n' <- compileSubExp n
  e' <- compileSubExp e
  s' <- compileSubExp s
  emit $ Imp.DeclareScalar x $ IntType et
  let i' = ConvOpExp (SExt Int32 et) $ Imp.var i $ IntType et
  declaringLoopVar i Int32 $ withPrimVar x (IntType et) $
    emit =<< (Imp.For i et n' <$>
              collect (do emit $ Imp.SetScalar x $ e' + i' * s'
                          copyDWIMDest dest [varIndex i] (Var x) []))

defCompileBasicOp (Destination [target]) (Copy src) =
  compileSubExpTo target $ Var src

defCompileBasicOp _ Split{} =
  return () -- Yes, really.

defCompileBasicOp
  (Destination [ArrayDestination (CopyIntoMemory (MemLocation destmem destshape destixfun)) _])
  (Concat _ i x ys _) = do
    xtype <- lookupType x
    offs_glb <- newVName "tmp_offs"
    withPrimVar offs_glb int32 $ do
      emit $ Imp.DeclareScalar offs_glb int32
      emit $ Imp.SetScalar offs_glb 0
      let perm = [i] ++ [0..i-1] ++ [i+1..length destshape-1]
          invperm = rearrangeInverse perm
          destloc = MemLocation destmem destshape
                    (IxFun.permute (IxFun.offsetIndex (IxFun.permute destixfun perm) $
                                     LeafExp offs_glb int32)
                     invperm)

      forM_ (x:ys) $ \y -> do
          yentry <- lookupArray y
          let srcloc = entryArrayLocation yentry
              rows = case drop i $ entryArrayShape yentry of
                      []  -> error $ "defCompileBasicOp Concat: empty array shape for " ++ pretty y
                      r:_ -> innerExp $ Imp.dimSizeToExp r
          copy (elemType xtype) destloc srcloc $ arrayOuterSize yentry
          emit $ Imp.SetScalar offs_glb $ Imp.var offs_glb int32 + rows

defCompileBasicOp (Destination [dest]) (ArrayLit es _) =
  forM_ (zip [0..] es) $ \(i,e) ->
  copyDWIMDest dest [constIndex i] e []

defCompileBasicOp _ Rearrange{} =
  return ()

defCompileBasicOp _ Rotate{} =
  return ()

defCompileBasicOp _ Reshape{} =
  return ()

defCompileBasicOp (Destination dests) (Partition _ n flags value_arrs)
  | (sizedests, arrdest) <- splitAt n dests,
    Just sizenames <- mapM fromScalarDestination sizedests,
    Just destlocs <- mapM arrDestLoc arrdest = do
  i <- newVName "i"
  declaringLoopVar i Int32 $ do
    outer_dim <- compileSubExp =<< (arraySize 0 <$> lookupType flags)
    -- We will use 'i' to index the flag array and the value array.
    -- Note that they have the same outer size ('outer_dim').
    read_flags_i <- readFromArray flags [varIndex i]

    -- First, for each of the 'n' output arrays, we compute the final
    -- size.  This is done by iterating through the flag array, but
    -- first we declare scalars to hold the size.  We do this by
    -- creating a mapping from equivalence classes to the name of the
    -- scalar holding the size.
    let sizes = HM.fromList $ zip [0..n-1] sizenames

    -- We initialise ecah size to zero.
    forM_ sizenames $ \sizename ->
      emit $ Imp.SetScalar sizename 0

    -- Now iterate across the flag array, storing each element in
    -- 'eqclass', then comparing it to the known classes and increasing
    -- the appropriate size variable.
    eqclass <- newVName "eqclass"
    emit $ Imp.DeclareScalar eqclass int32
    let mkSizeLoopBody code c sizevar =
          Imp.If (Imp.CmpOpExp (CmpEq int32) (Imp.var eqclass int32) (fromIntegral c))
          (Imp.SetScalar sizevar $ Imp.var sizevar int32 + 1)
          code
        sizeLoopBody = HM.foldlWithKey' mkSizeLoopBody Imp.Skip sizes
    emit $ Imp.For i Int32 outer_dim $
      Imp.SetScalar eqclass read_flags_i <>
      sizeLoopBody

    -- We can now compute the starting offsets of each of the
    -- partitions, creating a map from equivalence class to its
    -- corresponding offset.
    offsets <- flip evalStateT 0 $ forM sizes $ \size -> do
      cur_offset <- get
      partition_offset <- lift $ newVName "partition_offset"
      lift $ emit $ Imp.DeclareScalar partition_offset int32
      lift $ emit $ Imp.SetScalar partition_offset cur_offset
      put $ Imp.var partition_offset int32 + Imp.var size int32
      return partition_offset

    -- We create the memory location we use when writing a result
    -- element.  This is basically the index function of 'destloc', but
    -- with a dynamic offset, stored in 'partition_cur_offset'.
    partition_cur_offset <- newVName "partition_cur_offset"
    emit $ Imp.DeclareScalar partition_cur_offset int32

    -- Finally, we iterate through the data array and flag array in
    -- parallel, and put each element where it is supposed to go.  Note
    -- that after writing to a partition, we increase the corresponding
    -- offset.
    ets <- mapM (fmap elemType . lookupType) value_arrs
    srclocs <- mapM arrayLocation value_arrs
    copy_elements <- forM (zip3 destlocs ets srclocs) $ \(destloc,et,srcloc) ->
      copyArrayDWIM et
      destloc [varIndex partition_cur_offset]
      srcloc [varIndex i]
    let mkWriteLoopBody code c offsetvar =
          Imp.If (Imp.CmpOpExp (CmpEq int32) (Imp.var eqclass int32) (fromIntegral c))
          (Imp.SetScalar partition_cur_offset
             (Imp.var offsetvar int32)
           <>
           mconcat copy_elements
           <>
           Imp.SetScalar offsetvar
             (Imp.var offsetvar int32 + 1))
          code
        writeLoopBody = HM.foldlWithKey' mkWriteLoopBody Imp.Skip offsets
    emit $ Imp.For i Int32 outer_dim $
      Imp.SetScalar eqclass read_flags_i <>
      writeLoopBody
    return ()
  where arrDestLoc (ArrayDestination (CopyIntoMemory destloc) _) =
          Just destloc
        arrDestLoc _ =
          Nothing

defCompileBasicOp (Destination []) _ = return () -- No arms, no cake.

defCompileBasicOp target e =
  throwError $ "ImpGen.defCompileBasicOp: Invalid target\n  " ++
  show target ++ "\nfor expression\n  " ++ pretty e

writeExp :: ValueDestination -> Imp.Exp -> ImpM lore op ()
writeExp (ScalarDestination target) e =
  emit $ Imp.SetScalar target e
writeExp (ArrayElemDestination destmem bt space elemoffset) e = do
  vol <- asks envVolatility
  emit $ Imp.Write destmem elemoffset bt space vol e
writeExp target e =
  throwError $ "Cannot write " ++ pretty e ++ " to " ++ show target

insertInVtable :: VName -> VarEntry lore -> Env lore op -> Env lore op
insertInVtable name entry env =
  env { envVtable = HM.insert name entry $ envVtable env }

withArray :: ArrayDecl -> ImpM lore op a -> ImpM lore op a
withArray (ArrayDecl name bt location) m = do
  let entry = ArrayVar Nothing ArrayEntry
              { entryArrayLocation = location
              , entryArrayElemType = bt
              }
  local (insertInVtable name entry) m

withArrays :: [ArrayDecl] -> ImpM lore op a -> ImpM lore op a
withArrays = flip $ foldr withArray

withParams :: [Imp.Param] -> ImpM lore op a -> ImpM lore op a
withParams = flip $ foldr withParam

withParam :: Imp.Param -> ImpM lore op a -> ImpM lore op a
withParam (Imp.MemParam name memsize space) =
  let entry = MemVar Nothing MemEntry { entryMemSize = memsize
                                      , entryMemSpace = space
                                      }
  in local $ insertInVtable name entry
withParam (Imp.ScalarParam name bt) =
  let entry = ScalarVar Nothing ScalarEntry { entryScalarType = bt }
  in local $ insertInVtable name entry

declaringVars :: ExplicitMemorish lore =>
                 Maybe (Exp lore) -> [PatElem lore] -> ImpM lore op a -> ImpM lore op a
declaringVars e = flip $ foldr declaringVar
  where declaringVar = declaringScope e . scopeOf

declaringFParams :: ExplicitMemorish lore => [FParam lore] -> ImpM lore op a -> ImpM lore op a
declaringFParams = declaringScope Nothing . scopeOfFParams

declaringLParams :: ExplicitMemorish lore => [LParam lore] -> ImpM lore op a -> ImpM lore op a
declaringLParams = declaringScope Nothing . scopeOfLParams

declaringVarEntry :: VName -> VarEntry lore -> ImpM lore op a -> ImpM lore op a
declaringVarEntry name entry m = do
  case entry of
    MemVar _ entry' ->
      emit $ Imp.DeclareMem name $ entryMemSpace entry'
    ScalarVar _ entry' ->
      emit $ Imp.DeclareScalar name $ entryScalarType entry'
    ArrayVar _ _ ->
      return ()
  local (insertInVtable name entry) m

declaringPrimVar :: VName -> PrimType -> ImpM lore op a -> ImpM lore op a
declaringPrimVar name bt =
  declaringVarEntry name $ ScalarVar Nothing $ ScalarEntry bt

declaringPrimVars :: [(VName,PrimType)] -> ImpM lore op a -> ImpM lore op a
declaringPrimVars = flip $ foldr (uncurry declaringPrimVar)

declaringName :: Maybe (Exp lore) -> VName -> NameInfo ExplicitMemory
              -> ImpM lore op a -> ImpM lore op a
declaringName e name info m =
  case infoAttr info of
    Scalar bt -> do
      let entry = ScalarVar e ScalarEntry { entryScalarType = bt }
      declaringVarEntry name entry m
    MemMem size space -> do
      size' <- subExpToDimSize size
      let entry = MemVar e MemEntry {
              entryMemSize = size'
            , entryMemSpace = space
            }
      declaringVarEntry name entry m
    ArrayMem bt shape _ mem ixfun -> do
      shape' <- mapM subExpToDimSize $ shapeDims shape
      let location = MemLocation mem shape' ixfun
          entry = ArrayVar e ArrayEntry {
              entryArrayLocation = location
            , entryArrayElemType = bt
            }
      declaringVarEntry name entry m
  where infoAttr (LetInfo attr) = attr
        infoAttr (FParamInfo attr) = const NoUniqueness <$> attr
        infoAttr (LParamInfo attr) = attr
        infoAttr (IndexInfo it) = Scalar $ IntType it

declaringScope :: Maybe (Exp lore) -> Scope ExplicitMemory -> ImpM lore op a -> ImpM lore op a
declaringScope e scope m = foldr (uncurry $ declaringName e) m $ HM.toList scope

declaringScopes :: [(Maybe (Exp lore), Scope ExplicitMemory)] -> ImpM lore op a -> ImpM lore op a
declaringScopes es_and_scopes m = foldr (uncurry declaringScope) m es_and_scopes

withPrimVar :: VName -> PrimType -> ImpM lore op a -> ImpM lore op a
withPrimVar name bt =
  local (insertInVtable name $ ScalarVar Nothing $ ScalarEntry bt)

declaringLoopVars :: IntType -> [VName] -> ImpM lore op a -> ImpM lore op a
declaringLoopVars it = flip $ foldr (`declaringLoopVar` it)

declaringLoopVar :: VName -> IntType -> ImpM lore op a -> ImpM lore op a
declaringLoopVar name it =
  withPrimVar name $ IntType it

everythingVolatile :: ImpM lore op a -> ImpM lore op a
everythingVolatile = local $ \env -> env { envVolatility = Imp.Volatile }

-- | Remove the array targets.
funcallTargets :: Destination -> ImpM lore op [VName]
funcallTargets (Destination dests) =
  concat <$> mapM funcallTarget dests
  where funcallTarget (ScalarDestination name) =
          return [name]
        funcallTarget ArrayElemDestination{} =
          throwError "Cannot put scalar function return in-place yet." -- FIXME
        funcallTarget (ArrayDestination (CopyIntoMemory _) shape) =
          return $ catMaybes shape
        funcallTarget (ArrayDestination (SetMemory mem memsize) shape) =
          return $ maybeToList memsize ++ [mem] ++ catMaybes shape
        funcallTarget (MemoryDestination name size) =
          return $ maybeToList size ++ [name]

subExpToDimSize :: SubExp -> ImpM lore op Imp.DimSize
subExpToDimSize (Var v) =
  return $ Imp.VarSize v
subExpToDimSize (Constant (IntValue (Int32Value i))) =
  return $ Imp.ConstSize $ fromIntegral i
subExpToDimSize Constant{} =
  throwError "Size subexp is not an int32 constant."

compileSubExpTo :: ValueDestination -> SubExp -> ImpM lore op ()
compileSubExpTo dest se = copyDWIMDest dest [] se []

compileSubExp :: SubExp -> ImpM lore op Imp.Exp
compileSubExp (Constant v) =
  return $ Imp.ValueExp v
compileSubExp (Var v) = do
  t <- lookupType v
  case t of
    Prim pt -> return $ Imp.var v pt
    _       -> throwError $ "compileSubExp: SubExp is not a primitive type: " ++ pretty v

compileSubExpOfType :: PrimType -> SubExp -> Imp.Exp
compileSubExpOfType _ (Constant v) = Imp.ValueExp v
compileSubExpOfType t (Var v) = Imp.var v t

compilePrimExp :: PrimExp VName -> Imp.Exp
compilePrimExp = fmap Imp.ScalarVar

varIndex :: VName -> PrimExp VName
varIndex name = LeafExp name int32

constIndex :: Int -> PrimExp VName
constIndex = fromIntegral

lookupVar :: VName -> ImpM lore op (VarEntry lore)
lookupVar name = do
  res <- asks $ HM.lookup name . envVtable
  case res of
    Just entry -> return entry
    _ -> throwError $ "Unknown variable: " ++ textual name

lookupArray :: VName -> ImpM lore op ArrayEntry
lookupArray name = do
  res <- lookupVar name
  case res of
    ArrayVar _ entry -> return entry
    _                -> throwError $ "ImpGen.lookupArray: not an array: " ++ textual name

arrayLocation :: VName -> ImpM lore op MemLocation
arrayLocation name = entryArrayLocation <$> lookupArray name

lookupMemory :: VName -> ImpM lore op MemEntry
lookupMemory name = do
  res <- lookupVar name
  case res of
    MemVar _ entry -> return entry
    _              -> throwError $ "Unknown memory block: " ++ textual name

destinationFromParam :: Param (MemBound u) -> ImpM lore op ValueDestination
destinationFromParam param
  | ArrayMem _ shape _ mem ixfun <- paramAttr param = do
      let dims = shapeDims shape
      memloc <- MemLocation mem <$> mapM subExpToDimSize dims <*> pure ixfun
      return $
        ArrayDestination (CopyIntoMemory memloc)
        (map (const Nothing) dims)
  | otherwise =
      return $ ScalarDestination $ paramName param

destinationFromParams :: [Param (MemBound u)] -> ImpM lore op Destination
destinationFromParams = fmap Destination . mapM destinationFromParam

destinationFromPattern :: ExplicitMemorish lore => Pattern lore -> ImpM lore op Destination
destinationFromPattern (Pattern ctxElems valElems) =
  Destination <$> mapM inspect valElems
  where ctxNames = map patElemName ctxElems
        isctx = (`elem` ctxNames)
        inspect patElem = do
          let name = patElemName patElem
          entry <- lookupVar name
          case entry of
            ArrayVar _ (ArrayEntry (MemLocation mem shape ixfun) bt) ->
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
                BindInPlace _ _ slice ->
                  case (length $ sliceDims slice,
                        sliceIndices slice) of
                    (_, Just is) -> do
                      (_, space, elemOffset) <-
                        fullyIndexArray'
                        (MemLocation mem shape ixfun)
                        (map (primExpFromSubExp int32) is)
                        bt
                      return $ ArrayElemDestination mem bt space elemOffset
                    (r, Nothing) ->
                      let memdest = sliceArray (MemLocation mem shape ixfun) $
                                    map (fmap (primExpFromSubExp int32)) slice
                      in return $
                         ArrayDestination (CopyIntoMemory memdest) $
                         replicate r Nothing

            MemVar _ (MemEntry memsize _)
              | Imp.VarSize memsize' <- memsize, isctx memsize' ->
                return $ MemoryDestination name $ Just memsize'
              | otherwise ->
                return $ MemoryDestination name Nothing

            ScalarVar{} ->
              return $ ScalarDestination name

fullyIndexArray :: VName -> [PrimExp VName]
                -> ImpM lore op (VName, Imp.Space, Count Bytes)
fullyIndexArray name indices = do
  arr <- lookupArray name
  fullyIndexArray' (entryArrayLocation arr) indices $ entryArrayElemType arr

fullyIndexArray' :: MemLocation -> [PrimExp VName] -> PrimType
                 -> ImpM lore op (VName, Imp.Space, Count Bytes)
fullyIndexArray' (MemLocation mem _ ixfun) indices bt = do
  space <- entryMemSpace <$> lookupMemory mem
  return (mem, space,
          bytes $ compilePrimExp $
          IxFun.index ixfun indices $ primByteSize bt)

readFromArray :: VName -> [PrimExp VName]
              -> ImpM lore op Imp.Exp
readFromArray name indices = do
  arr <- lookupArray name
  (mem, space, i) <-
    fullyIndexArray' (entryArrayLocation arr) indices $ entryArrayElemType arr
  vol <- asks envVolatility
  return $ Imp.index mem i (entryArrayElemType arr) space vol

sliceArray :: MemLocation
           -> Slice (PrimExp VName)
           -> MemLocation
sliceArray (MemLocation mem shape ixfun) slice =
  MemLocation mem (update shape slice) $ IxFun.slice ixfun slice
  where update (d:ds) (DimSlice{}:is) = d : update ds is
        update (_:ds) (DimFix{}:is) = update ds is
        update _      _               = []

offsetArray :: MemLocation
            -> PrimExp VName
            -> MemLocation
offsetArray (MemLocation mem shape ixfun) offset =
  MemLocation mem shape $ IxFun.offsetIndex ixfun offset

strideArray :: MemLocation
            -> PrimExp VName
            -> MemLocation
strideArray (MemLocation mem shape ixfun) stride =
  MemLocation mem shape $ IxFun.strideIndex ixfun stride

subExpNotArray :: SubExp -> ImpM lore op Bool
subExpNotArray se = subExpType se >>= \case
  Array {} -> return False
  _        -> return True

arrayOuterSize :: ArrayEntry -> Count Elements
arrayOuterSize = arrayDimSize 0

arrayDimSize :: Int -> ArrayEntry -> Count Elements
arrayDimSize i =
  product . map Imp.dimSizeToExp . take 1 . drop i . entryArrayShape

-- More complicated read/write operations that use index functions.

copy :: CopyCompiler lore op
copy bt dest src n = do
  cc <- asks envCopyCompiler
  cc bt dest src n

-- | Use an 'Imp.Copy' if possible, otherwise 'copyElementWise'.
defaultCopy :: CopyCompiler lore op
defaultCopy bt dest src n
  | ixFunMatchesInnerShape
      (Shape $ map dimSizeToSubExp destshape) destIxFun,
    ixFunMatchesInnerShape
      (Shape $ map dimSizeToSubExp srcshape) srcIxFun,
    Just destoffset <-
      IxFun.linearWithOffset destIxFun bt_size,
    Just srcoffset  <-
      IxFun.linearWithOffset srcIxFun bt_size = do
        srcspace <- entryMemSpace <$> lookupMemory srcmem
        destspace <- entryMemSpace <$> lookupMemory destmem
        emit $ Imp.Copy
          destmem (bytes $ compilePrimExp destoffset) destspace
          srcmem (bytes $ compilePrimExp srcoffset) srcspace $
          (n * row_size) `withElemType` bt
  | otherwise =
      copyElementWise bt dest src n
  where bt_size = primByteSize bt
        row_size = product $ map Imp.dimSizeToExp $ drop 1 srcshape
        MemLocation destmem destshape destIxFun = dest
        MemLocation srcmem srcshape srcIxFun = src

copyElementWise :: CopyCompiler lore op
copyElementWise bt (MemLocation destmem _ destIxFun) (MemLocation srcmem srcshape srcIxFun) n = do
    is <- replicateM (IxFun.rank destIxFun) (newVName "i")
    declaringLoopVars Int32 is $ do
      let ivars = map varIndex is
          destidx = IxFun.index destIxFun ivars bt_size
          srcidx = IxFun.index srcIxFun ivars bt_size
          bounds = map innerExp $ n : drop 1 (map Imp.dimSizeToExp srcshape)
      srcspace <- entryMemSpace <$> lookupMemory srcmem
      destspace <- entryMemSpace <$> lookupMemory destmem
      vol <- asks envVolatility
      emit $ foldl (.) id (zipWith (`Imp.For` Int32) is bounds) $
        Imp.Write destmem (bytes $ compilePrimExp destidx) bt destspace vol $
        Imp.index srcmem (bytes $ compilePrimExp srcidx) bt srcspace vol
  where bt_size = primByteSize bt

-- | Copy from here to there; both destination and source may be
-- indexeded.
copyArrayDWIM :: PrimType
              -> MemLocation -> [PrimExp VName]
              -> MemLocation -> [PrimExp VName]
              -> ImpM lore op (Imp.Code op)
copyArrayDWIM bt
  destlocation@(MemLocation _ destshape dest_ixfun) destis
  srclocation@(MemLocation _ srcshape src_ixfun) srcis

  | length srcis == length srcshape, length destis == length destshape = do
  (targetmem, destspace, targetoffset) <-
    fullyIndexArray' destlocation destis bt
  (srcmem, srcspace, srcoffset) <-
    fullyIndexArray' srclocation srcis bt
  vol <- asks envVolatility
  return $ Imp.Write targetmem targetoffset bt destspace vol $
    Imp.index srcmem srcoffset bt srcspace vol

  | otherwise = do
      let destlocation' =
            sliceArray destlocation $ fullSliceNum (IxFun.shape dest_ixfun) $ map DimFix destis
          srclocation'  =
            sliceArray srclocation $ fullSliceNum (IxFun.shape src_ixfun) $ map DimFix srcis
      if destlocation' == srclocation'
        then return mempty -- Copy would be no-op.
        else collect $ copy bt destlocation' srclocation' $
             product $ map Imp.dimSizeToExp $
             take 1 $ drop (length srcis) srcshape

-- | Like 'copyDWIM', but the target is a 'ValueDestination'
-- instead of a variable name.
copyDWIMDest :: ValueDestination -> [PrimExp VName] -> SubExp -> [PrimExp VName]
             -> ImpM lore op ()

copyDWIMDest _ _ (Constant v) (_:_) =
  throwError $
  unwords ["copyDWIMDest: constant source", pretty v, "cannot be indexed."]
copyDWIMDest dest dest_is (Constant v) [] =
  case dest of
  ScalarDestination name ->
    emit $ Imp.SetScalar name $ Imp.ValueExp v
  ArrayElemDestination dest_mem _ dest_space dest_i -> do
    vol <- asks envVolatility
    emit $ Imp.Write dest_mem dest_i bt dest_space vol $ Imp.ValueExp v
  MemoryDestination{} ->
    throwError $
    unwords ["copyDWIMDest: constant source", pretty v, "cannot be written to memory destination."]
  ArrayDestination (CopyIntoMemory dest_loc) _ -> do
    (dest_mem, dest_space, dest_i) <-
      fullyIndexArray' dest_loc dest_is bt
    vol <- asks envVolatility
    emit $ Imp.Write dest_mem dest_i bt dest_space vol $ Imp.ValueExp v
  ArrayDestination{} ->
    throwError $
    unwords ["copyDWIMDest: constant source", pretty v,
             "cannot be written to array destination that is not CopyIntoMEmory"]
  where bt = primValueType v

copyDWIMDest dest dest_is (Var src) src_is = do
  src_entry <- lookupVar src
  case (dest, src_entry) of
    (MemoryDestination mem memsizetarget, MemVar _ (MemEntry memsize space)) -> do
      emit $ Imp.SetMem mem src space
      case memsizetarget of
        Nothing ->
          return ()
        Just memsizetarget' ->
          emit $ Imp.SetScalar memsizetarget' $
          innerExp $ Imp.dimSizeToExp memsize

    (MemoryDestination{}, _) ->
      throwError $
      unwords ["copyDWIMDest: cannot write", pretty src, "to memory destination."]

    (_, MemVar{}) ->
      throwError $
      unwords ["copyDWIMDest: source", pretty src, "is a memory block."]

    (_, ScalarVar _ (ScalarEntry _)) | not $ null src_is ->
      throwError $
      unwords ["copyDWIMDest: prim-typed source", pretty src, "with nonzero indices."]


    (ScalarDestination name, _) | not $ null dest_is ->
      throwError $
      unwords ["copyDWIMDest: prim-typed target", pretty name, "with nonzero indices."]

    (ScalarDestination name, ScalarVar _ (ScalarEntry pt)) ->
      emit $ Imp.SetScalar name $ Imp.var src pt

    (ScalarDestination name, ArrayVar _ arr) -> do
      let bt = entryArrayElemType arr
      (mem, space, i) <-
        fullyIndexArray' (entryArrayLocation arr) src_is bt
      vol <- asks envVolatility
      emit $ Imp.SetScalar name $ Imp.index mem i bt space vol

    (ArrayElemDestination{}, _) | not $ null dest_is->
      throwError $
      unwords ["copyDWIMDest: array elemenent destination given indices:", pretty dest_is]

    (ArrayElemDestination dest_mem _ dest_space dest_i,
     ScalarVar _ (ScalarEntry bt)) -> do
      vol <- asks envVolatility
      emit $ Imp.Write dest_mem dest_i bt dest_space vol $ Imp.var src bt

    (ArrayElemDestination dest_mem _ dest_space dest_i, ArrayVar _ src_arr)
      | length (entryArrayShape src_arr) == length src_is -> do
          let bt = entryArrayElemType src_arr
          (src_mem, src_space, src_i) <-
            fullyIndexArray' (entryArrayLocation src_arr) src_is bt
          vol <- asks envVolatility
          emit $ Imp.Write dest_mem dest_i bt dest_space vol $
            Imp.index src_mem src_i bt src_space vol

    (ArrayElemDestination{}, ArrayVar{}) ->
      throwError $
      unwords ["copyDWIMDest: array element destination, but array source",
               pretty src,
               "with incomplete indexing."]

    (ArrayDestination (CopyIntoMemory dest_loc) dest_dims, ArrayVar _ src_arr) -> do
      let src_loc = entryArrayLocation src_arr
          bt = entryArrayElemType src_arr
      emit =<< copyArrayDWIM bt dest_loc dest_is src_loc src_is
      zipWithM_ maybeSetShape dest_dims $ entryArrayShape src_arr

    (ArrayDestination (CopyIntoMemory dest_loc) _, ScalarVar _ (ScalarEntry bt)) -> do
      (dest_mem, dest_space, dest_i) <-
        fullyIndexArray' dest_loc dest_is bt
      vol <- asks envVolatility
      emit $ Imp.Write dest_mem dest_i bt dest_space vol (Imp.var src bt)

    (ArrayDestination{} , ScalarVar{}) ->
      throwError $
      "copyDWIMDest: array destination but scalar source" <>
      pretty src

    (ArrayDestination (SetMemory dest_mem dest_memsize) dest_dims, ArrayVar _ src_arr) -> do
      let src_mem = memLocationName $ entryArrayLocation src_arr
      space <- entryMemSpace <$> lookupMemory src_mem
      srcmemsize <- entryMemSize <$> lookupMemory src_mem
      emit $ Imp.SetMem dest_mem src_mem space
      zipWithM_ maybeSetShape dest_dims $ entryArrayShape src_arr
      case dest_memsize of
        Nothing -> return ()
        Just dest_memsize' -> emit $ Imp.SetScalar dest_memsize' $
                              innerExp $ Imp.memSizeToExp srcmemsize


  where maybeSetShape Nothing _ =
          return ()
        maybeSetShape (Just dim) size =
          emit $ Imp.SetScalar dim $ innerExp $ Imp.dimSizeToExp size

-- | Copy from here to there; both destination and source be
-- indexeded.  If so, they better be arrays of enough dimensions.
-- This function will generally just Do What I Mean, and Do The Right
-- Thing.  Both destination and source must be in scope.
copyDWIM :: VName -> [PrimExp VName] -> SubExp -> [PrimExp VName]
         -> ImpM lore op ()
copyDWIM dest dest_is src src_is = do
  dest_entry <- lookupVar dest
  let dest_target =
        case dest_entry of
          ScalarVar _ _ ->
            ScalarDestination dest

          ArrayVar _ (ArrayEntry (MemLocation mem shape ixfun) _) ->
            ArrayDestination
            (CopyIntoMemory (MemLocation mem shape ixfun)) $
            replicate (length shape) Nothing

          MemVar _ _ ->
            MemoryDestination dest Nothing
  copyDWIMDest dest_target dest_is src src_is

-- | @compileAlloc dest size space@ allocates @n@ bytes of memory in @space@,
-- writing the result to @dest@, which must be a single
-- 'MemoryDestination',
compileAlloc :: Destination -> SubExp -> Space
             -> ImpM lore op ()
compileAlloc (Destination [MemoryDestination mem sizevar]) e space = do
  e' <- compileSubExp e
  emit $ Imp.Allocate mem (Imp.bytes e') space
  case sizevar of Just sizevar' -> emit $ Imp.SetScalar sizevar' e'
                  Nothing       -> return ()
compileAlloc dest _ _ =
  throwError $ "compileAlloc: Invalid destination: " ++ show dest

dimSizeToSubExp :: Imp.Size -> SubExp
dimSizeToSubExp (Imp.ConstSize n) = constant n
dimSizeToSubExp (Imp.VarSize v) = Var v

dimSizeToPrimExp :: Imp.Size -> PrimExp VName
dimSizeToPrimExp = primExpFromSubExp int32 . dimSizeToSubExp
