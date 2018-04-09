{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, LambdaCase, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.CodeGen.ImpGen
  ( -- * Entry Points
    compileProg

    -- * Pluggable Compiler
  , OpCompiler
  , ExpCompiler
  , CopyCompiler
  , BodyCompiler
  , Operations (..)
  , defaultOperations
  , Destination (..)
  , ValueDestination (..)
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
  , lookupVar
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
  , declaringPrimVar
  , declaringPrimVars
  , withPrimVar
  , everythingVolatile
  , compileBody
  , compileLoopBody
  , defCompileBody
  , compileStms
  , compileExp
  , defCompileExp
  , sliceArray
  , offsetArray
  , strideArray
  , fullyIndexArray
  , fullyIndexArray'
  , varIndex
  , Imp.dimSizeToExp
  , dimSizeToSubExp
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

import Control.Monad.RWS    hiding (mapM, forM)
import Control.Monad.State  hiding (mapM, forM)
import Control.Monad.Writer hiding (mapM, forM)
import Control.Monad.Except hiding (mapM, forM)
import qualified Control.Monad.Fail as Fail
import Data.Either
import Data.Traversable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.List
import Data.Ord

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
import Futhark.Error
import Futhark.Util

-- | How to compile an 'Op'.
type OpCompiler lore op = Destination -> Op lore -> ImpM lore op ()

-- | How to compile a 'Body'.
type BodyCompiler lore op = Destination -> Body lore -> ImpM lore op ()

-- | How to compile an 'Exp'.
type ExpCompiler lore op = Destination -> Exp lore -> ImpM lore op ()

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
defaultOperations opc = Operations { opsExpCompiler = defCompileExp
                                   , opsOpCompiler = opc
                                   , opsBodyCompiler = defCompileBody
                                   , opsCopyCompiler = defaultCopy
                                   }

-- | When an array is declared, this is where it is stored.
data MemLocation = MemLocation { memLocationName :: VName
                               , memLocationShape :: [Imp.DimSize]
                               , memLocationIxFun :: IxFun.IxFun Imp.Exp
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

newtype ScalarEntry = ScalarEntry {
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

data ValueDestination = ScalarDestination VName
                      | ArrayElemDestination VName PrimType Imp.Space (Count Bytes)
                      | MemoryDestination VName
                      | ArrayDestination (Maybe MemLocation)
                        -- ^ The 'MemLocation' is 'Just' if a copy if
                        -- required.  If it is 'Nothing', then a
                        -- copy/assignment of a memory block somewhere
                        -- takes care of this array.
                      deriving (Show)

-- | If the given value destination if a 'ScalarDestination', return
-- the variable name.  Otherwise, 'Nothing'.
fromScalarDestination :: ValueDestination -> Maybe VName
fromScalarDestination (ScalarDestination name) = Just name
fromScalarDestination _                        = Nothing

data Env lore op = Env {
    envVtable :: M.Map VName (VarEntry lore)
  , envExpCompiler :: ExpCompiler lore op
  , envBodyCompiler :: BodyCompiler lore op
  , envOpCompiler :: OpCompiler lore op
  , envCopyCompiler :: CopyCompiler lore op
  , envDefaultSpace :: Imp.Space
  , envVolatility :: Imp.Volatility
  }

newEnv :: Operations lore op -> Imp.Space -> Env lore op
newEnv ops ds = Env { envVtable = M.empty
                    , envExpCompiler = opsExpCompiler ops
                    , envBodyCompiler = opsBodyCompiler ops
                    , envOpCompiler = opsOpCompiler ops
                    , envCopyCompiler = opsCopyCompiler ops
                    , envDefaultSpace = ds
                    , envVolatility = Imp.Nonvolatile
                    }

newtype ImpM lore op a = ImpM (RWST (Env lore op) (Imp.Code op) VNameSource (Either InternalError) a)
  deriving (Functor, Applicative, Monad,
            MonadState VNameSource,
            MonadReader (Env lore op),
            MonadWriter (Imp.Code op),
            MonadError InternalError)

instance Fail.MonadFail (ImpM lore op) where
  fail = error . ("ImpM.fail: "++)

instance MonadFreshNames (ImpM lore op) where
  getNameSource = get
  putNameSource = put

instance HasScope SOACS (ImpM lore op) where
  askScope = M.map (LetInfo . entryType) <$> asks envVtable
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
        -> Either InternalError (a, VNameSource, Imp.Code op)
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
                     , envVtable = M.map scrubExps $ envVtable env
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
            -> Prog lore -> m (Either InternalError (Imp.Functions op))
compileProg ops ds prog =
  modifyNameSource $ \src ->
  case mapAccumLM (compileFunDef ops ds) src (progFunctions prog) of
    Left err -> (Left err, src)
    Right (src', funs) -> (Right $ Imp.Functions funs, src')

compileInParam :: ExplicitMemorish lore =>
                  FParam lore -> ImpM lore op (Either Imp.Param ArrayDecl)
compileInParam fparam = case paramAttr fparam of
  MemPrim bt ->
    return $ Left $ Imp.ScalarParam name bt
  MemMem _ space ->
    return $ Left $ Imp.MemParam name space
  MemArray bt shape _ (ArrayIn mem ixfun) -> do
    shape' <- mapM subExpToDimSize $ shapeDims shape
    return $ Right $ ArrayDecl name bt $
      MemLocation mem shape' $ fmap compilePrimExp ixfun
  where name = paramName fparam

data ArrayDecl = ArrayDecl VName PrimType MemLocation

fparamSizes :: Typed attr => Param attr -> S.Set VName
fparamSizes fparam
  | Mem (Var size) _ <- paramType fparam = S.singleton size
  | otherwise = S.fromList $ subExpVars $ arrayDims $ paramType fparam

compileInParams :: ExplicitMemorish lore =>
                   [FParam lore] -> [EntryPointType]
                -> ImpM lore op ([Imp.Param], [ArrayDecl], [Imp.ExternalValue])
compileInParams params orig_epts = do
  let (ctx_params, val_params) =
        splitAt (length params - sum (map entryPointSize orig_epts)) params
  (inparams, arraydecls) <- partitionEithers <$> mapM compileInParam (ctx_params++val_params)
  let findArray x = find (isArrayDecl x) arraydecls
      sizes = mconcat $ map fparamSizes $ ctx_params++val_params

      summaries = M.fromList $ mapMaybe memSummary params
        where memSummary param
                | MemMem (Constant (IntValue (Int64Value size))) space <- paramAttr param =
                    Just (paramName param, (Imp.ConstSize size, space))
                | MemMem (Var size) space <- paramAttr param =
                    Just (paramName param, (Imp.VarSize size, space))
                | otherwise =
                    Nothing

      findMemInfo :: VName -> Maybe (Imp.MemSize, Space)
      findMemInfo = flip M.lookup summaries

      mkValueDesc fparam signedness =
        case (findArray $ paramName fparam, paramType fparam) of
          (Just (ArrayDecl _ bt (MemLocation mem shape _)), _) -> do
            (memsize, memspace) <- findMemInfo mem
            Just $ Imp.ArrayValue mem memsize memspace bt signedness shape
          (_, Prim bt)
            | paramName fparam `S.member` sizes ->
              Nothing
            | otherwise ->
              Just $ Imp.ScalarValue bt signedness $ paramName fparam
          _ ->
            Nothing

      mkExts (TypeOpaque desc n:epts) fparams =
        let (fparams',rest) = splitAt n fparams
        in Imp.OpaqueValue desc
           (mapMaybe (`mkValueDesc` Imp.TypeDirect) fparams') :
           mkExts epts rest
      mkExts (TypeUnsigned:epts) (fparam:fparams) =
        maybeToList (Imp.TransparentValue <$> mkValueDesc fparam Imp.TypeUnsigned) ++
        mkExts epts fparams
      mkExts (TypeDirect:epts) (fparam:fparams) =
        maybeToList (Imp.TransparentValue <$> mkValueDesc fparam Imp.TypeDirect) ++
        mkExts epts fparams
      mkExts _ _ = []

  return (inparams, arraydecls, mkExts orig_epts val_params)
  where isArrayDecl x (ArrayDecl y _ _) = x == y

compileOutParams :: ExplicitMemorish lore =>
                    [RetType lore] -> [EntryPointType]
                 -> ImpM lore op ([Imp.ExternalValue], [Imp.Param], Destination)
compileOutParams orig_rts orig_epts = do
  ((extvs, dests), (outparams,ctx_dests)) <-
    runWriterT $ evalStateT (mkExts orig_epts orig_rts) (M.empty, M.empty)
  let ctx_dests' = map snd $ sortBy (comparing fst) $ M.toList ctx_dests
  return (extvs, outparams, Destination $ ctx_dests' <> dests)
  where imp = lift . lift

        mkExts (TypeOpaque desc n:epts) rts = do
          let (rts',rest) = splitAt n rts
          (evs, dests) <- unzip <$> zipWithM mkParam rts' (repeat Imp.TypeDirect)
          (more_values, more_dests) <- mkExts epts rest
          return (Imp.OpaqueValue desc evs : more_values,
                  dests ++ more_dests)
        mkExts (TypeUnsigned:epts) (rt:rts) = do
          (ev,dest) <- mkParam rt Imp.TypeUnsigned
          (more_values, more_dests) <- mkExts epts rts
          return (Imp.TransparentValue ev : more_values,
                  dest : more_dests)
        mkExts (TypeDirect:epts) (rt:rts) = do
          (ev,dest) <- mkParam rt Imp.TypeDirect
          (more_values, more_dests) <- mkExts epts rts
          return (Imp.TransparentValue ev : more_values,
                  dest : more_dests)
        mkExts _ _ = return ([], [])

        mkParam MemMem{} _ =
          compilerBugS "Functions may not explicitly return memory blocks."
        mkParam (MemPrim t) ept = do
          out <- imp $ newVName "scalar_out"
          tell ([Imp.ScalarParam out t], mempty)
          return (Imp.ScalarValue t ept out, ScalarDestination out)
        mkParam (MemArray t shape _ attr) ept = do
          space <- asks envDefaultSpace
          (memout, memsize) <- case attr of
            ReturnsNewBlock _ x x_size _ixfun -> do
              memout <- imp $ newVName "out_mem"
              sizeout <- ensureMemSizeOut x_size
              tell ([Imp.MemParam memout space],
                    M.singleton x $ MemoryDestination memout)
              return (memout, sizeout)
            ReturnsInBlock memout _ -> do
              memsize <- imp $ entryMemSize <$> lookupMemory memout
              return (memout, memsize)
          resultshape <- mapM inspectExtSize $ shapeDims shape
          return (Imp.ArrayValue memout memsize space t ept resultshape,
                  ArrayDestination Nothing)

        inspectExtSize (Ext x) = do
          (memseen,arrseen) <- get
          case M.lookup x arrseen of
            Nothing -> do
              out <- imp $ newVName "out_arrsize"
              tell ([Imp.ScalarParam out int32],
                    M.singleton x $ ScalarDestination out)
              put (memseen, M.insert x out arrseen)
              return $ Imp.VarSize out
            Just out ->
              return $ Imp.VarSize out
        inspectExtSize (Free se) =
          imp $ subExpToDimSize se

        -- | Return the name of the out-parameter for the memory size
        -- 'x', creating it if it does not already exist.
        ensureMemSizeOut (Ext x) = do
          (memseen, arrseen) <- get
          case M.lookup x memseen of
            Nothing -> do sizeout <- imp $ newVName "out_memsize"
                          tell ([Imp.ScalarParam sizeout int64],
                                M.singleton x $ ScalarDestination sizeout)
                          put (M.insert x sizeout memseen, arrseen)
                          return $ Imp.VarSize sizeout
            Just sizeout -> return $ Imp.VarSize sizeout
        ensureMemSizeOut (Free v) = imp $ subExpToDimSize v

compileFunDef :: ExplicitMemorish lore =>
                 Operations lore op -> Imp.Space
              -> VNameSource
              -> FunDef lore
              -> Either InternalError (VNameSource, (Name, Imp.Function op))
compileFunDef ops ds src (FunDef entry fname rettype params body) = do
  ((outparams, inparams, results, args), src', body') <-
    runImpM compile ops ds src
  return (src',
          (fname,
           Imp.Function (isJust entry) outparams inparams body' results args))
  where params_entry = maybe (replicate (length params) TypeDirect) fst entry
        ret_entry = maybe (replicate (length rettype) TypeDirect) snd entry
        compile = do
          (inparams, arraydecls, args) <- compileInParams params params_entry
          (results, outparams, dests) <- compileOutParams rettype ret_entry
          withFParams params $
            withArrays arraydecls $
            compileBody dests body
          return (outparams, inparams, results, args)

compileBody :: Destination -> Body lore -> ImpM lore op ()
compileBody dest body = do
  cb <- asks envBodyCompiler
  cb dest body

defCompileBody :: ExplicitMemorish lore => Destination -> Body lore -> ImpM lore op ()
defCompileBody (Destination dest) (Body _ bnds ses) =
  compileStms (stmsToList bnds) $ zipWithM_ compileSubExpTo dest ses

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
  collect $ compileStms (stmsToList bnds) $ do
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

compileExp :: Destination -> Exp lore -> ImpM lore op a -> ImpM lore op a
compileExp targets e m = do
  ec <- asks envExpCompiler
  ec targets e
  m

defCompileExp :: ExplicitMemorish lore => Destination -> Exp lore -> ImpM lore op ()

defCompileExp dest (If cond tbranch fbranch _) = do
  cond' <- compileSubExp cond
  tcode <- collect $ compileBody dest tbranch
  fcode <- collect $ compileBody dest fbranch
  emit $ Imp.If cond' tcode fcode

defCompileExp dest (Apply fname args _ _) = do
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
        ForLoop i it bound loopvars -> do
          bound' <- compileSubExp bound
          let setLoopParam (p,a)
                | Prim _ <- paramType p =
                    copyDWIM (paramName p) [] (Var a) [varIndex i]
                | otherwise =
                    return ()

          let emitForm body' = do
                set_loop_params <- collect $ mapM_ setLoopParam loopvars
                emit $ Imp.For i it bound' $ set_loop_params<>body'
          return (declaringLParams (map fst loopvars) .
                  declaringLoopVar i it,
                  emitForm)
        WhileLoop cond ->
          return (id, emit . Imp.While (Imp.var cond Bool))

    bindForm $ do
      body' <- compileLoopBody mergenames body
      emitForm body'
    zipWithM_ compileSubExpTo dest $ map (Var . paramName . fst) merge
    where merge = ctx ++ val
          mergepat = map fst merge
          mergenames = map paramName mergepat

defCompileExp dest (Op op) = do
  opc <- asks envOpCompiler
  opc dest op

defCompileBasicOp :: Destination -> BasicOp lore -> ImpM lore op ()

defCompileBasicOp (Destination [target]) (SubExp se) =
  compileSubExpTo target se

defCompileBasicOp (Destination [target]) (Opaque se) =
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

defCompileBasicOp (Destination [_]) (Assert e msg loc) = do
  e' <- compileSubExp e
  emit $ Imp.Assert e' msg loc

defCompileBasicOp (Destination [target]) (Index src slice)
  | Just idxs <- sliceIndices slice =
      copyDWIMDest target [] (Var src) $ map (compileSubExpOfType int32) idxs

defCompileBasicOp _ Index{} =
  return ()

defCompileBasicOp (Destination [ArrayDestination (Just memloc)]) (Update _ slice se)
  | MemLocation mem shape ixfun <- memloc = do
    bt <- elemType <$> subExpType se
    target' <-
      case sliceIndices slice of
        Just is -> do
          (_, space, elemOffset) <-
            fullyIndexArray'
            (MemLocation mem shape ixfun)
            (map (compileSubExpOfType int32) is)
            bt
          return $ ArrayElemDestination mem bt space elemOffset
        Nothing ->
          let memdest = sliceArray (MemLocation mem shape ixfun) $
                        map (fmap (compileSubExpOfType int32)) slice
          in return $ ArrayDestination $ Just memdest

    copyDWIMDest target' [] se []

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
  let i' = ConvOpExp (SExt Int32 et) $ Imp.var i $ IntType Int32
  declaringLoopVar i Int32 $ withPrimVar x (IntType et) $
    emit =<< (Imp.For i Int32 n' <$>
              collect (do emit $ Imp.SetScalar x $ e' + i' * s'
                          copyDWIMDest dest [varIndex i] (Var x) []))

defCompileBasicOp (Destination [target]) (Copy src) =
  compileSubExpTo target $ Var src

defCompileBasicOp (Destination [target]) (Manifest _ src) =
  compileSubExpTo target $ Var src

defCompileBasicOp
  (Destination [ArrayDestination (Just (MemLocation destmem destshape destixfun))])
  (Concat i x ys _) = do
    xtype <- lookupType x
    offs_glb <- newVName "tmp_offs"
    withPrimVar offs_glb int32 $ do
      emit $ Imp.DeclareScalar offs_glb int32
      emit $ Imp.SetScalar offs_glb 0
      let perm = [i] ++ [0..i-1] ++ [i+1..length destshape-1]
          invperm = rearrangeInverse perm
          destloc = MemLocation destmem destshape
                    (IxFun.permute (IxFun.offsetIndex (IxFun.permute destixfun perm) $
                                    varIndex offs_glb)
                     invperm)

      forM_ (x:ys) $ \y -> do
          yentry <- lookupArray y
          let srcloc = entryArrayLocation yentry
              rows = case drop i $ entryArrayShape yentry of
                      []  -> error $ "defCompileBasicOp Concat: empty array shape for " ++ pretty y
                      r:_ -> innerExp $ Imp.dimSizeToExp r
          copy (elemType xtype) destloc srcloc $ arrayOuterSize yentry
          emit $ Imp.SetScalar offs_glb $ Imp.var offs_glb int32 + rows

defCompileBasicOp (Destination [dest]) (ArrayLit es _)
  | ArrayDestination (Just dest_mem) <- dest,
    Just (vs@(v:_)) <- mapM isLiteral es = do
      dest_space <- entryMemSpace <$> lookupMemory (memLocationName dest_mem)
      let t = primValueType v
      static_array <- newVName "static_array"
      emit $ Imp.DeclareArray static_array dest_space t vs
      let static_src = MemLocation static_array [Imp.ConstSize $ genericLength es] $
                       IxFun.iota [genericLength es]
          num_bytes = Imp.ConstSize $ genericLength es * primByteSize t
          entry = MemVar Nothing $ MemEntry num_bytes dest_space
      local (insertInVtable static_array entry) $
        copy t dest_mem static_src $ genericLength es
  | otherwise =
    forM_ (zip [0..] es) $ \(i,e) ->
      copyDWIMDest dest [constIndex i] e []

  where isLiteral (Constant v) = Just v
        isLiteral _ = Nothing

defCompileBasicOp _ Rearrange{} =
  return ()

defCompileBasicOp _ Rotate{} =
  return ()

defCompileBasicOp _ Reshape{} =
  return ()

defCompileBasicOp _ Repeat{} =
  return ()

defCompileBasicOp (Destination dests) (Partition n flags value_arrs)
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
    let sizes = M.fromList $ zip [0..n-1] sizenames

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
        sizeLoopBody = M.foldlWithKey' mkSizeLoopBody Imp.Skip sizes
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
        writeLoopBody = M.foldlWithKey' mkWriteLoopBody Imp.Skip offsets
    emit $ Imp.For i Int32 outer_dim $
      Imp.SetScalar eqclass read_flags_i <>
      writeLoopBody
    return ()
  where arrDestLoc (ArrayDestination destloc) = destloc
        arrDestLoc _ = Nothing

defCompileBasicOp (Destination []) _ = return () -- No arms, no cake.

defCompileBasicOp target e =
  compilerBugS $ "ImpGen.defCompileBasicOp: Invalid target\n  " ++
  show target ++ "\nfor expression\n  " ++ pretty e

writeExp :: ValueDestination -> Imp.Exp -> ImpM lore op ()
writeExp (ScalarDestination target) e =
  emit $ Imp.SetScalar target e
writeExp (ArrayElemDestination destmem bt space elemoffset) e = do
  vol <- asks envVolatility
  emit $ Imp.Write destmem elemoffset bt space vol e
writeExp target e =
  compilerBugS $ "Cannot write " ++ pretty e ++ " to " ++ show target

insertInVtable :: VName -> VarEntry lore -> Env lore op -> Env lore op
insertInVtable name entry env =
  env { envVtable = M.insert name entry $ envVtable env }

withArray :: ArrayDecl -> ImpM lore op a -> ImpM lore op a
withArray (ArrayDecl name bt location) m = do
  let entry = ArrayVar Nothing ArrayEntry
              { entryArrayLocation = location
              , entryArrayElemType = bt
              }
  local (insertInVtable name entry) m

withArrays :: [ArrayDecl] -> ImpM lore op a -> ImpM lore op a
withArrays = flip $ foldr withArray

-- | Like 'declaringFParams', but does not create new declarations.
withFParams :: ExplicitMemorish lore => [FParam lore] -> ImpM lore op a -> ImpM lore op a
withFParams = flip $ foldr withFParam
  where withFParam fparam m = do
          entry <- memBoundToVarEntry Nothing $ noUniquenessReturns $ paramAttr fparam
          local (insertInVtable (paramName fparam) entry) m

declaringVars :: ExplicitMemorish lore =>
                 Maybe (Exp lore) -> [PatElem lore] -> ImpM lore op a -> ImpM lore op a
declaringVars e = flip $ foldr declaringVar
  where declaringVar = declaringScope e . scopeOfPatElem

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

memBoundToVarEntry :: Maybe (Exp lore) -> MemBound NoUniqueness
                   -> ImpM lore op (VarEntry lore)
memBoundToVarEntry e (MemPrim bt) =
  return $ ScalarVar e ScalarEntry { entryScalarType = bt }
memBoundToVarEntry e (MemMem size space) = do
  size' <- subExpToDimSize size
  return $ MemVar e MemEntry { entryMemSize = size'
                             , entryMemSpace = space
                             }
memBoundToVarEntry e (MemArray bt shape _ (ArrayIn mem ixfun)) = do
  shape' <- mapM subExpToDimSize $ shapeDims shape
  let location = MemLocation mem shape' $ fmap compilePrimExp ixfun
  return $ ArrayVar e ArrayEntry { entryArrayLocation = location
                                 , entryArrayElemType = bt
                                 }

declaringName :: Maybe (Exp lore) -> VName -> NameInfo ExplicitMemory
              -> ImpM lore op a -> ImpM lore op a
declaringName e name info m = do
  entry <- memBoundToVarEntry e $ infoAttr info
  declaringVarEntry name entry m
  where infoAttr (LetInfo attr) = attr
        infoAttr (FParamInfo attr) = noUniquenessReturns attr
        infoAttr (LParamInfo attr) = attr
        infoAttr (IndexInfo it) = MemPrim $ IntType it

declaringScope :: Maybe (Exp lore) -> Scope ExplicitMemory -> ImpM lore op a -> ImpM lore op a
declaringScope e scope m = foldr (uncurry $ declaringName e) m $ M.toList scope

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
          compilerBugS "Cannot put scalar function return in-place yet." -- FIXME
        funcallTarget (ArrayDestination _) =
          return []
        funcallTarget (MemoryDestination name) =
          return [name]

subExpToDimSize :: SubExp -> ImpM lore op Imp.DimSize
subExpToDimSize (Var v) =
  return $ Imp.VarSize v
subExpToDimSize (Constant (IntValue (Int64Value i))) =
  return $ Imp.ConstSize $ fromIntegral i
subExpToDimSize (Constant (IntValue (Int32Value i))) =
  return $ Imp.ConstSize $ fromIntegral i
subExpToDimSize Constant{} =
  compilerBugS "Size subexp is not an int32 or int64 constant."

compileSubExpTo :: ValueDestination -> SubExp -> ImpM lore op ()
compileSubExpTo dest se = copyDWIMDest dest [] se []

compileSubExp :: SubExp -> ImpM lore op Imp.Exp
compileSubExp (Constant v) =
  return $ Imp.ValueExp v
compileSubExp (Var v) = do
  t <- lookupType v
  case t of
    Prim pt -> return $ Imp.var v pt
    _       -> compilerBugS $ "compileSubExp: SubExp is not a primitive type: " ++ pretty v

compileSubExpOfType :: PrimType -> SubExp -> Imp.Exp
compileSubExpOfType _ (Constant v) = Imp.ValueExp v
compileSubExpOfType t (Var v) = Imp.var v t

compilePrimExp :: PrimExp VName -> Imp.Exp
compilePrimExp = fmap Imp.ScalarVar

varIndex :: VName -> Imp.Exp
varIndex name = LeafExp (Imp.ScalarVar name) int32

constIndex :: Int -> Imp.Exp
constIndex = fromIntegral

lookupVar :: VName -> ImpM lore op (VarEntry lore)
lookupVar name = do
  res <- asks $ M.lookup name . envVtable
  case res of
    Just entry -> return entry
    _ -> compilerBugS $ "Unknown variable: " ++ pretty name

lookupArray :: VName -> ImpM lore op ArrayEntry
lookupArray name = do
  res <- lookupVar name
  case res of
    ArrayVar _ entry -> return entry
    _                -> compilerBugS $ "ImpGen.lookupArray: not an array: " ++ pretty name

arrayLocation :: VName -> ImpM lore op MemLocation
arrayLocation name = entryArrayLocation <$> lookupArray name

lookupMemory :: VName -> ImpM lore op MemEntry
lookupMemory name = do
  res <- lookupVar name
  case res of
    MemVar _ entry -> return entry
    _              -> compilerBugS $ "Unknown memory block: " ++ pretty name

destinationFromParam :: Param (MemBound u) -> ImpM lore op ValueDestination
destinationFromParam param
  | MemArray _ shape _ (ArrayIn mem ixfun) <- paramAttr param = do
      let dims = shapeDims shape
      memloc <- MemLocation mem <$> mapM subExpToDimSize dims <*>
                pure (fmap compilePrimExp ixfun)
      return $ ArrayDestination $ Just memloc
  | otherwise =
      return $ ScalarDestination $ paramName param

destinationFromParams :: [Param (MemBound u)] -> ImpM lore op Destination
destinationFromParams = fmap Destination . mapM destinationFromParam

destinationFromPattern :: ExplicitMemorish lore => Pattern lore -> ImpM lore op Destination
destinationFromPattern pat = fmap Destination . mapM inspect . patternElements $ pat
  where ctx_names = patternContextNames pat
        inspect patElem = do
          let name = patElemName patElem
          entry <- lookupVar name
          case entry of
            ArrayVar _ (ArrayEntry (MemLocation mem shape ixfun) _) ->
              return $ ArrayDestination $
              if mem `elem` ctx_names
              then Nothing
              else Just $ MemLocation mem shape ixfun
            MemVar{} ->
              return $ MemoryDestination name

            ScalarVar{} ->
              return $ ScalarDestination name

fullyIndexArray :: VName -> [Imp.Exp]
                -> ImpM lore op (VName, Imp.Space, Count Bytes)
fullyIndexArray name indices = do
  arr <- lookupArray name
  fullyIndexArray' (entryArrayLocation arr) indices $ entryArrayElemType arr

fullyIndexArray' :: MemLocation -> [Imp.Exp] -> PrimType
                 -> ImpM lore op (VName, Imp.Space, Count Bytes)
fullyIndexArray' (MemLocation mem _ ixfun) indices bt = do
  space <- entryMemSpace <$> lookupMemory mem
  return (mem, space,
          bytes $ IxFun.index ixfun indices $ primByteSize bt)

readFromArray :: VName -> [Imp.Exp]
              -> ImpM lore op Imp.Exp
readFromArray name indices = do
  arr <- lookupArray name
  (mem, space, i) <-
    fullyIndexArray' (entryArrayLocation arr) indices $ entryArrayElemType arr
  vol <- asks envVolatility
  return $ Imp.index mem i (entryArrayElemType arr) space vol

sliceArray :: MemLocation
           -> Slice Imp.Exp
           -> MemLocation
sliceArray (MemLocation mem shape ixfun) slice =
  MemLocation mem (update shape slice) $ IxFun.slice ixfun slice
  where update (d:ds) (DimSlice{}:is) = d : update ds is
        update (_:ds) (DimFix{}:is) = update ds is
        update _      _               = []

offsetArray :: MemLocation
            -> Imp.Exp
            -> MemLocation
offsetArray (MemLocation mem shape ixfun) offset =
  MemLocation mem shape $ IxFun.offsetIndex ixfun offset

strideArray :: MemLocation
            -> Imp.Exp
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
      (Shape $ map dimSizeToExp destshape) destIxFun,
    ixFunMatchesInnerShape
      (Shape $ map dimSizeToExp srcshape) srcIxFun,
    Just destoffset <-
      IxFun.linearWithOffset destIxFun bt_size,
    Just srcoffset  <-
      IxFun.linearWithOffset srcIxFun bt_size = do
        srcspace <- entryMemSpace <$> lookupMemory srcmem
        destspace <- entryMemSpace <$> lookupMemory destmem
        emit $ Imp.Copy
          destmem (bytes destoffset) destspace
          srcmem (bytes srcoffset) srcspace $
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
        Imp.Write destmem (bytes destidx) bt destspace vol $
        Imp.index srcmem (bytes srcidx) bt srcspace vol
  where bt_size = primByteSize bt

-- | Copy from here to there; both destination and source may be
-- indexeded.
copyArrayDWIM :: PrimType
              -> MemLocation -> [Imp.Exp]
              -> MemLocation -> [Imp.Exp]
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
            sliceArray destlocation $
            fullSliceNum (IxFun.shape dest_ixfun) $ map DimFix destis
          srclocation'  =
            sliceArray srclocation $
            fullSliceNum (IxFun.shape src_ixfun) $ map DimFix srcis
      if destlocation' == srclocation'
        then return mempty -- Copy would be no-op.
        else collect $ copy bt destlocation' srclocation' $
             product $ map Imp.dimSizeToExp $
             take 1 $ drop (length srcis) srcshape

-- | Like 'copyDWIM', but the target is a 'ValueDestination'
-- instead of a variable name.
copyDWIMDest :: ValueDestination -> [Imp.Exp] -> SubExp -> [Imp.Exp]
             -> ImpM lore op ()

copyDWIMDest _ _ (Constant v) (_:_) =
  compilerBugS $
  unwords ["copyDWIMDest: constant source", pretty v, "cannot be indexed."]
copyDWIMDest dest dest_is (Constant v) [] =
  case dest of
  ScalarDestination name ->
    emit $ Imp.SetScalar name $ Imp.ValueExp v
  ArrayElemDestination dest_mem _ dest_space dest_i -> do
    vol <- asks envVolatility
    emit $ Imp.Write dest_mem dest_i bt dest_space vol $ Imp.ValueExp v
  MemoryDestination{} ->
    compilerBugS $
    unwords ["copyDWIMDest: constant source", pretty v, "cannot be written to memory destination."]
  ArrayDestination (Just dest_loc) -> do
    (dest_mem, dest_space, dest_i) <-
      fullyIndexArray' dest_loc dest_is bt
    vol <- asks envVolatility
    emit $ Imp.Write dest_mem dest_i bt dest_space vol $ Imp.ValueExp v
  ArrayDestination Nothing ->
    compilerBugS "copyDWIMDest: ArrayDestination Nothing"
  where bt = primValueType v

copyDWIMDest dest dest_is (Var src) src_is = do
  src_entry <- lookupVar src
  case (dest, src_entry) of
    (MemoryDestination mem, MemVar _ (MemEntry _ space)) ->
      emit $ Imp.SetMem mem src space

    (MemoryDestination{}, _) ->
      compilerBugS $
      unwords ["copyDWIMDest: cannot write", pretty src, "to memory destination."]

    (_, MemVar{}) ->
      compilerBugS $
      unwords ["copyDWIMDest: source", pretty src, "is a memory block."]

    (_, ScalarVar _ (ScalarEntry _)) | not $ null src_is ->
      compilerBugS $
      unwords ["copyDWIMDest: prim-typed source", pretty src, "with nonzero indices."]


    (ScalarDestination name, _) | not $ null dest_is ->
      compilerBugS $
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
      compilerBugS $
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
      compilerBugS $
      unwords ["copyDWIMDest: array element destination, but array source",
               pretty src,
               "with incomplete indexing."]

    (ArrayDestination (Just dest_loc), ArrayVar _ src_arr) -> do
      let src_loc = entryArrayLocation src_arr
          bt = entryArrayElemType src_arr
      emit =<< copyArrayDWIM bt dest_loc dest_is src_loc src_is

    (ArrayDestination (Just dest_loc), ScalarVar _ (ScalarEntry bt)) -> do
      (dest_mem, dest_space, dest_i) <-
        fullyIndexArray' dest_loc dest_is bt
      vol <- asks envVolatility
      emit $ Imp.Write dest_mem dest_i bt dest_space vol (Imp.var src bt)

    (ArrayDestination Nothing, _) ->
      return () -- Nothing to do; something else set some memory
                -- somewhere.

-- | Copy from here to there; both destination and source be
-- indexeded.  If so, they better be arrays of enough dimensions.
-- This function will generally just Do What I Mean, and Do The Right
-- Thing.  Both destination and source must be in scope.
copyDWIM :: VName -> [Imp.Exp] -> SubExp -> [Imp.Exp]
         -> ImpM lore op ()
copyDWIM dest dest_is src src_is = do
  dest_entry <- lookupVar dest
  let dest_target =
        case dest_entry of
          ScalarVar _ _ ->
            ScalarDestination dest

          ArrayVar _ (ArrayEntry (MemLocation mem shape ixfun) _) ->
            ArrayDestination $ Just $ MemLocation mem shape ixfun

          MemVar _ _ ->
            MemoryDestination dest
  copyDWIMDest dest_target dest_is src src_is

-- | @compileAlloc dest size space@ allocates @n@ bytes of memory in @space@,
-- writing the result to @dest@, which must be a single
-- 'MemoryDestination',
compileAlloc :: Destination -> SubExp -> Space
             -> ImpM lore op ()
compileAlloc (Destination [MemoryDestination mem]) e space = do
  e' <- compileSubExp e
  emit $ Imp.Allocate mem (Imp.bytes e') space
compileAlloc dest _ _ =
  compilerBugS $ "compileAlloc: Invalid destination: " ++ show dest

dimSizeToSubExp :: Imp.Size -> SubExp
dimSizeToSubExp (Imp.ConstSize n) = constant n
dimSizeToSubExp (Imp.VarSize v) = Var v

dimSizeToExp :: Imp.Size -> Imp.Exp
dimSizeToExp = compilePrimExp . primExpFromSubExp int32 . dimSizeToSubExp
