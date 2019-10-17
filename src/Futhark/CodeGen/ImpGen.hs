{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, LambdaCase, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.CodeGen.ImpGen
  ( -- * Entry Points
    compileProg

    -- * Pluggable Compiler
  , OpCompiler
  , ExpCompiler
  , CopyCompiler
  , StmsCompiler
  , AllocCompiler
  , Operations (..)
  , defaultOperations
  , ValueDestination
  , arrayDestination
  , MemLocation (..)
  , MemEntry (..)
  , ScalarEntry (..)

    -- * Monadic Compiler Interface
  , ImpM
  , Env (envDefaultSpace, envFunction)
  , VTable
  , getVTable
  , localVTable
  , subImpM
  , subImpM_
  , emit
  , emitFunction
  , hasFunction
  , collect
  , collect'
  , comment
  , VarEntry (..)
  , ArrayEntry (..)

    -- * Lookups
  , lookupVar
  , lookupArray
  , lookupMemory

    -- * Building Blocks
  , ToExp(..)
  , compileAlloc
  , subExpToDimSize
  , everythingVolatile
  , compileBody
  , compileBody'
  , compileLoopBody
  , defCompileStms
  , compileStms
  , compileExp
  , defCompileExp
  , offsetArray
  , strideArray
  , fullyIndexArray
  , fullyIndexArray'
  , Imp.dimSizeToExp
  , dimSizeToSubExp
  , copy
  , copyDWIM
  , copyDWIMDest
  , copyElementWise
  , typeSize

  -- * Constructing code.
  , dLParams
  , dFParams
  , dScope
  , dScopes
  , dArray
  , dPrim, dPrim_, dPrimV_, dPrimV, dPrimVE

  , sFor, sWhile
  , sComment
  , sIf, sWhen, sUnless
  , sOp
  , sDeclareMem, sAlloc, sAlloc_
  , sArray, sAllocArray, sAllocArrayPerm, sStaticArray
  , sWrite, sUpdate
  , sLoopNest
  , (<--)
  )
  where

import Control.Monad.RWS    hiding (mapM, forM)
import Control.Monad.State  hiding (mapM, forM, State)
import Control.Monad.Writer hiding (mapM, forM)
import Control.Monad.Except hiding (mapM, forM)
import qualified Control.Monad.Fail as Fail
import Data.Either
import Data.Traversable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.List

import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.CodeGen.ImpCode
  (Bytes, Elements,
   bytes, elements, withElemType)
import Futhark.Representation.ExplicitMemory
import Futhark.Representation.SOACS (SOACS)
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Construct (fullSliceNum)
import Futhark.MonadFreshNames
import Futhark.Error
import Futhark.Util

-- | How to compile an 'Op'.
type OpCompiler lore op = Pattern lore -> Op lore -> ImpM lore op ()

-- | How to compile some 'Stms'.
type StmsCompiler lore op = Names -> Stms lore -> ImpM lore op () -> ImpM lore op ()

-- | How to compile an 'Exp'.
type ExpCompiler lore op = Pattern lore -> Exp lore -> ImpM lore op ()

type CopyCompiler lore op = PrimType
                           -> MemLocation
                           -> MemLocation
                           -> Count Elements Imp.Exp -- ^ Number of row elements of the source.
                           -> ImpM lore op ()

-- | An alternate way of compiling an allocation.
type AllocCompiler lore op = VName -> Count Bytes Imp.Exp -> ImpM lore op ()

data Operations lore op = Operations { opsExpCompiler :: ExpCompiler lore op
                                     , opsOpCompiler :: OpCompiler lore op
                                     , opsStmsCompiler :: StmsCompiler lore op
                                     , opsCopyCompiler :: CopyCompiler lore op
                                     , opsAllocCompilers :: M.Map Space (AllocCompiler lore op)
                                     }

-- | An operations set for which the expression compiler always
-- returns 'CompileExp'.
defaultOperations :: (ExplicitMemorish lore, FreeIn op) =>
                     OpCompiler lore op -> Operations lore op
defaultOperations opc = Operations { opsExpCompiler = defCompileExp
                                   , opsOpCompiler = opc
                                   , opsStmsCompiler = defCompileStms
                                   , opsCopyCompiler = defaultCopy
                                   , opsAllocCompilers = mempty
                                   }

-- | When an array is dared, this is where it is stored.
data MemLocation = MemLocation { memLocationName :: VName
                               , memLocationShape :: [Imp.DimSize]
                               , memLocationIxFun :: IxFun.IxFun Imp.Exp
                               }
                   deriving (Eq, Show)

data ArrayEntry = ArrayEntry {
    entryArrayLocation :: MemLocation
  , entryArrayElemType :: PrimType
  }
  deriving (Show)

entryArrayShape :: ArrayEntry -> [Imp.DimSize]
entryArrayShape = memLocationShape . entryArrayLocation

newtype MemEntry = MemEntry { entryMemSpace :: Imp.Space }
  deriving (Show)

newtype ScalarEntry = ScalarEntry {
    entryScalarType    :: PrimType
  }
  deriving (Show)

-- | Every non-scalar variable must be associated with an entry.
data VarEntry lore = ArrayVar (Maybe (Exp lore)) ArrayEntry
                   | ScalarVar (Maybe (Exp lore)) ScalarEntry
                   | MemVar (Maybe (Exp lore)) MemEntry
                   deriving (Show)

-- | When compiling an expression, this is a description of where the
-- result should end up.  The integer is a reference to the construct
-- that gave rise to this destination (for patterns, this will be the
-- tag of the first name in the pattern).  This can be used to make
-- the generated code easier to relate to the original code.
data Destination = Destination { destinationTag :: Maybe Int
                               , valueDestinations :: [ValueDestination] }
                    deriving (Show)

data ValueDestination = ScalarDestination VName
                      | MemoryDestination VName
                      | ArrayDestination (Maybe MemLocation)
                        -- ^ The 'MemLocation' is 'Just' if a copy if
                        -- required.  If it is 'Nothing', then a
                        -- copy/assignment of a memory block somewhere
                        -- takes care of this array.
                      deriving (Show)

arrayDestination :: MemLocation -> ValueDestination
arrayDestination = ArrayDestination . Just

data Env lore op = Env {
    envExpCompiler :: ExpCompiler lore op
  , envStmsCompiler :: StmsCompiler lore op
  , envOpCompiler :: OpCompiler lore op
  , envCopyCompiler :: CopyCompiler lore op
  , envAllocCompilers :: M.Map Space (AllocCompiler lore op)
  , envDefaultSpace :: Imp.Space
  , envVolatility :: Imp.Volatility
  , envFunction :: Name
    -- ^ Name of the function we are compiling.
  }

newEnv :: Operations lore op -> Imp.Space -> Name -> Env lore op
newEnv ops ds fname =
  Env { envExpCompiler = opsExpCompiler ops
      , envStmsCompiler = opsStmsCompiler ops
      , envOpCompiler = opsOpCompiler ops
      , envCopyCompiler = opsCopyCompiler ops
      , envAllocCompilers = mempty
      , envDefaultSpace = ds
      , envVolatility = Imp.Nonvolatile
      , envFunction = fname
      }

-- | The symbol table used during compilation.
type VTable lore = M.Map VName (VarEntry lore)

data State lore op = State { stateVTable :: VTable lore
                           , stateFunctions :: Imp.Functions op
                           , stateNameSource :: VNameSource
                           }

newState :: VNameSource -> State lore op
newState = State mempty mempty

newtype ImpM lore op a = ImpM (RWST (Env lore op) (Imp.Code op) (State lore op) (Either InternalError) a)
  deriving (Functor, Applicative, Monad,
            MonadState (State lore op),
            MonadReader (Env lore op),
            MonadWriter (Imp.Code op),
            MonadError InternalError)

instance Fail.MonadFail (ImpM lore op) where
  fail = error . ("ImpM.fail: "++)

instance MonadFreshNames (ImpM lore op) where
  getNameSource = gets stateNameSource
  putNameSource src = modify $ \s -> s { stateNameSource = src }

-- Cannot be an ExplicitMemory scope because the index functions have
-- the wrong leaves (VName instead of Imp.Exp).
instance HasScope SOACS (ImpM lore op) where
  askScope = M.map (LetInfo . entryType) <$> gets stateVTable
    where entryType (MemVar _ memEntry) =
            Mem (entryMemSpace memEntry)
          entryType (ArrayVar _ arrayEntry) =
            Array
            (entryArrayElemType arrayEntry)
            (Shape $ map dimSizeToSubExp $ entryArrayShape arrayEntry)
            NoUniqueness
          entryType (ScalarVar _ scalarEntry) =
            Prim $ entryScalarType scalarEntry

runImpM :: ImpM lore op a
        -> Operations lore op -> Imp.Space -> Name -> State lore op
        -> Either InternalError (a, State lore op, Imp.Code op)
runImpM (ImpM m) ops space fname = runRWST m $ newEnv ops space fname

subImpM_ :: Operations lore op' -> ImpM lore op' a
         -> ImpM lore op (Imp.Code op')
subImpM_ ops m = snd <$> subImpM ops m

subImpM :: Operations lore op' -> ImpM lore op' a
        -> ImpM lore op (a, Imp.Code op')
subImpM ops (ImpM m) = do
  env <- ask
  s <- get
  case runRWST m env { envExpCompiler = opsExpCompiler ops
                     , envStmsCompiler = opsStmsCompiler ops
                     , envCopyCompiler = opsCopyCompiler ops
                     , envOpCompiler = opsOpCompiler ops
                     , envAllocCompilers = opsAllocCompilers ops
                     }
                 s { stateVTable = stateVTable s
                   , stateFunctions = mempty } of
    Left err -> throwError err
    Right (x, s', code) -> do
      putNameSource $ stateNameSource s'
      return (x, code)

-- | Execute a code generation action, returning the code that was
-- emitted.
collect :: ImpM lore op () -> ImpM lore op (Imp.Code op)
collect m = pass $ do
  ((), code) <- listen m
  return (code, const mempty)

collect' :: ImpM lore op a -> ImpM lore op (a, Imp.Code op)
collect' m = pass $ do
  (x, code) <- listen m
  return ((x, code), const mempty)

-- | Execute a code generation action, wrapping the generated code
-- within a 'Imp.Comment' with the given description.
comment :: String -> ImpM lore op () -> ImpM lore op ()
comment desc m = do code <- collect m
                    emit $ Imp.Comment desc code

-- | Emit some generated imperative code.
emit :: Imp.Code op -> ImpM lore op ()
emit = tell

-- | Emit a function in the generated code.
emitFunction :: Name -> Imp.Function op -> ImpM lore op ()
emitFunction fname fun = do
  Imp.Functions fs <- gets stateFunctions
  modify $ \s -> s { stateFunctions = Imp.Functions $ (fname,fun) : fs }

-- | Check if a function of a given name exists.
hasFunction :: Name -> ImpM lore op Bool
hasFunction fname = gets $ \s -> let Imp.Functions fs = stateFunctions s
                                 in isJust $ lookup fname fs

compileProg :: (ExplicitMemorish lore, MonadFreshNames m) =>
               Operations lore op -> Imp.Space
            -> Prog lore -> m (Either InternalError (Imp.Functions op))
compileProg ops space prog =
  modifyNameSource $ \src ->
  case foldM compileFunDef' (newState src) (progFunctions prog) of
    Left err -> (Left err, src)
    Right s -> (Right $ stateFunctions s, stateNameSource s)
  where compileFunDef' s fdef = do
          ((), s', _) <-
            runImpM (compileFunDef fdef) ops space (funDefName fdef) s
          return s'

compileInParam :: ExplicitMemorish lore =>
                  FParam lore -> ImpM lore op (Either Imp.Param ArrayDecl)
compileInParam fparam = case paramAttr fparam of
  MemPrim bt ->
    return $ Left $ Imp.ScalarParam name bt
  MemMem space ->
    return $ Left $ Imp.MemParam name space
  MemArray bt shape _ (ArrayIn mem ixfun) -> do
    shape' <- mapM subExpToDimSize $ shapeDims shape
    return $ Right $ ArrayDecl name bt $
      MemLocation mem shape' $ fmap (toExp' int32) ixfun
  where name = paramName fparam

data ArrayDecl = ArrayDecl VName PrimType MemLocation

fparamSizes :: Typed attr => Param attr -> S.Set VName
fparamSizes = S.fromList . subExpVars . arrayDims . paramType

compileInParams :: ExplicitMemorish lore =>
                   [FParam lore] -> [EntryPointType]
                -> ImpM lore op ([Imp.Param], [ArrayDecl], [Imp.ExternalValue])
compileInParams params orig_epts = do
  let (ctx_params, val_params) =
        splitAt (length params - sum (map entryPointSize orig_epts)) params
  (inparams, arrayds) <- partitionEithers <$> mapM compileInParam (ctx_params++val_params)
  let findArray x = find (isArrayDecl x) arrayds
      sizes = mconcat $ map fparamSizes $ ctx_params++val_params

      summaries = M.fromList $ mapMaybe memSummary params
        where memSummary param
                | MemMem space <- paramAttr param =
                    Just (paramName param, space)
                | otherwise =
                    Nothing

      findMemInfo :: VName -> Maybe Space
      findMemInfo = flip M.lookup summaries

      mkValueDesc fparam signedness =
        case (findArray $ paramName fparam, paramType fparam) of
          (Just (ArrayDecl _ bt (MemLocation mem shape _)), _) -> do
            memspace <- findMemInfo mem
            Just $ Imp.ArrayValue mem memspace bt signedness shape
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

  return (inparams, arrayds, mkExts orig_epts val_params)
  where isArrayDecl x (ArrayDecl y _ _) = x == y

compileOutParams :: ExplicitMemorish lore =>
                    [RetType lore] -> [EntryPointType]
                 -> ImpM lore op ([Imp.ExternalValue], [Imp.Param], Destination)
compileOutParams orig_rts orig_epts = do
  ((extvs, dests), (outparams,ctx_dests)) <-
    runWriterT $ evalStateT (mkExts orig_epts orig_rts) (M.empty, M.empty)
  let ctx_dests' = map snd $ sortOn fst $ M.toList ctx_dests
  return (extvs, outparams, Destination Nothing $ ctx_dests' <> dests)
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
          memout <- case attr of
            ReturnsNewBlock _ x _ixfun -> do
              memout <- imp $ newVName "out_mem"
              tell ([Imp.MemParam memout space],
                    M.singleton x $ MemoryDestination memout)
              return memout
            ReturnsInBlock memout _ ->
              return memout
          resultshape <- mapM inspectExtSize $ shapeDims shape
          return (Imp.ArrayValue memout space t ept resultshape,
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

compileFunDef :: ExplicitMemorish lore =>
                 FunDef lore
              -> ImpM lore op ()
compileFunDef (FunDef entry fname rettype params body) = do
  ((outparams, inparams, results, args), body') <- collect' compile
  emitFunction fname $ Imp.Function (isJust entry) outparams inparams body' results args
  where params_entry = maybe (replicate (length params) TypeDirect) fst entry
        ret_entry = maybe (replicate (length rettype) TypeDirect) snd entry
        compile = do
          (inparams, arrayds, args) <- compileInParams params params_entry
          (results, outparams, Destination _ dests) <- compileOutParams rettype ret_entry
          addFParams params
          addArrays arrayds

          let Body _ stms ses = body
          compileStms (freeIn ses) stms $
            forM_ (zip dests ses) $ \(d, se) -> copyDWIMDest d [] se []

          return (outparams, inparams, results, args)

compileBody :: (ExplicitMemorish lore) => Pattern lore -> Body lore -> ImpM lore op ()
compileBody pat (Body _ bnds ses) = do
  Destination _ dests <- destinationFromPattern pat
  compileStms (freeIn ses) bnds $
    forM_ (zip dests ses) $ \(d, se) -> copyDWIMDest d [] se []

compileBody' :: (ExplicitMemorish lore, attr ~ LetAttr lore)
             => [Param attr] -> Body lore -> ImpM lore op ()
compileBody' = compileBody . patternFromParams

compileLoopBody :: Typed attr => [Param attr] -> Body lore -> ImpM lore op ()
compileLoopBody mergeparams (Body _ bnds ses) = do
  -- We cannot write the results to the merge parameters immediately,
  -- as some of the results may actually *be* merge parameters, and
  -- would thus be clobbered.  Therefore, we first copy to new
  -- variables mirroring the merge parameters, and then copy this
  -- buffer to the merge parameters.  This is efficient, because the
  -- operations are all scalar operations.
  tmpnames <- mapM (newVName . (++"_tmp") . baseString . paramName) mergeparams
  compileStms (freeIn ses) bnds $ do
    copy_to_merge_params <- forM (zip3 mergeparams tmpnames ses) $ \(p,tmp,se) ->
      case typeOf p of
        Prim pt  -> do
          emit $ Imp.DeclareScalar tmp pt
          emit $ Imp.SetScalar tmp $ toExp' pt se
          return $ emit $ Imp.SetScalar (paramName p) $ Imp.var tmp pt
        Mem space | Var v <- se -> do
          emit $ Imp.DeclareMem tmp space
          emit $ Imp.SetMem tmp v space
          return $ emit $ Imp.SetMem (paramName p) tmp space
        _ -> return $ return ()
    sequence_ copy_to_merge_params

compileStms :: Names -> Stms lore -> ImpM lore op () -> ImpM lore op ()
compileStms alive_after_stms all_stms m = do
  cb <- asks envStmsCompiler
  cb alive_after_stms all_stms m

defCompileStms :: (ExplicitMemorish lore, FreeIn op) =>
                  Names -> Stms lore -> ImpM lore op () -> ImpM lore op ()
defCompileStms alive_after_stms all_stms m =
  -- We keep track of any memory blocks produced by the statements,
  -- and after the last time that memory block is used, we insert a
  -- Free.  This is very conservative, but can cut down on lifetimes
  -- in some cases.
  void $ compileStms' mempty $ stmsToList all_stms
  where compileStms' allocs (Let pat _ e:bs) = do
          dVars (Just e) (patternElements pat)

          e_code <- collect $ compileExp pat e
          (live_after, bs_code) <- collect' $ compileStms' (patternAllocs pat <> allocs) bs
          let dies_here v = not (v `nameIn` live_after) &&
                            v `nameIn` freeIn e_code
              to_free = S.filter (dies_here . fst) allocs

          emit e_code
          mapM_ (emit . uncurry Imp.Free) to_free
          emit bs_code

          return $ freeIn e_code <> live_after
        compileStms' _ [] = do
          code <- collect m
          emit code
          return $ freeIn code <> alive_after_stms

        patternAllocs = S.fromList . mapMaybe isMemPatElem . patternElements
        isMemPatElem pe = case patElemType pe of
                            Mem space -> Just (patElemName pe, space)
                            _         -> Nothing

compileExp :: Pattern lore -> Exp lore -> ImpM lore op ()
compileExp pat e = do
  ec <- asks envExpCompiler
  ec pat e

defCompileExp :: (ExplicitMemorish lore) =>
                 Pattern lore -> Exp lore -> ImpM lore op ()

defCompileExp pat (If cond tbranch fbranch _) = do
  tcode <- collect $ compileBody pat tbranch
  fcode <- collect $ compileBody pat fbranch
  emit $ Imp.If (toExp' Bool cond) tcode fcode

defCompileExp pat (Apply fname args _ _) = do
  dest <- destinationFromPattern pat
  targets <- funcallTargets dest
  args' <- catMaybes <$> mapM compileArg args
  emit $ Imp.Call targets fname args'
  where compileArg (se, _) = do
          t <- subExpType se
          case (se, t) of
            (_, Prim pt)   -> return $ Just $ Imp.ExpArg $ toExp' pt se
            (Var v, Mem{}) -> return $ Just $ Imp.MemArg v
            _              -> return Nothing

defCompileExp pat (BasicOp op) = defCompileBasicOp pat op

defCompileExp pat (DoLoop ctx val form body) = do
  dFParams mergepat
  forM_ merge $ \(p, se) ->
    when ((==0) $ arrayRank $ paramType p) $
    copyDWIM (paramName p) [] se []

  let doBody = compileLoopBody mergepat body

  case form of
    ForLoop i it bound loopvars -> do
      let setLoopParam (p,a)
            | Prim _ <- paramType p =
                copyDWIM (paramName p) [] (Var a) [Imp.vi32 i]
            | otherwise =
                return ()

      dLParams $ map fst loopvars
      sFor' i it (toExp' (IntType it) bound) $
        mapM_ setLoopParam loopvars >> doBody
    WhileLoop cond ->
      sWhile (Imp.var cond Bool) doBody

  Destination _ pat_dests <- destinationFromPattern pat
  forM_ (zip pat_dests $ map (Var . paramName . fst) merge) $ \(d, r) ->
    copyDWIMDest d [] r []

  where merge = ctx ++ val
        mergepat = map fst merge

defCompileExp pat (Op op) = do
  opc <- asks envOpCompiler
  opc pat op

defCompileBasicOp :: ExplicitMemorish lore =>
                     Pattern lore -> BasicOp lore -> ImpM lore op ()

defCompileBasicOp (Pattern _ [pe]) (SubExp se) =
  copyDWIM (patElemName pe) [] se []

defCompileBasicOp (Pattern _ [pe]) (Opaque se) =
  copyDWIM (patElemName pe) [] se []

defCompileBasicOp (Pattern _ [pe]) (UnOp op e) = do
  e' <- toExp e
  patElemName pe <-- Imp.UnOpExp op e'

defCompileBasicOp (Pattern _ [pe]) (ConvOp conv e) = do
  e' <- toExp e
  patElemName pe <-- Imp.ConvOpExp conv e'

defCompileBasicOp (Pattern _ [pe]) (BinOp bop x y) = do
  x' <- toExp x
  y' <- toExp y
  patElemName pe <-- Imp.BinOpExp bop x' y'

defCompileBasicOp (Pattern _ [pe]) (CmpOp bop x y) = do
  x' <- toExp x
  y' <- toExp y
  patElemName pe <-- Imp.CmpOpExp bop x' y'

defCompileBasicOp _ (Assert e msg loc) = do
  e' <- toExp e
  msg' <- traverse toExp msg
  emit $ Imp.Assert e' msg' loc

defCompileBasicOp (Pattern _ [pe]) (Index src slice)
  | Just idxs <- sliceIndices slice =
      copyDWIM (patElemName pe) [] (Var src) $ map (toExp' int32) idxs

defCompileBasicOp _ Index{} =
  return ()

defCompileBasicOp (Pattern _ [pe]) (Update _ slice se) =
  sUpdate (patElemName pe) (map (fmap (toExp' int32)) slice) se

defCompileBasicOp (Pattern _ [pe]) (Replicate (Shape ds) se) = do
  ds' <- mapM toExp ds
  is <- replicateM (length ds) (newVName "i")
  copy_elem <- collect $ copyDWIM (patElemName pe) (map Imp.vi32 is) se []
  emit $ foldl (.) id (zipWith (`Imp.For` Int32) is ds') copy_elem

defCompileBasicOp _ Scratch{} =
  return ()

defCompileBasicOp (Pattern [] [pe]) (Iota n e s it) = do
  n' <- toExp n
  e' <- toExp e
  s' <- toExp s
  sFor "i" n' $ \i -> do
    let i' = ConvOpExp (SExt Int32 it) i
    x <- dPrimV "x" $ e' + i' * s'
    copyDWIM (patElemName pe) [i] (Var x) []

defCompileBasicOp (Pattern _ [pe]) (Copy src) =
  copyDWIM (patElemName pe) [] (Var src) []

defCompileBasicOp (Pattern _ [pe]) (Manifest _ src) =
  copyDWIM (patElemName pe) [] (Var src) []

defCompileBasicOp (Pattern _ [pe]) (Concat i x ys _) = do
    MemLocation destmem destshape destixfun <-
      entryArrayLocation <$> lookupArray (patElemName pe)
    offs_glb <- dPrim "tmp_offs" int32
    emit $ Imp.SetScalar offs_glb 0
    let perm = [i] ++ [0..i-1] ++ [i+1..length destshape-1]
        invperm = rearrangeInverse perm
        destloc = MemLocation destmem destshape
                  (IxFun.permute (IxFun.offsetIndex (IxFun.permute destixfun perm) $
                                  Imp.vi32 offs_glb)
                   invperm)

    forM_ (x:ys) $ \y -> do
      yentry <- lookupArray y
      let srcloc = entryArrayLocation yentry
          rows = case drop i $ entryArrayShape yentry of
                  []  -> error $ "defCompileBasicOp Concat: empty array shape for " ++ pretty y
                  r:_ -> unCount $ Imp.dimSizeToExp r
      copy (elemType $ patElemType pe) destloc srcloc $ arrayOuterSize yentry
      emit $ Imp.SetScalar offs_glb $ Imp.var offs_glb int32 + rows

defCompileBasicOp (Pattern [] [pe]) (ArrayLit es _)
  | Just vs@(v:_) <- mapM isLiteral es = do
      dest_mem <- entryArrayLocation <$> lookupArray (patElemName pe)
      dest_space <- entryMemSpace <$> lookupMemory (memLocationName dest_mem)
      let t = primValueType v
      static_array <- newVName "static_array"
      emit $ Imp.DeclareArray static_array dest_space t $ Imp.ArrayValues vs
      let static_src = MemLocation static_array [Imp.ConstSize $ fromIntegral $ length es] $
                       IxFun.iota [fromIntegral $ length es]
          entry = MemVar Nothing $ MemEntry dest_space
      addVar static_array entry
      copy t dest_mem static_src $ fromIntegral $ length es
  | otherwise =
    forM_ (zip [0..] es) $ \(i,e) ->
      copyDWIM (patElemName pe) [fromInteger i] e []

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

defCompileBasicOp pat e =
  compilerBugS $ "ImpGen.defCompileBasicOp: Invalid pattern\n  " ++
  pretty pat ++ "\nfor expression\n  " ++ pretty e

-- | Note: a hack to be used only for functions.
addArrays :: [ArrayDecl] -> ImpM lore op ()
addArrays = mapM_ addArray
  where addArray (ArrayDecl name bt location) =
          addVar name $
          ArrayVar Nothing ArrayEntry
          { entryArrayLocation = location
          , entryArrayElemType = bt
          }

-- | Like 'daringFParams', but does not create new declarations.
-- Note: a hack to be used only for functions.
addFParams :: ExplicitMemorish lore => [FParam lore] -> ImpM lore op ()
addFParams = mapM_ addFParam
  where addFParam fparam = do
          entry <- memBoundToVarEntry Nothing $ noUniquenessReturns $ paramAttr fparam
          addVar (paramName fparam) entry

-- | Another hack.
addLoopVar :: VName -> IntType -> ImpM lore op ()
addLoopVar i it = addVar i $ ScalarVar Nothing $ ScalarEntry $ IntType it

dVars :: ExplicitMemorish lore =>
            Maybe (Exp lore) -> [PatElem lore] -> ImpM lore op ()
dVars e = mapM_ dVar
  where dVar = dScope e . scopeOfPatElem

dFParams :: ExplicitMemorish lore => [FParam lore] -> ImpM lore op ()
dFParams = dScope Nothing . scopeOfFParams

dLParams :: ExplicitMemorish lore => [LParam lore] -> ImpM lore op ()
dLParams = dScope Nothing . scopeOfLParams

dPrim_ :: VName -> PrimType -> ImpM lore op ()
dPrim_ name t = do
 emit $ Imp.DeclareScalar name t
 addVar name $ ScalarVar Nothing $ ScalarEntry t

dPrim :: String -> PrimType -> ImpM lore op VName
dPrim name t = do name' <- newVName name
                  dPrim_ name' t
                  return name'

dPrimV_ :: VName -> Imp.Exp -> ImpM lore op ()
dPrimV_ name e = do dPrim_ name $ primExpType e
                    name <-- e

dPrimV :: String -> Imp.Exp -> ImpM lore op VName
dPrimV name e = do name' <- dPrim name $ primExpType e
                   name' <-- e
                   return name'

dPrimVE :: String -> Imp.Exp -> ImpM lore op Imp.Exp
dPrimVE name e = do name' <- dPrim name $ primExpType e
                    name' <-- e
                    return $ Imp.var name' $ primExpType e

memBoundToVarEntry :: Maybe (Exp lore) -> MemBound NoUniqueness
                   -> ImpM lore op (VarEntry lore)
memBoundToVarEntry e (MemPrim bt) =
  return $ ScalarVar e ScalarEntry { entryScalarType = bt }
memBoundToVarEntry e (MemMem space) =
  return $ MemVar e $ MemEntry space
memBoundToVarEntry e (MemArray bt shape _ (ArrayIn mem ixfun)) = do
  shape' <- mapM subExpToDimSize $ shapeDims shape
  let location = MemLocation mem shape' $ fmap (toExp' int32) ixfun
  return $ ArrayVar e ArrayEntry { entryArrayLocation = location
                                 , entryArrayElemType = bt
                                 }

dInfo :: Maybe (Exp lore) -> VName -> NameInfo ExplicitMemory
         -> ImpM lore op ()
dInfo e name info = do
  entry <- memBoundToVarEntry e $ infoAttr info
  case entry of
    MemVar _ entry' ->
      emit $ Imp.DeclareMem name $ entryMemSpace entry'
    ScalarVar _ entry' ->
      emit $ Imp.DeclareScalar name $ entryScalarType entry'
    ArrayVar _ _ ->
      return ()
  addVar name entry
  where infoAttr (LetInfo attr) = attr
        infoAttr (FParamInfo attr) = noUniquenessReturns attr
        infoAttr (LParamInfo attr) = attr
        infoAttr (IndexInfo it) = MemPrim $ IntType it

dScope :: Maybe (Exp lore) -> Scope ExplicitMemory -> ImpM lore op ()
dScope e = mapM_ (uncurry $ dInfo e) . M.toList

dScopes :: [(Maybe (Exp lore), Scope ExplicitMemory)] -> ImpM lore op ()
dScopes = mapM_ $ uncurry dScope

dArray :: VName -> PrimType -> ShapeBase SubExp -> MemBind -> ImpM lore op ()
dArray name bt shape membind = do
  entry <- memBoundToVarEntry Nothing $ MemArray bt shape NoUniqueness membind
  addVar name entry

everythingVolatile :: ImpM lore op a -> ImpM lore op a
everythingVolatile = local $ \env -> env { envVolatility = Imp.Volatile }

-- | Remove the array targets.
funcallTargets :: Destination -> ImpM lore op [VName]
funcallTargets (Destination _ dests) =
  concat <$> mapM funcallTarget dests
  where funcallTarget (ScalarDestination name) =
          return [name]
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

-- | Compile things to 'Imp.Exp'.
class ToExp a where
  -- | Compile to an 'Imp.Exp', where the type (must must still be a
  -- primitive) is deduced monadically.
  toExp :: a -> ImpM lore op Imp.Exp
  -- | Compile where we know the type in advance.
  toExp' :: PrimType -> a -> Imp.Exp

instance ToExp SubExp where
  toExp (Constant v) =
    return $ Imp.ValueExp v
  toExp (Var v) =
    lookupVar v >>= \case
    ScalarVar _ (ScalarEntry pt) ->
      return $ Imp.var v pt
    _       -> compilerBugS $ "toExp SubExp: SubExp is not a primitive type: " ++ pretty v

  toExp' _ (Constant v) = Imp.ValueExp v
  toExp' t (Var v) = Imp.var v t

instance ToExp (PrimExp VName) where
  toExp = pure . fmap Imp.ScalarVar
  toExp' _ = fmap Imp.ScalarVar

addVar :: VName -> VarEntry lore -> ImpM lore op ()
addVar name entry =
  modify $ \s -> s { stateVTable = M.insert name entry $ stateVTable s }

-- | Get the current symbol table.
getVTable :: ImpM lore op (VTable lore)
getVTable = gets stateVTable

putVTable :: VTable lore -> ImpM lore op ()
putVTable vtable = modify $ \s -> s { stateVTable = vtable }

-- | Run an action with a modified symbol table.  All changes to the
-- symbol table will be reverted once the action is done!
localVTable :: (VTable lore -> VTable lore) -> ImpM lore op a -> ImpM lore op a
localVTable f m = do
  old_vtable <- getVTable
  putVTable $ f old_vtable
  a <- m
  putVTable old_vtable
  return a

lookupVar :: VName -> ImpM lore op (VarEntry lore)
lookupVar name = do
  res <- gets $ M.lookup name . stateVTable
  case res of
    Just entry -> return entry
    _ -> compilerBugS $ "Unknown variable: " ++ pretty name

lookupArray :: VName -> ImpM lore op ArrayEntry
lookupArray name = do
  res <- lookupVar name
  case res of
    ArrayVar _ entry -> return entry
    _                -> compilerBugS $ "ImpGen.lookupArray: not an array: " ++ pretty name

lookupMemory :: VName -> ImpM lore op MemEntry
lookupMemory name = do
  res <- lookupVar name
  case res of
    MemVar _ entry -> return entry
    _              -> compilerBugS $ "Unknown memory block: " ++ pretty name

destinationFromPattern :: ExplicitMemorish lore => Pattern lore -> ImpM lore op Destination
destinationFromPattern pat = fmap (Destination (baseTag <$> maybeHead (patternNames pat))) . mapM inspect $
                             patternElements pat
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
                -> ImpM lore op (VName, Imp.Space, Count Elements Imp.Exp)
fullyIndexArray name indices = do
  arr <- lookupArray name
  fullyIndexArray' (entryArrayLocation arr) indices

fullyIndexArray' :: MemLocation -> [Imp.Exp]
                 -> ImpM lore op (VName, Imp.Space, Count Elements Imp.Exp)
fullyIndexArray' (MemLocation mem _ ixfun) indices = do
  space <- entryMemSpace <$> lookupMemory mem
  return (mem, space,
          elements $ IxFun.index ixfun indices)

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

arrayOuterSize :: ArrayEntry -> Count Elements Imp.Exp
arrayOuterSize = arrayDimSize 0

arrayDimSize :: Int -> ArrayEntry -> Count Elements Imp.Exp
arrayDimSize i =
  product . map Imp.dimSizeToExp . take 1 . drop i . entryArrayShape

-- More complicated read/write operations that use index functions.

copy :: CopyCompiler lore op
copy bt pat src n = do
  cc <- asks envCopyCompiler
  cc bt pat src n

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
    let ivars = map Imp.vi32 is
        destidx = IxFun.index destIxFun ivars
        srcidx = IxFun.index srcIxFun ivars
        bounds = map unCount $ n : drop 1 (map Imp.dimSizeToExp srcshape)
    srcspace <- entryMemSpace <$> lookupMemory srcmem
    destspace <- entryMemSpace <$> lookupMemory destmem
    vol <- asks envVolatility
    emit $ foldl (.) id (zipWith (`Imp.For` Int32) is bounds) $
      Imp.Write destmem (elements destidx) bt destspace vol $
      Imp.index srcmem (elements srcidx) bt srcspace vol

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
    fullyIndexArray' destlocation destis
  (srcmem, srcspace, srcoffset) <-
    fullyIndexArray' srclocation srcis
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
          destrank = length (memLocationShape destlocation')
          srcrank = length (memLocationShape srclocation')
      if destrank /= srcrank
        then fail $ "copyArrayDWIM: cannot copy to " ++
             pretty (memLocationName destlocation') ++
             " from " ++ pretty (memLocationName srclocation') ++
             " because ranks do not match (" ++ pretty destrank ++
             " vs " ++ pretty srcrank ++ ")"
      else if destlocation' == srclocation'
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
copyDWIMDest pat dest_is (Constant v) [] =
  case pat of
  ScalarDestination name ->
    emit $ Imp.SetScalar name $ Imp.ValueExp v
  MemoryDestination{} ->
    compilerBugS $
    unwords ["copyDWIMDest: constant source", pretty v, "cannot be written to memory destination."]
  ArrayDestination (Just dest_loc) -> do
    (dest_mem, dest_space, dest_i) <-
      fullyIndexArray' dest_loc dest_is
    vol <- asks envVolatility
    emit $ Imp.Write dest_mem dest_i bt dest_space vol $ Imp.ValueExp v
  ArrayDestination Nothing ->
    compilerBugS "copyDWIMDest: ArrayDestination Nothing"
  where bt = primValueType v

copyDWIMDest dest dest_is (Var src) src_is = do
  src_entry <- lookupVar src
  case (dest, src_entry) of
    (MemoryDestination mem, MemVar _ (MemEntry space)) ->
      emit $ Imp.SetMem mem src space

    (MemoryDestination{}, _) ->
      compilerBugS $
      unwords ["copyDWIMDest: cannot write", pretty src, "to memory destination."]

    (_, MemVar{}) ->
      compilerBugS $
      unwords ["copyDWIMDest: source", pretty src, "is a memory block."]

    (_, ScalarVar _ (ScalarEntry _)) | not $ null src_is ->
      compilerBugS $
      unwords ["copyDWIMDest: prim-typed source", pretty src, "with nonzero indices", pretty src_is]

    (ScalarDestination name, _) | not $ null dest_is ->
      compilerBugS $
      unwords ["copyDWIMDest: prim-typed target", pretty name, "with nonzero indices", pretty dest_is]

    (ScalarDestination name, ScalarVar _ (ScalarEntry pt)) ->
      emit $ Imp.SetScalar name $ Imp.var src pt

    (ScalarDestination name, ArrayVar _ arr) -> do
      let bt = entryArrayElemType arr
      (mem, space, i) <-
        fullyIndexArray' (entryArrayLocation arr) src_is
      vol <- asks envVolatility
      emit $ Imp.SetScalar name $ Imp.index mem i bt space vol

    (ArrayDestination (Just dest_loc), ArrayVar _ src_arr) -> do
      let src_loc = entryArrayLocation src_arr
          bt = entryArrayElemType src_arr
      emit =<< copyArrayDWIM bt dest_loc dest_is src_loc src_is

    (ArrayDestination (Just dest_loc), ScalarVar _ (ScalarEntry bt)) -> do
      (dest_mem, dest_space, dest_i) <- fullyIndexArray' dest_loc dest_is
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

-- | @compileAlloc pat size space@ allocates @n@ bytes of memory in @space@,
-- writing the result to @dest@, which must be a single
-- 'MemoryDestination',
compileAlloc :: ExplicitMemorish lore =>
                Pattern lore -> SubExp -> Space
             -> ImpM lore op ()
compileAlloc (Pattern [] [mem]) e space = do
  e' <- Imp.bytes <$> toExp e
  allocator <- asks $ M.lookup space . envAllocCompilers
  case allocator of
    Nothing -> emit $ Imp.Allocate (patElemName mem) e' space
    Just allocator' -> allocator' (patElemName mem) e'
compileAlloc pat _ _ =
  compilerBugS $ "compileAlloc: Invalid pattern: " ++ pretty pat

dimSizeToSubExp :: Imp.Size -> SubExp
dimSizeToSubExp (Imp.ConstSize n) = constant n
dimSizeToSubExp (Imp.VarSize v) = Var v

dimSizeToExp :: Imp.Size -> Imp.Exp
dimSizeToExp = toExp' int32 . primExpFromSubExp int32 . dimSizeToSubExp

-- | The number of bytes needed to represent the array in a
-- straightforward contiguous format.
typeSize :: Type -> Count Bytes Imp.Exp
typeSize t = Imp.bytes $ Imp.LeafExp (Imp.SizeOf $ elemType t) int32 *
             product (map (toExp' int32) (arrayDims t))

--- Building blocks for constructing code.

sFor' :: VName -> IntType -> Imp.Exp -> ImpM lore op () -> ImpM lore op ()
sFor' i it bound body = do
  addLoopVar i it
  body' <- collect body
  emit $ Imp.For i it bound body'

sFor :: String -> Imp.Exp -> (Imp.Exp -> ImpM lore op ()) -> ImpM lore op ()
sFor i bound body = do
  i' <- newVName i
  it <- case primExpType bound of
          IntType it -> return it
          t -> compilerBugS $ "sFor: bound " ++ pretty bound ++ " is of type " ++ pretty t
  addLoopVar i' it
  body' <- collect $ body $ Imp.var i' $ IntType it
  emit $ Imp.For i' it bound body'

sWhile :: Imp.Exp -> ImpM lore op () -> ImpM lore op ()
sWhile cond body = do
  body' <- collect body
  emit $ Imp.While cond body'

sComment :: String -> ImpM lore op () -> ImpM lore op ()
sComment s code = do
  code' <- collect code
  emit $ Imp.Comment s code'

sIf :: Imp.Exp -> ImpM lore op () -> ImpM lore op () -> ImpM lore op ()
sIf cond tbranch fbranch = do
  tbranch' <- collect tbranch
  fbranch' <- collect fbranch
  emit $ Imp.If cond tbranch' fbranch'

sWhen :: Imp.Exp -> ImpM lore op () -> ImpM lore op ()
sWhen cond tbranch = sIf cond tbranch (return ())

sUnless :: Imp.Exp -> ImpM lore op () -> ImpM lore op ()
sUnless cond = sIf cond (return ())

sOp :: op -> ImpM lore op ()
sOp = emit . Imp.Op

sDeclareMem :: String -> Space -> ImpM lore op VName
sDeclareMem name space = do
  name' <- newVName name
  emit $ Imp.DeclareMem name' space
  addVar name' $ MemVar Nothing $ MemEntry space
  return name'

sAlloc_ :: VName -> Count Bytes Imp.Exp -> Space -> ImpM lore op ()
sAlloc_ name' size' space = do
  allocator <- asks $ M.lookup space . envAllocCompilers
  case allocator of
    Nothing -> emit $ Imp.Allocate name' size' space
    Just allocator' -> allocator' name' size'

sAlloc :: String -> Count Bytes Imp.Exp -> Space -> ImpM lore op VName
sAlloc name size space = do
  name' <- sDeclareMem name space
  sAlloc_ name' size space
  return name'

sArray :: String -> PrimType -> ShapeBase SubExp -> MemBind -> ImpM lore op VName
sArray name bt shape membind = do
  name' <- newVName name
  dArray name' bt shape membind
  return name'

-- | Like 'sAllocArray', but permute the in-memory representation of the indices as specified.
sAllocArrayPerm :: String -> PrimType -> ShapeBase SubExp -> Space -> [Int] -> ImpM lore op VName
sAllocArrayPerm name pt shape space perm = do
  let permuted_dims = rearrangeShape perm $ shapeDims shape
  mem <- sAlloc (name ++ "_mem") (typeSize (Array pt shape NoUniqueness)) space
  let iota_ixfun = IxFun.iota $ map (primExpFromSubExp int32) permuted_dims
  sArray name pt shape $
    ArrayIn mem $ IxFun.permute iota_ixfun $ rearrangeInverse perm

-- | Uses linear/iota index function.
sAllocArray :: String -> PrimType -> ShapeBase SubExp -> Space -> ImpM lore op VName
sAllocArray name pt shape space =
  sAllocArrayPerm name pt shape space [0..shapeRank shape-1]

-- | Uses linear/iota index function.
sStaticArray :: String -> Space -> PrimType -> Imp.ArrayContents -> ImpM lore op VName
sStaticArray name space pt vs = do
  let num_elems = case vs of Imp.ArrayValues vs' -> length vs'
                             Imp.ArrayZeros n -> fromIntegral n
      shape = Shape [intConst Int32 $ toInteger num_elems]
  mem <- newVName $ name ++ "_mem"
  emit $ Imp.DeclareArray mem space pt vs
  addVar mem $ MemVar Nothing $ MemEntry space
  sArray name pt shape $ ArrayIn mem $ IxFun.iota [fromIntegral num_elems]

sWrite :: VName -> [Imp.Exp] -> PrimExp Imp.ExpLeaf -> ImpM lore op ()
sWrite arr is v = do
  (mem, space, offset) <- fullyIndexArray arr is
  vol <- asks envVolatility
  emit $ Imp.Write mem offset (primExpType v) space vol v

sUpdate :: VName -> Slice Imp.Exp -> SubExp -> ImpM lore op ()
sUpdate arr slice v = do
  MemLocation mem shape ixfun <- entryArrayLocation <$> lookupArray arr
  let memdest = sliceArray (MemLocation mem shape ixfun) slice
  copyDWIMDest (ArrayDestination $ Just memdest) [] v []

sLoopNest :: Shape
          -> ([Imp.Exp] -> ImpM lore op ())
          -> ImpM lore op ()
sLoopNest = sLoopNest' [] . shapeDims
  where sLoopNest' is [] f = f $ reverse is
        sLoopNest' is (d:ds) f = do
          d' <- toExp d
          sFor "nest_i" d' $ \i -> sLoopNest' (i:is) ds f

-- | ASsignment.
(<--) :: VName -> Imp.Exp -> ImpM lore op ()
x <-- e = emit $ Imp.SetScalar x e
infixl 3 <--
