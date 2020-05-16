{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, LambdaCase, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Strict #-}
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
  , MemLocation (..)
  , MemEntry (..)
  , ScalarEntry (..)

    -- * Monadic Compiler Interface
  , ImpM
  , localDefaultSpace, askFunction
  , askEnv, localEnv
  , localOps
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
  , everythingVolatile
  , compileBody
  , compileBody'
  , compileLoopBody
  , defCompileStms
  , compileStms
  , compileExp
  , defCompileExp
  , fullyIndexArray
  , fullyIndexArray'
  , copy
  , copyDWIM
  , copyDWIMFix
  , copyElementWise
  , typeSize

  -- * Constructing code.
  , dLParams
  , dFParams
  , dScope
  , dArray
  , dPrim, dPrimVol_, dPrim_, dPrimV_, dPrimV, dPrimVE

  , sFor, sWhile
  , sComment
  , sIf, sWhen, sUnless
  , sOp
  , sDeclareMem, sAlloc, sAlloc_
  , sArray, sArrayInMem, sAllocArray, sAllocArrayPerm, sStaticArray
  , sWrite, sUpdate
  , sLoopNest
  , (<--)

  , function
  )
  where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor (first)
import qualified Data.DList as DL
import Data.Either
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.List (find, sortOn, genericLength)

import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.CodeGen.ImpCode
  (Count, Bytes, Elements,
   bytes, elements, withElemType)
import Futhark.Representation.Mem
import Futhark.Representation.SOACS (SOACS)
import qualified Futhark.Representation.Mem.IxFun as IxFun
import Futhark.Construct (fullSliceNum)
import Futhark.MonadFreshNames
import Futhark.Util

-- | How to compile an 'Op'.
type OpCompiler lore r op = Pattern lore -> Op lore -> ImpM lore r op ()

-- | How to compile some 'Stms'.
type StmsCompiler lore r op = Names -> Stms lore -> ImpM lore r op () -> ImpM lore r op ()

-- | How to compile an 'Exp'.
type ExpCompiler lore r op = Pattern lore -> Exp lore -> ImpM lore r op ()

type CopyCompiler lore r op = PrimType
                           -> MemLocation -> Slice Imp.Exp
                           -> MemLocation -> Slice Imp.Exp
                           -> ImpM lore r op ()

-- | An alternate way of compiling an allocation.
type AllocCompiler lore r op = VName -> Count Bytes Imp.Exp -> ImpM lore r op ()

data Operations lore r op = Operations { opsExpCompiler :: ExpCompiler lore r op
                                     , opsOpCompiler :: OpCompiler lore r op
                                     , opsStmsCompiler :: StmsCompiler lore r op
                                     , opsCopyCompiler :: CopyCompiler lore r op
                                     , opsAllocCompilers :: M.Map Space (AllocCompiler lore r op)
                                     }

-- | An operations set for which the expression compiler always
-- returns 'defCompileExp'.
defaultOperations :: (Mem lore, FreeIn op) =>
                     OpCompiler lore r op -> Operations lore r op
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

data Env lore r op = Env {
    envExpCompiler :: ExpCompiler lore r op
  , envStmsCompiler :: StmsCompiler lore r op
  , envOpCompiler :: OpCompiler lore r op
  , envCopyCompiler :: CopyCompiler lore r op
  , envAllocCompilers :: M.Map Space (AllocCompiler lore r op)
  , envDefaultSpace :: Imp.Space
  , envVolatility :: Imp.Volatility
  , envEnv :: r
    -- ^ User-extensible environment.
  , envFunction :: Maybe Name
    -- ^ Name of the function we are compiling, if any.
  }

newEnv :: r -> Operations lore r op -> Imp.Space -> Env lore r op
newEnv r ops ds =
  Env { envExpCompiler = opsExpCompiler ops
      , envStmsCompiler = opsStmsCompiler ops
      , envOpCompiler = opsOpCompiler ops
      , envCopyCompiler = opsCopyCompiler ops
      , envAllocCompilers = mempty
      , envDefaultSpace = ds
      , envVolatility = Imp.Nonvolatile
      , envEnv = r
      , envFunction = Nothing
      }

-- | The symbol table used during compilation.
type VTable lore = M.Map VName (VarEntry lore)

data ImpState lore r op =
  ImpState { stateVTable :: VTable lore
           , stateFunctions :: Imp.Functions op
           , stateCode :: Imp.Code op
           , stateNameSource :: VNameSource
           }

newState :: VNameSource -> ImpState lore r op
newState = ImpState mempty mempty mempty

newtype ImpM lore r op a =
  ImpM (ReaderT (Env lore r op) (State (ImpState lore r op)) a)
  deriving (Functor, Applicative, Monad,
            MonadState (ImpState lore r op),
            MonadReader (Env lore r op))

instance MonadFreshNames (ImpM lore r op) where
  getNameSource = gets stateNameSource
  putNameSource src = modify $ \s -> s { stateNameSource = src }

-- Cannot be an KernelsMem scope because the index functions have
-- the wrong leaves (VName instead of Imp.Exp).
instance HasScope SOACS (ImpM lore r op) where
  askScope = gets $ M.map (LetInfo . entryType) . stateVTable
    where entryType (MemVar _ memEntry) =
            Mem (entryMemSpace memEntry)
          entryType (ArrayVar _ arrayEntry) =
            Array
            (entryArrayElemType arrayEntry)
            (Shape $ entryArrayShape arrayEntry)
            NoUniqueness
          entryType (ScalarVar _ scalarEntry) =
            Prim $ entryScalarType scalarEntry

runImpM :: ImpM lore r op a
        -> r -> Operations lore r op -> Imp.Space -> ImpState lore r op
        -> (a, ImpState lore r op)
runImpM (ImpM m) r ops space = runState (runReaderT m $ newEnv r ops space)

subImpM_ :: r' -> Operations lore r' op' -> ImpM lore r' op' a
         -> ImpM lore r op (Imp.Code op')
subImpM_ r ops m = snd <$> subImpM r ops m

subImpM :: r' -> Operations lore r' op' -> ImpM lore r' op' a
        -> ImpM lore r op (a, Imp.Code op')
subImpM r ops (ImpM m) = do
  env <- ask
  s <- get

  let env' = env { envExpCompiler = opsExpCompiler ops
                 , envStmsCompiler = opsStmsCompiler ops
                 , envCopyCompiler = opsCopyCompiler ops
                 , envOpCompiler = opsOpCompiler ops
                 , envAllocCompilers = opsAllocCompilers ops
                 , envEnv = r
                 }
      s' = ImpState { stateVTable = stateVTable s
                    , stateFunctions = mempty
                    , stateCode = mempty
                    , stateNameSource = stateNameSource s
                    }
      (x, s'') = runState (runReaderT m env') s'

  putNameSource $ stateNameSource s''
  return (x, stateCode s'')

-- | Execute a code generation action, returning the code that was
-- emitted.
collect :: ImpM lore r op () -> ImpM lore r op (Imp.Code op)
collect = fmap snd . collect'

collect' :: ImpM lore r op a -> ImpM lore r op (a, Imp.Code op)
collect' m = do
  prev_code <- gets stateCode
  modify $ \s -> s { stateCode = mempty }
  x <- m
  new_code <- gets stateCode
  modify $ \s -> s { stateCode = prev_code }
  return (x, new_code)

-- | Execute a code generation action, wrapping the generated code
-- within a 'Imp.Comment' with the given description.
comment :: String -> ImpM lore r op () -> ImpM lore r op ()
comment desc m = do code <- collect m
                    emit $ Imp.Comment desc code

-- | Emit some generated imperative code.
emit :: Imp.Code op -> ImpM lore r op ()
emit code = modify $ \s -> s { stateCode = stateCode s <> code }

-- | Emit a function in the generated code.
emitFunction :: Name -> Imp.Function op -> ImpM lore r op ()
emitFunction fname fun = do
  Imp.Functions fs <- gets stateFunctions
  modify $ \s -> s { stateFunctions = Imp.Functions $ (fname,fun) : fs }

-- | Check if a function of a given name exists.
hasFunction :: Name -> ImpM lore r op Bool
hasFunction fname = gets $ \s -> let Imp.Functions fs = stateFunctions s
                                 in isJust $ lookup fname fs

constsVTable :: Mem lore => Stms lore -> VTable lore
constsVTable = foldMap stmVtable
  where stmVtable (Let pat _ e) =
          foldMap (peVtable e) $ patternElements pat
        peVtable e (PatElem name attr) =
          M.singleton name $ memBoundToVarEntry (Just e) attr

compileProg :: (Mem lore, FreeIn op, MonadFreshNames m) =>
               r -> Operations lore r op -> Imp.Space
            -> Prog lore -> m (Imp.Definitions op)
compileProg r ops space (Prog consts funs) =
  modifyNameSource $ \src ->
  let (consts', s') =
        runImpM compile r ops space
        (newState src) { stateVTable = constsVTable consts }
  in (Imp.Definitions consts' (stateFunctions s'),
      stateNameSource s')
  where compile = do
          mapM_ compileFunDef' funs
          free_in_funs <- gets (freeIn . stateFunctions)
          compileConsts free_in_funs consts

        compileFunDef' fdef =
          local (\env -> env { envFunction = Just $ funDefName fdef }) $
          compileFunDef fdef

compileConsts :: Names -> Stms lore -> ImpM lore r op (Imp.Constants op)
compileConsts used_consts stms = do
  code <- collect $ compileStms used_consts stms $ pure ()
  pure $ uncurry Imp.Constants $ first DL.toList $ extract code
  where -- Fish out those top-level declarations in the constant
        -- initialisation code that are free in the functions.
        extract (x Imp.:>>: y) =
          extract x <> extract y
        extract (Imp.DeclareMem name space)
          | name `nameIn` used_consts =
              (DL.singleton $ Imp.MemParam name space,
               mempty)
        extract (Imp.DeclareScalar name _ t)
          | name `nameIn` used_consts =
              (DL.singleton $ Imp.ScalarParam name t,
               mempty)
        extract s =
          (mempty, s)

compileInParam :: Mem lore =>
                  FParam lore -> ImpM lore r op (Either Imp.Param ArrayDecl)
compileInParam fparam = case paramAttr fparam of
  MemPrim bt ->
    return $ Left $ Imp.ScalarParam name bt
  MemMem space ->
    return $ Left $ Imp.MemParam name space
  MemArray bt shape _ (ArrayIn mem ixfun) ->
    return $ Right $ ArrayDecl name bt $
    MemLocation mem (shapeDims shape) $ fmap (toExp' int32) ixfun
  where name = paramName fparam

data ArrayDecl = ArrayDecl VName PrimType MemLocation

fparamSizes :: Typed attr => Param attr -> S.Set VName
fparamSizes = S.fromList . subExpVars . arrayDims . paramType

compileInParams :: Mem lore =>
                   [FParam lore] -> [EntryPointType]
                -> ImpM lore r op ([Imp.Param], [ArrayDecl], [Imp.ExternalValue])
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

compileOutParams :: Mem lore =>
                    [RetType lore] -> [EntryPointType]
                 -> ImpM lore r op ([Imp.ExternalValue], [Imp.Param], Destination)
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
          error "Functions may not explicitly return memory blocks."
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
              return $ Var out
            Just out ->
              return $ Var out
        inspectExtSize (Free se) =
          return se

compileFunDef :: Mem lore =>
                 FunDef lore
              -> ImpM lore r op ()
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

compileBody :: (Mem lore) => Pattern lore -> Body lore -> ImpM lore r op ()
compileBody pat (Body _ bnds ses) = do
  Destination _ dests <- destinationFromPattern pat
  compileStms (freeIn ses) bnds $
    forM_ (zip dests ses) $ \(d, se) -> copyDWIMDest d [] se []

compileBody' :: [Param attr] -> Body lore -> ImpM lore r op ()
compileBody' params (Body _ bnds ses) =
  compileStms (freeIn ses) bnds $
    forM_ (zip params ses) $ \(param, se) -> copyDWIM (paramName param) [] se []

compileLoopBody :: Typed attr => [Param attr] -> Body lore -> ImpM lore r op ()
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
          emit $ Imp.DeclareScalar tmp Imp.Nonvolatile pt
          emit $ Imp.SetScalar tmp $ toExp' pt se
          return $ emit $ Imp.SetScalar (paramName p) $ Imp.var tmp pt
        Mem space | Var v <- se -> do
          emit $ Imp.DeclareMem tmp space
          emit $ Imp.SetMem tmp v space
          return $ emit $ Imp.SetMem (paramName p) tmp space
        _ -> return $ return ()
    sequence_ copy_to_merge_params

compileStms :: Names -> Stms lore -> ImpM lore r op () -> ImpM lore r op ()
compileStms alive_after_stms all_stms m = do
  cb <- asks envStmsCompiler
  cb alive_after_stms all_stms m

defCompileStms :: (Mem lore, FreeIn op) =>
                  Names -> Stms lore -> ImpM lore r op () -> ImpM lore r op ()
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

compileExp :: Pattern lore -> Exp lore -> ImpM lore r op ()
compileExp pat e = do
  ec <- asks envExpCompiler
  ec pat e

defCompileExp :: (Mem lore) =>
                 Pattern lore -> Exp lore -> ImpM lore r op ()

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
                copyDWIM (paramName p) [] (Var a) [DimFix $ Imp.vi32 i]
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

defCompileBasicOp :: Mem lore =>
                     Pattern lore -> BasicOp -> ImpM lore r op ()

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
      copyDWIM (patElemName pe) [] (Var src) $ map (DimFix . toExp' int32) idxs

defCompileBasicOp _ Index{} =
  return ()

defCompileBasicOp (Pattern _ [pe]) (Update _ slice se) =
  sUpdate (patElemName pe) (map (fmap (toExp' int32)) slice) se

defCompileBasicOp (Pattern _ [pe]) (Replicate (Shape ds) se) = do
  ds' <- mapM toExp ds
  is <- replicateM (length ds) (newVName "i")
  copy_elem <- collect $ copyDWIM (patElemName pe) (map (DimFix . Imp.vi32) is) se []
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
    copyDWIM (patElemName pe) [DimFix i] (Var x) []

defCompileBasicOp (Pattern _ [pe]) (Copy src) =
  copyDWIM (patElemName pe) [] (Var src) []

defCompileBasicOp (Pattern _ [pe]) (Manifest _ src) =
  copyDWIM (patElemName pe) [] (Var src) []

defCompileBasicOp (Pattern _ [pe]) (Concat i x ys _) = do
  offs_glb <- dPrim "tmp_offs" int32
  emit $ Imp.SetScalar offs_glb 0

  forM_ (x:ys) $ \y -> do
    y_dims <- arrayDims <$> lookupType y
    let rows = case drop i y_dims of
                 []  -> error $ "defCompileBasicOp Concat: empty array shape for " ++ pretty y
                 r:_ -> toExp' int32 r
        skip_dims = take i y_dims
        sliceAllDim d = DimSlice 0 d 1
        skip_slices = map (sliceAllDim . toExp' int32) skip_dims
        destslice = skip_slices ++ [DimSlice (Imp.vi32 offs_glb) rows 1]
    copyDWIM (patElemName pe) destslice (Var y) []
    emit $ Imp.SetScalar offs_glb $ Imp.var offs_glb int32 + rows

defCompileBasicOp (Pattern [] [pe]) (ArrayLit es _)
  | Just vs@(v:_) <- mapM isLiteral es = do
      dest_mem <- entryArrayLocation <$> lookupArray (patElemName pe)
      dest_space <- entryMemSpace <$> lookupMemory (memLocationName dest_mem)
      let t = primValueType v
      static_array <- newVName "static_array"
      emit $ Imp.DeclareArray static_array dest_space t $ Imp.ArrayValues vs
      let static_src = MemLocation static_array [intConst Int32 $ fromIntegral $ length es] $
                       IxFun.iota [fromIntegral $ length es]
          entry = MemVar Nothing $ MemEntry dest_space
      addVar static_array entry
      let slice = [DimSlice 0 (genericLength es) 1]
      copy t dest_mem slice static_src slice
  | otherwise =
    forM_ (zip [0..] es) $ \(i,e) ->
      copyDWIM (patElemName pe) [DimFix $ fromInteger i] e []

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
  error $ "ImpGen.defCompileBasicOp: Invalid pattern\n  " ++
  pretty pat ++ "\nfor expression\n  " ++ pretty e

-- | Note: a hack to be used only for functions.
addArrays :: [ArrayDecl] -> ImpM lore r op ()
addArrays = mapM_ addArray
  where addArray (ArrayDecl name bt location) =
          addVar name $
          ArrayVar Nothing ArrayEntry
          { entryArrayLocation = location
          , entryArrayElemType = bt
          }

-- | Like 'dFParams', but does not create new declarations.
-- Note: a hack to be used only for functions.
addFParams :: Mem lore => [FParam lore] -> ImpM lore r op ()
addFParams = mapM_ addFParam
  where addFParam fparam =
          addVar (paramName fparam) $
          memBoundToVarEntry Nothing $ noUniquenessReturns $ paramAttr fparam

-- | Another hack.
addLoopVar :: VName -> IntType -> ImpM lore r op ()
addLoopVar i it = addVar i $ ScalarVar Nothing $ ScalarEntry $ IntType it

dVars :: Mem lore =>
            Maybe (Exp lore) -> [PatElem lore] -> ImpM lore r op ()
dVars e = mapM_ dVar
  where dVar = dScope e . scopeOfPatElem

dFParams :: Mem lore => [FParam lore] -> ImpM lore r op ()
dFParams = dScope Nothing . scopeOfFParams

dLParams :: Mem lore => [LParam lore] -> ImpM lore r op ()
dLParams = dScope Nothing . scopeOfLParams

dPrimVol_ :: VName -> PrimType -> ImpM lore r op ()
dPrimVol_ name t = do
 emit $ Imp.DeclareScalar name Imp.Volatile t
 addVar name $ ScalarVar Nothing $ ScalarEntry t

dPrim_ :: VName -> PrimType -> ImpM lore r op ()
dPrim_ name t = do
 emit $ Imp.DeclareScalar name Imp.Nonvolatile t
 addVar name $ ScalarVar Nothing $ ScalarEntry t

dPrim :: String -> PrimType -> ImpM lore r op VName
dPrim name t = do name' <- newVName name
                  dPrim_ name' t
                  return name'

dPrimV_ :: VName -> Imp.Exp -> ImpM lore r op ()
dPrimV_ name e = do dPrim_ name $ primExpType e
                    name <-- e

dPrimV :: String -> Imp.Exp -> ImpM lore r op VName
dPrimV name e = do name' <- dPrim name $ primExpType e
                   name' <-- e
                   return name'

dPrimVE :: String -> Imp.Exp -> ImpM lore r op Imp.Exp
dPrimVE name e = do name' <- dPrim name $ primExpType e
                    name' <-- e
                    return $ Imp.var name' $ primExpType e

memBoundToVarEntry :: Maybe (Exp lore) -> MemBound NoUniqueness
                   -> VarEntry lore
memBoundToVarEntry e (MemPrim bt) =
  ScalarVar e ScalarEntry { entryScalarType = bt }
memBoundToVarEntry e (MemMem space) =
  MemVar e $ MemEntry space
memBoundToVarEntry e (MemArray bt shape _ (ArrayIn mem ixfun)) =
  let location = MemLocation mem (shapeDims shape) $ fmap (toExp' int32) ixfun
  in ArrayVar e ArrayEntry { entryArrayLocation = location
                           , entryArrayElemType = bt
                           }

infoAttr :: Mem lore =>
            NameInfo lore
         -> MemInfo SubExp NoUniqueness MemBind
infoAttr (LetInfo attr) = attr
infoAttr (FParamInfo attr) = noUniquenessReturns attr
infoAttr (LParamInfo attr) = attr
infoAttr (IndexInfo it) = MemPrim $ IntType it

dInfo :: Mem lore =>
         Maybe (Exp lore) -> VName -> NameInfo lore
      -> ImpM lore r op ()
dInfo e name info = do
  let entry = memBoundToVarEntry e $ infoAttr info
  case entry of
    MemVar _ entry' ->
      emit $ Imp.DeclareMem name $ entryMemSpace entry'
    ScalarVar _ entry' ->
      emit $ Imp.DeclareScalar name Imp.Nonvolatile $ entryScalarType entry'
    ArrayVar _ _ ->
      return ()
  addVar name entry

dScope :: Mem lore =>
          Maybe (Exp lore) -> Scope lore -> ImpM lore r op ()
dScope e = mapM_ (uncurry $ dInfo e) . M.toList

dArray :: VName -> PrimType -> ShapeBase SubExp -> MemBind -> ImpM lore r op ()
dArray name bt shape membind =
  addVar name $
  memBoundToVarEntry Nothing $ MemArray bt shape NoUniqueness membind

everythingVolatile :: ImpM lore r op a -> ImpM lore r op a
everythingVolatile = local $ \env -> env { envVolatility = Imp.Volatile }

-- | Remove the array targets.
funcallTargets :: Destination -> ImpM lore r op [VName]
funcallTargets (Destination _ dests) =
  concat <$> mapM funcallTarget dests
  where funcallTarget (ScalarDestination name) =
          return [name]
        funcallTarget (ArrayDestination _) =
          return []
        funcallTarget (MemoryDestination name) =
          return [name]

-- | Compile things to 'Imp.Exp'.
class ToExp a where
  -- | Compile to an 'Imp.Exp', where the type (must must still be a
  -- primitive) is deduced monadically.
  toExp :: a -> ImpM lore r op Imp.Exp
  -- | Compile where we know the type in advance.
  toExp' :: PrimType -> a -> Imp.Exp

instance ToExp SubExp where
  toExp (Constant v) =
    return $ Imp.ValueExp v
  toExp (Var v) =
    lookupVar v >>= \case
    ScalarVar _ (ScalarEntry pt) ->
      return $ Imp.var v pt
    _       -> error $ "toExp SubExp: SubExp is not a primitive type: " ++ pretty v

  toExp' _ (Constant v) = Imp.ValueExp v
  toExp' t (Var v) = Imp.var v t

instance ToExp (PrimExp VName) where
  toExp = pure . fmap Imp.ScalarVar
  toExp' _ = fmap Imp.ScalarVar

addVar :: VName -> VarEntry lore -> ImpM lore r op ()
addVar name entry =
  modify $ \s -> s { stateVTable = M.insert name entry $ stateVTable s }

localDefaultSpace :: Imp.Space -> ImpM lore r op a -> ImpM lore r op a
localDefaultSpace space = local (\env -> env { envDefaultSpace = space })

askFunction :: ImpM lore r op (Maybe Name)
askFunction = asks envFunction

askEnv :: ImpM lore r op r
askEnv = asks envEnv

localEnv :: (r -> r) -> ImpM lore r op a -> ImpM lore r op a
localEnv f = local $ \env -> env { envEnv = f $ envEnv env }

localOps :: Operations lore r op -> ImpM lore r op a -> ImpM lore r op a
localOps ops = local $ \env ->
                         env { envExpCompiler = opsExpCompiler ops
                             , envStmsCompiler = opsStmsCompiler ops
                             , envCopyCompiler = opsCopyCompiler ops
                             , envOpCompiler = opsOpCompiler ops
                             , envAllocCompilers = opsAllocCompilers ops
                             }

-- | Get the current symbol table.
getVTable :: ImpM lore r op (VTable lore)
getVTable = gets stateVTable

putVTable :: VTable lore -> ImpM lore r op ()
putVTable vtable = modify $ \s -> s { stateVTable = vtable }

-- | Run an action with a modified symbol table.  All changes to the
-- symbol table will be reverted once the action is done!
localVTable :: (VTable lore -> VTable lore) -> ImpM lore r op a -> ImpM lore r op a
localVTable f m = do
  old_vtable <- getVTable
  putVTable $ f old_vtable
  a <- m
  putVTable old_vtable
  return a

lookupVar :: VName -> ImpM lore r op (VarEntry lore)
lookupVar name = do
  res <- gets $ M.lookup name . stateVTable
  case res of
    Just entry -> return entry
    _ -> error $ "Unknown variable: " ++ pretty name

lookupArray :: VName -> ImpM lore r op ArrayEntry
lookupArray name = do
  res <- lookupVar name
  case res of
    ArrayVar _ entry -> return entry
    _                -> error $ "ImpGen.lookupArray: not an array: " ++ pretty name

lookupMemory :: VName -> ImpM lore r op MemEntry
lookupMemory name = do
  res <- lookupVar name
  case res of
    MemVar _ entry -> return entry
    _              -> error $ "Unknown memory block: " ++ pretty name

destinationFromPattern :: Mem lore => Pattern lore -> ImpM lore r op Destination
destinationFromPattern pat =
  fmap (Destination (baseTag <$> maybeHead (patternNames pat))) . mapM inspect $
  patternElements pat
  where inspect patElem = do
          let name = patElemName patElem
          entry <- lookupVar name
          case entry of
            ArrayVar _ (ArrayEntry MemLocation{} _) ->
              return $ ArrayDestination Nothing
            MemVar{} ->
              return $ MemoryDestination name

            ScalarVar{} ->
              return $ ScalarDestination name

fullyIndexArray :: VName -> [Imp.Exp]
                -> ImpM lore r op (VName, Imp.Space, Count Elements Imp.Exp)
fullyIndexArray name indices = do
  arr <- lookupArray name
  fullyIndexArray' (entryArrayLocation arr) indices

fullyIndexArray' :: MemLocation -> [Imp.Exp]
                 -> ImpM lore r op (VName, Imp.Space, Count Elements Imp.Exp)
fullyIndexArray' (MemLocation mem _ ixfun) indices = do
  space <- entryMemSpace <$> lookupMemory mem
  let indices' = case space of
                   ScalarSpace ds _ ->
                     let (zero_is, is) = splitFromEnd (length ds) indices
                     in map (const 0) zero_is ++ is
                   _ -> indices
  return (mem, space,
          elements $ IxFun.index ixfun indices')

-- More complicated read/write operations that use index functions.

copy :: CopyCompiler lore r op
copy bt dest destslice src srcslice = do
  cc <- asks envCopyCompiler
  cc bt dest destslice src srcslice

-- | Use an 'Imp.Copy' if possible, otherwise 'copyElementWise'.
defaultCopy :: CopyCompiler lore r op
defaultCopy bt dest destslice src srcslice
  | Just destoffset <-
      IxFun.linearWithOffset (IxFun.slice destIxFun destslice) bt_size,
    Just srcoffset  <-
      IxFun.linearWithOffset (IxFun.slice srcIxFun srcslice) bt_size = do
        srcspace <- entryMemSpace <$> lookupMemory srcmem
        destspace <- entryMemSpace <$> lookupMemory destmem
        if isScalarSpace srcspace || isScalarSpace destspace
          then copyElementWise bt dest destslice src srcslice
          else emit $ Imp.Copy
               destmem (bytes destoffset) destspace
               srcmem (bytes srcoffset) srcspace $
               num_elems `withElemType` bt
  | otherwise =
      copyElementWise bt dest destslice src srcslice
  where bt_size = primByteSize bt
        num_elems = Imp.elements $ product $ sliceDims srcslice
        MemLocation destmem _ destIxFun = dest
        MemLocation srcmem _ srcIxFun = src
        isScalarSpace ScalarSpace{} = True
        isScalarSpace _ = False

copyElementWise :: CopyCompiler lore r op
copyElementWise bt dest destslice src srcslice = do
    let bounds = sliceDims srcslice
    is <- replicateM (length bounds) (newVName "i")
    let ivars = map Imp.vi32 is
    (destmem, destspace, destidx) <-
      fullyIndexArray' dest $ fixSlice destslice ivars
    (srcmem, srcspace, srcidx) <-
      fullyIndexArray' src $ fixSlice srcslice ivars
    vol <- asks envVolatility
    emit $ foldl (.) id (zipWith (`Imp.For` Int32) is bounds) $
      Imp.Write destmem destidx bt destspace vol $
      Imp.index srcmem srcidx bt srcspace vol

-- | Copy from here to there; both destination and source may be
-- indexeded.
copyArrayDWIM :: PrimType
              -> MemLocation -> [DimIndex Imp.Exp]
              -> MemLocation -> [DimIndex Imp.Exp]
              -> ImpM lore r op (Imp.Code op)
copyArrayDWIM bt
  destlocation@(MemLocation _ destshape _) destslice
  srclocation@(MemLocation _ srcshape _) srcslice

  | Just destis <- mapM dimFix destslice,
    Just srcis <- mapM dimFix srcslice,
    length srcis == length srcshape,
    length destis == length destshape = do
  (targetmem, destspace, targetoffset) <-
    fullyIndexArray' destlocation destis
  (srcmem, srcspace, srcoffset) <-
    fullyIndexArray' srclocation srcis
  vol <- asks envVolatility
  return $ Imp.Write targetmem targetoffset bt destspace vol $
    Imp.index srcmem srcoffset bt srcspace vol

  | otherwise = do
      let destslice' =
            fullSliceNum (map (toExp' int32) destshape) destslice
          srcslice'  =
            fullSliceNum (map (toExp' int32) srcshape) srcslice
          destrank = length $ sliceDims destslice'
          srcrank = length $ sliceDims srcslice'
      if destrank /= srcrank
        then error $ "copyArrayDWIM: cannot copy to " ++
             pretty (memLocationName destlocation) ++
             " from " ++ pretty (memLocationName srclocation) ++
             " because ranks do not match (" ++ pretty destrank ++
             " vs " ++ pretty srcrank ++ ")"
      else if destlocation == srclocation && destslice' == srcslice'
        then return mempty -- Copy would be no-op.
        else collect $ copy bt destlocation destslice' srclocation srcslice'

-- | Like 'copyDWIM', but the target is a 'ValueDestination'
-- instead of a variable name.
copyDWIMDest :: ValueDestination -> [DimIndex Imp.Exp] -> SubExp -> [DimIndex Imp.Exp]
             -> ImpM lore r op ()

copyDWIMDest _ _ (Constant v) (_:_) =
  error $
  unwords ["copyDWIMDest: constant source", pretty v, "cannot be indexed."]
copyDWIMDest pat dest_slice (Constant v) [] =
  case mapM dimFix dest_slice of
    Nothing ->
      error $
      unwords ["copyDWIMDest: constant source", pretty v, "with slice destination."]
    Just dest_is ->
      case pat of
        ScalarDestination name ->
          emit $ Imp.SetScalar name $ Imp.ValueExp v
        MemoryDestination{} ->
          error $
          unwords ["copyDWIMDest: constant source", pretty v, "cannot be written to memory destination."]
        ArrayDestination (Just dest_loc) -> do
          (dest_mem, dest_space, dest_i) <-
            fullyIndexArray' dest_loc dest_is
          vol <- asks envVolatility
          emit $ Imp.Write dest_mem dest_i bt dest_space vol $ Imp.ValueExp v
        ArrayDestination Nothing ->
          error "copyDWIMDest: ArrayDestination Nothing"
  where bt = primValueType v

copyDWIMDest dest dest_slice (Var src) src_slice = do
  src_entry <- lookupVar src
  case (dest, src_entry) of
    (MemoryDestination mem, MemVar _ (MemEntry space)) ->
      emit $ Imp.SetMem mem src space

    (MemoryDestination{}, _) ->
      error $
      unwords ["copyDWIMDest: cannot write", pretty src, "to memory destination."]

    (_, MemVar{}) ->
      error $
      unwords ["copyDWIMDest: source", pretty src, "is a memory block."]

    (_, ScalarVar _ (ScalarEntry _)) | not $ null src_slice ->
      error $
      unwords ["copyDWIMDest: prim-typed source", pretty src, "with slice", pretty src_slice]

    (ScalarDestination name, _) | not $ null dest_slice ->
      error $
      unwords ["copyDWIMDest: prim-typed target", pretty name, "with slice", pretty dest_slice]

    (ScalarDestination name, ScalarVar _ (ScalarEntry pt)) ->
      emit $ Imp.SetScalar name $ Imp.var src pt

    (ScalarDestination name, ArrayVar _ arr)
      | Just src_is <- mapM dimFix src_slice,
        length src_slice == length (entryArrayShape arr) -> do
          let bt = entryArrayElemType arr
          (mem, space, i) <-
            fullyIndexArray' (entryArrayLocation arr) src_is
          vol <- asks envVolatility
          emit $ Imp.SetScalar name $ Imp.index mem i bt space vol
      | otherwise ->
          error $
          unwords ["copyDWIMDest: prim-typed target and array-typed source", pretty src,
                   "with slice", pretty src_slice]

    (ArrayDestination (Just dest_loc), ArrayVar _ src_arr) -> do
      let src_loc = entryArrayLocation src_arr
          bt = entryArrayElemType src_arr
      emit =<< copyArrayDWIM bt dest_loc dest_slice src_loc src_slice

    (ArrayDestination (Just dest_loc), ScalarVar _ (ScalarEntry bt))
      | Just dest_is <- mapM dimFix dest_slice -> do
          (dest_mem, dest_space, dest_i) <- fullyIndexArray' dest_loc dest_is
          vol <- asks envVolatility
          emit $ Imp.Write dest_mem dest_i bt dest_space vol (Imp.var src bt)
      | otherwise ->
          error $
          unwords ["copyDWIMDest: array-typed target and prim-typed source", pretty src,
                   "with slice", pretty dest_slice]

    (ArrayDestination Nothing, _) ->
      return () -- Nothing to do; something else set some memory
                -- somewhere.

-- | Copy from here to there; both destination and source be
-- indexeded.  If so, they better be arrays of enough dimensions.
-- This function will generally just Do What I Mean, and Do The Right
-- Thing.  Both destination and source must be in scope.
copyDWIM :: VName -> [DimIndex Imp.Exp] -> SubExp -> [DimIndex Imp.Exp]
         -> ImpM lore r op ()
copyDWIM dest dest_slice src src_slice = do
  dest_entry <- lookupVar dest
  let dest_target =
        case dest_entry of
          ScalarVar _ _ ->
            ScalarDestination dest

          ArrayVar _ (ArrayEntry (MemLocation mem shape ixfun) _) ->
            ArrayDestination $ Just $ MemLocation mem shape ixfun

          MemVar _ _ ->
            MemoryDestination dest
  copyDWIMDest dest_target dest_slice src src_slice

-- | As 'copyDWIM', but implicitly 'DimFix'es the indexes.
copyDWIMFix :: VName -> [Imp.Exp] -> SubExp -> [Imp.Exp] -> ImpM lore r op ()
copyDWIMFix dest dest_is src src_is =
  copyDWIM dest (map DimFix dest_is) src (map DimFix src_is)

-- | @compileAlloc pat size space@ allocates @n@ bytes of memory in @space@,
-- writing the result to @dest@, which must be a single
-- 'MemoryDestination',
compileAlloc :: Mem lore =>
                Pattern lore -> SubExp -> Space
             -> ImpM lore r op ()
compileAlloc (Pattern [] [mem]) e space = do
  e' <- Imp.bytes <$> toExp e
  allocator <- asks $ M.lookup space . envAllocCompilers
  case allocator of
    Nothing -> emit $ Imp.Allocate (patElemName mem) e' space
    Just allocator' -> allocator' (patElemName mem) e'
compileAlloc pat _ _ =
  error $ "compileAlloc: Invalid pattern: " ++ pretty pat

-- | The number of bytes needed to represent the array in a
-- straightforward contiguous format.
typeSize :: Type -> Count Bytes Imp.Exp
typeSize t = Imp.bytes $ Imp.LeafExp (Imp.SizeOf $ elemType t) int32 *
             product (map (toExp' int32) (arrayDims t))

--- Building blocks for constructing code.

sFor' :: VName -> IntType -> Imp.Exp -> ImpM lore r op () -> ImpM lore r op ()
sFor' i it bound body = do
  addLoopVar i it
  body' <- collect body
  emit $ Imp.For i it bound body'

sFor :: String -> Imp.Exp -> (Imp.Exp -> ImpM lore r op ()) -> ImpM lore r op ()
sFor i bound body = do
  i' <- newVName i
  it <- case primExpType bound of
          IntType it -> return it
          t -> error $ "sFor: bound " ++ pretty bound ++ " is of type " ++ pretty t
  addLoopVar i' it
  body' <- collect $ body $ Imp.var i' $ IntType it
  emit $ Imp.For i' it bound body'

sWhile :: Imp.Exp -> ImpM lore r op () -> ImpM lore r op ()
sWhile cond body = do
  body' <- collect body
  emit $ Imp.While cond body'

sComment :: String -> ImpM lore r op () -> ImpM lore r op ()
sComment s code = do
  code' <- collect code
  emit $ Imp.Comment s code'

sIf :: Imp.Exp -> ImpM lore r op () -> ImpM lore r op () -> ImpM lore r op ()
sIf cond tbranch fbranch = do
  tbranch' <- collect tbranch
  fbranch' <- collect fbranch
  emit $ Imp.If cond tbranch' fbranch'

sWhen :: Imp.Exp -> ImpM lore r op () -> ImpM lore r op ()
sWhen cond tbranch = sIf cond tbranch (return ())

sUnless :: Imp.Exp -> ImpM lore r op () -> ImpM lore r op ()
sUnless cond = sIf cond (return ())

sOp :: op -> ImpM lore r op ()
sOp = emit . Imp.Op

sDeclareMem :: String -> Space -> ImpM lore r op VName
sDeclareMem name space = do
  name' <- newVName name
  emit $ Imp.DeclareMem name' space
  addVar name' $ MemVar Nothing $ MemEntry space
  return name'

sAlloc_ :: VName -> Count Bytes Imp.Exp -> Space -> ImpM lore r op ()
sAlloc_ name' size' space = do
  allocator <- asks $ M.lookup space . envAllocCompilers
  case allocator of
    Nothing -> emit $ Imp.Allocate name' size' space
    Just allocator' -> allocator' name' size'

sAlloc :: String -> Count Bytes Imp.Exp -> Space -> ImpM lore r op VName
sAlloc name size space = do
  name' <- sDeclareMem name space
  sAlloc_ name' size space
  return name'

sArray :: String -> PrimType -> ShapeBase SubExp -> MemBind -> ImpM lore r op VName
sArray name bt shape membind = do
  name' <- newVName name
  dArray name' bt shape membind
  return name'

-- | Declare an array in row-major order in the given memory block.
sArrayInMem :: String -> PrimType -> ShapeBase SubExp -> VName -> ImpM lore r op VName
sArrayInMem name pt shape mem =
  sArray name pt shape $ ArrayIn mem $
  IxFun.iota $ map (primExpFromSubExp int32) $ shapeDims shape

-- | Like 'sAllocArray', but permute the in-memory representation of the indices as specified.
sAllocArrayPerm :: String -> PrimType -> ShapeBase SubExp -> Space -> [Int] -> ImpM lore r op VName
sAllocArrayPerm name pt shape space perm = do
  let permuted_dims = rearrangeShape perm $ shapeDims shape
  mem <- sAlloc (name ++ "_mem") (typeSize (Array pt shape NoUniqueness)) space
  let iota_ixfun = IxFun.iota $ map (primExpFromSubExp int32) permuted_dims
  sArray name pt shape $
    ArrayIn mem $ IxFun.permute iota_ixfun $ rearrangeInverse perm

-- | Uses linear/iota index function.
sAllocArray :: String -> PrimType -> ShapeBase SubExp -> Space -> ImpM lore r op VName
sAllocArray name pt shape space =
  sAllocArrayPerm name pt shape space [0..shapeRank shape-1]

-- | Uses linear/iota index function.
sStaticArray :: String -> Space -> PrimType -> Imp.ArrayContents -> ImpM lore r op VName
sStaticArray name space pt vs = do
  let num_elems = case vs of Imp.ArrayValues vs' -> length vs'
                             Imp.ArrayZeros n -> fromIntegral n
      shape = Shape [intConst Int32 $ toInteger num_elems]
  mem <- newVName $ name ++ "_mem"
  emit $ Imp.DeclareArray mem space pt vs
  addVar mem $ MemVar Nothing $ MemEntry space
  sArray name pt shape $ ArrayIn mem $ IxFun.iota [fromIntegral num_elems]

sWrite :: VName -> [Imp.Exp] -> PrimExp Imp.ExpLeaf -> ImpM lore r op ()
sWrite arr is v = do
  (mem, space, offset) <- fullyIndexArray arr is
  vol <- asks envVolatility
  emit $ Imp.Write mem offset (primExpType v) space vol v

sUpdate :: VName -> Slice Imp.Exp -> SubExp -> ImpM lore r op ()
sUpdate arr slice v = copyDWIM arr slice v []

sLoopNest :: Shape
          -> ([Imp.Exp] -> ImpM lore r op ())
          -> ImpM lore r op ()
sLoopNest = sLoopNest' [] . shapeDims
  where sLoopNest' is [] f = f $ reverse is
        sLoopNest' is (d:ds) f = do
          d' <- toExp d
          sFor "nest_i" d' $ \i -> sLoopNest' (i:is) ds f

-- | ASsignment.
(<--) :: VName -> Imp.Exp -> ImpM lore r op ()
x <-- e = emit $ Imp.SetScalar x e
infixl 3 <--

-- | Constructing a non-entry point function.
function :: [Imp.Param] -> [Imp.Param] -> ImpM lore r op ()
         -> ImpM lore r op (Imp.Function op)
function outputs inputs m = do
  body <- collect $ do
    mapM_ addParam $ outputs ++ inputs
    m
  return $ Imp.Function False outputs inputs body [] []
  where addParam (Imp.MemParam name space) =
          addVar name $ MemVar Nothing $ MemEntry space
        addParam (Imp.ScalarParam name bt) =
          addVar name $ ScalarVar Nothing $ ScalarEntry bt
