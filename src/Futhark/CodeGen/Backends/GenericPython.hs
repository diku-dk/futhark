{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, LambdaCase #-}
{-# LANGUAGE TupleSections #-}
-- | A generic Python code generator which is polymorphic in the type
-- of the operations.  Concretely, we use this to handle both
-- sequential and PyOpenCL Python code.
module Futhark.CodeGen.Backends.GenericPython
  ( compileProg
  , Constructor (..)
  , emptyConstructor

  , compileName
  , compileDim
  , compileExp
  , compileCode
  , compilePrimValue
  , compilePrimType
  , compilePrimTypeExt
  , compilePrimToNp
  , compilePrimToExtNp

  , Operations (..)
  , defaultOperations

  , unpackDim

  , CompilerM (..)
  , OpCompiler
  , WriteScalar
  , ReadScalar
  , Allocate
  , Copy
  , StaticArray
  , EntryOutput
  , EntryInput

  , CompilerEnv(..)
  , CompilerState(..)
  , stm
  , stms
  , atInit
  , collect'
  , collect
  , simpleCall

  , copyMemoryDefaultSpace
  ) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M

import Futhark.Representation.Primitive hiding (Bool)
import Futhark.MonadFreshNames
import Futhark.Representation.AST.Syntax (Space(..))
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.CodeGen.Backends.GenericPython.Options
import Futhark.CodeGen.Backends.GenericPython.Definitions
import Futhark.Util (zEncodeString)
import Futhark.Representation.AST.Attributes (builtInFunctions, isBuiltInFunction)

-- | A substitute expression compiler, tried before the main
-- compilation function.
type OpCompiler op s = op -> CompilerM op s ()

-- | Write a scalar to the given memory block with the given index and
-- in the given memory space.
type WriteScalar op s = VName -> PyExp -> PrimType -> Imp.SpaceId -> PyExp
                        -> CompilerM op s ()

-- | Read a scalar from the given memory block with the given index and
-- in the given memory space.
type ReadScalar op s = VName -> PyExp -> PrimType -> Imp.SpaceId
                       -> CompilerM op s PyExp

-- | Allocate a memory block of the given size in the given memory
-- space, saving a reference in the given variable name.
type Allocate op s = VName -> PyExp -> Imp.SpaceId
                     -> CompilerM op s ()

-- | Copy from one memory block to another.
type Copy op s = VName -> PyExp -> Imp.Space ->
                 VName -> PyExp -> Imp.Space ->
                 PyExp -> PrimType ->
                 CompilerM op s ()

-- | Create a static array of values - initialised at load time.
type StaticArray op s = VName -> Imp.SpaceId -> PrimType -> Imp.ArrayContents -> CompilerM op s ()

-- | Construct the Python array being returned from an entry point.
type EntryOutput op s = VName -> Imp.SpaceId ->
                        PrimType -> Imp.Signedness ->
                        [Imp.DimSize] ->
                        CompilerM op s PyExp

-- | Unpack the array being passed to an entry point.
type EntryInput op s = VName -> Imp.SpaceId ->
                       PrimType -> Imp.Signedness ->
                       [Imp.DimSize] ->
                       PyExp ->
                       CompilerM op s ()


data Operations op s = Operations { opsWriteScalar :: WriteScalar op s
                                  , opsReadScalar :: ReadScalar op s
                                  , opsAllocate :: Allocate op s
                                  , opsCopy :: Copy op s
                                  , opsStaticArray :: StaticArray op s
                                  , opsCompiler :: OpCompiler op s
                                  , opsEntryOutput :: EntryOutput op s
                                  , opsEntryInput :: EntryInput op s
                                  }

-- | A set of operations that fail for every operation involving
-- non-default memory spaces.  Uses plain pointers and @malloc@ for
-- memory management.
defaultOperations :: Operations op s
defaultOperations = Operations { opsWriteScalar = defWriteScalar
                               , opsReadScalar = defReadScalar
                               , opsAllocate  = defAllocate
                               , opsCopy = defCopy
                               , opsStaticArray = defStaticArray
                               , opsCompiler = defCompiler
                               , opsEntryOutput = defEntryOutput
                               , opsEntryInput = defEntryInput
                               }
  where defWriteScalar _ _ _ _ _ =
          error "Cannot write to non-default memory space because I am dumb"
        defReadScalar _ _ _ _ =
          error "Cannot read from non-default memory space"
        defAllocate _ _ _ =
          error "Cannot allocate in non-default memory space"
        defCopy _ _ _ _ _ _ _ _ =
          error "Cannot copy to or from non-default memory space"
        defStaticArray _ _ _ _ =
          error "Cannot create static array in non-default memory space"
        defCompiler _ =
          error "The default compiler cannot compile extended operations"
        defEntryOutput _ _ _ _ =
          error "Cannot return array not in default memory space"
        defEntryInput _ _ _ _ =
          error "Cannot accept array not in default memory space"

data CompilerEnv op s = CompilerEnv {
    envOperations :: Operations op s
  , envFtable     :: M.Map Name [Imp.Type]
}

envOpCompiler :: CompilerEnv op s -> OpCompiler op s
envOpCompiler = opsCompiler . envOperations

envReadScalar :: CompilerEnv op s -> ReadScalar op s
envReadScalar = opsReadScalar . envOperations

envWriteScalar :: CompilerEnv op s -> WriteScalar op s
envWriteScalar = opsWriteScalar . envOperations

envAllocate :: CompilerEnv op s -> Allocate op s
envAllocate = opsAllocate . envOperations

envCopy :: CompilerEnv op s -> Copy op s
envCopy = opsCopy . envOperations

envStaticArray :: CompilerEnv op s -> StaticArray op s
envStaticArray = opsStaticArray . envOperations

envEntryOutput :: CompilerEnv op s -> EntryOutput op s
envEntryOutput = opsEntryOutput . envOperations

envEntryInput :: CompilerEnv op s -> EntryInput op s
envEntryInput = opsEntryInput . envOperations

newCompilerEnv :: Imp.Functions op -> Operations op s -> CompilerEnv op s
newCompilerEnv (Imp.Functions funs) ops =
  CompilerEnv { envOperations = ops
              , envFtable = ftable <> builtinFtable
              }
  where ftable = M.fromList $ map funReturn funs
        funReturn (name, Imp.Function _ outparams _ _ _ _) = (name, paramsTypes outparams)
        builtinFtable = M.map (map Imp.Scalar . snd) builtInFunctions

data CompilerState s = CompilerState {
    compNameSrc :: VNameSource
  , compInit :: [PyStmt]
  , compUserState :: s
}

newCompilerState :: VNameSource -> s -> CompilerState s
newCompilerState src s = CompilerState { compNameSrc = src
                                       , compInit = []
                                       , compUserState = s }

newtype CompilerM op s a = CompilerM (RWS (CompilerEnv op s) [PyStmt] (CompilerState s) a)
  deriving (Functor, Applicative, Monad,
            MonadState (CompilerState s),
            MonadReader (CompilerEnv op s),
            MonadWriter [PyStmt])

instance MonadFreshNames (CompilerM op s) where
  getNameSource = gets compNameSrc
  putNameSource src = modify $ \s -> s { compNameSrc = src }

collect :: CompilerM op s () -> CompilerM op s [PyStmt]
collect m = pass $ do
  ((), w) <- listen m
  return (w, const mempty)

collect' :: CompilerM op s a -> CompilerM op s (a, [PyStmt])
collect' m = pass $ do
  (x, w) <- listen m
  return ((x, w), const mempty)

atInit :: PyStmt -> CompilerM op s ()
atInit x = modify $ \s ->
  s { compInit = compInit s ++ [x] }

stm :: PyStmt -> CompilerM op s ()
stm x = tell [x]

stms :: [PyStmt] -> CompilerM op s ()
stms = mapM_ stm

futharkFun :: String -> String
futharkFun s = "futhark_" ++ zEncodeString s

paramsTypes :: [Imp.Param] -> [Imp.Type]
paramsTypes = map paramType
  where paramType (Imp.MemParam _ space) = Imp.Mem (Imp.ConstSize 0) space
        paramType (Imp.ScalarParam _ t) = Imp.Scalar t

compileOutput :: [Imp.Param] -> [PyExp]
compileOutput = map (Var . compileName . Imp.paramName)

runCompilerM :: Imp.Functions op -> Operations op s
             -> VNameSource
             -> s
             -> CompilerM op s a
             -> a
runCompilerM prog ops src userstate (CompilerM m) =
  fst $ evalRWS m (newCompilerEnv prog ops) (newCompilerState src userstate)

standardOptions :: [Option]
standardOptions = [
  Option { optionLongName = "write-runtime-to"
         , optionShortName = Just 't'
         , optionArgument = RequiredArgument "str"
         , optionAction =
           [
             If (Var "runtime_file")
             [Exp $ simpleCall "runtime_file.close" []] []
           , Assign (Var "runtime_file") $
             simpleCall "open" [Var "optarg", String "w"]
           ]
         },
  Option { optionLongName = "runs"
         , optionShortName = Just 'r'
         , optionArgument = RequiredArgument "str"
         , optionAction =
           [ Assign (Var "num_runs") $ Var "optarg"
           , Assign (Var "do_warmup_run") $ Bool True
           ]
         },
  Option { optionLongName = "entry-point"
         , optionShortName = Just 'e'
         , optionArgument = RequiredArgument "str"
         , optionAction =
           [ Assign (Var "entry_point") $ Var "optarg" ]
         },
  Option { optionLongName = "binary-output"
         , optionShortName = Just 'b'
         , optionArgument = NoArgument
         , optionAction = [Assign (Var "binary_output") $ Bool True]
         },
  Option { optionLongName = "tuning"
         , optionShortName = Nothing
         , optionArgument = RequiredArgument "open"
         , optionAction = [Exp $ simpleCall "read_tuning_file" [Var "sizes", Var "optarg"]]
         }
  ]


-- | The class generated by the code generator must have a
-- constructor, although it can be vacuous.
data Constructor = Constructor [String] [PyStmt]

-- | A constructor that takes no arguments and does nothing.
emptyConstructor :: Constructor
emptyConstructor = Constructor ["self"] [Pass]

constructorToFunDef :: Constructor -> [PyStmt] -> PyFunDef
constructorToFunDef (Constructor params body) at_init =
  Def "__init__" params $ body <> at_init

compileProg :: MonadFreshNames m =>
               Maybe String
            -> Constructor
            -> [PyStmt]
            -> [PyStmt]
            -> Operations op s
            -> s
            -> [PyStmt]
            -> [Option]
            -> Imp.Functions op
            -> m String
compileProg module_name constructor imports defines ops userstate pre_timing options prog@(Imp.Functions funs) = do
  src <- getNameSource
  let prog' = runCompilerM prog ops src userstate compileProg'
      maybe_shebang =
        case module_name of Nothing -> "#!/usr/bin/env python\n"
                            Just _  -> ""
  return $ maybe_shebang ++
    pretty (PyProg $ imports ++
            [ Import "argparse" Nothing
            , Assign (Var "sizes") $ Dict []
            ] ++
            defines ++
            [Escape pyUtility] ++
            prog')
  where compileProg' = do
          definitions <- mapM compileFunc funs
          at_inits <- gets compInit

          let constructor' = constructorToFunDef constructor at_inits

          case module_name of
            Just name -> do
              (entry_points, entry_point_types) <-
                unzip <$> mapM compileEntryFun (filter (Imp.functionEntry . snd) funs)
              return [ClassDef $ Class name $
                       Assign (Var "entry_points") (Dict entry_point_types) :
                       map FunDef (constructor' : definitions ++ entry_points)]
            Nothing -> do
              let classinst = Assign (Var "self") $ simpleCall "internal" []
              (entry_point_defs, entry_point_names, entry_points) <-
                unzip3 <$> mapM (callEntryFun pre_timing)
                (filter (Imp.functionEntry . snd) funs)
              return (parse_options ++
                      ClassDef (Class "internal" $ map FunDef $
                                constructor' : definitions) :
                      classinst :
                      map FunDef entry_point_defs ++
                      selectEntryPoint entry_point_names entry_points)

        parse_options =
          Assign (Var "runtime_file") None :
          Assign (Var "do_warmup_run") (Bool False) :
          Assign (Var "num_runs") (Integer 1) :
          Assign (Var "entry_point") (String "main") :
          Assign (Var "binary_output") (Bool False) :
          generateOptionParser (standardOptions ++ options)

        selectEntryPoint entry_point_names entry_points =
          [ Assign (Var "entry_points") $
              Dict $ zip (map String entry_point_names) entry_points,
            Assign (Var "entry_point_fun") $
              simpleCall "entry_points.get" [Var "entry_point"],
            If (BinOp "==" (Var "entry_point_fun") None)
              [Exp $ simpleCall "sys.exit"
                  [Call (Field
                          (String "No entry point '{}'.  Select another with --entry point.  Options are:\n{}")
                          "format")
                    [Arg $ Var "entry_point",

                     Arg $ Call (Field (String "\n") "join")
                     [Arg $ simpleCall "entry_points.keys" []]]]]
              [Exp $ simpleCall "entry_point_fun" []]
          ]

compileFunc :: (Name, Imp.Function op) -> CompilerM op s PyFunDef
compileFunc (fname, Imp.Function _ outputs inputs body _ _) = do
  body' <- collect $ compileCode body
  let inputs' = map (compileName . Imp.paramName) inputs
  let ret = Return $ tupleOrSingle $ compileOutput outputs
  return $ Def (futharkFun . nameToString $ fname) ("self" : inputs') (body'++[ret])

tupleOrSingle :: [PyExp] -> PyExp
tupleOrSingle [e] = e
tupleOrSingle es = Tuple es

-- | A 'Call' where the function is a variable and every argument is a
-- simple 'Arg'.
simpleCall :: String -> [PyExp] -> PyExp
simpleCall fname = Call (Var fname) . map Arg

compileName :: VName -> String
compileName = zEncodeString . pretty

compileDim :: Imp.DimSize -> PyExp
compileDim (Imp.ConstSize i) = Integer $ toInteger i
compileDim (Imp.VarSize v) = Var $ compileName v

unpackDim :: PyExp -> Imp.DimSize -> Int32 -> CompilerM op s ()
unpackDim arr_name (Imp.ConstSize c) i = do
  let shape_name = Field arr_name "shape"
  let constant_c = Integer $ toInteger c
  let constant_i = Integer $ toInteger i
  stm $ Assert (BinOp "==" constant_c (Index shape_name $ IdxExp constant_i)) $
    String "constant dimension wrong"

unpackDim arr_name (Imp.VarSize var) i = do
  let shape_name = Field arr_name "shape"
      src = Index shape_name $ IdxExp $ Integer $ toInteger i
  stm $ Assign (Var $ compileName var) $ simpleCall "np.int32" [src]

entryPointOutput :: Imp.ExternalValue -> CompilerM op s PyExp
entryPointOutput (Imp.OpaqueValue desc vs) =
  simpleCall "opaque" . (String (pretty desc):) <$>
  mapM (entryPointOutput . Imp.TransparentValue) vs
entryPointOutput (Imp.TransparentValue (Imp.ScalarValue bt ept name)) =
  return $ simpleCall tf [Var $ compileName name]
  where tf = compilePrimToExtNp bt ept
entryPointOutput (Imp.TransparentValue (Imp.ArrayValue mem Imp.DefaultSpace bt ept dims)) = do
  let cast = Cast (Var $ compileName mem) (compilePrimTypeExt bt ept)
  return $ simpleCall "createArray" [cast, Tuple $ map compileDim dims]
entryPointOutput (Imp.TransparentValue (Imp.ArrayValue mem (Imp.Space sid) bt ept dims)) = do
  pack_output <- asks envEntryOutput
  pack_output mem sid bt ept dims

badInput :: Int -> PyExp -> String -> PyStmt
badInput i e t =
  Raise $ simpleCall "TypeError"
  [Call (Field (String err_msg) "format")
   [Arg (String t), Arg $ simpleCall "type" [e], Arg e]]
  where err_msg = unlines [ "Argument #" ++ show i ++ " has invalid value"
                          , "Futhark type: {}"
                          , "Argument has Python type {} and value: {}"]


entryPointInput :: (Int, Imp.ExternalValue, PyExp) -> CompilerM op s ()
entryPointInput (i, Imp.OpaqueValue desc vs, e) = do
  let type_is_ok = BinOp "and" (simpleCall "isinstance" [e, Var "opaque"])
                               (BinOp "==" (Field e "desc") (String desc))
  stm $ If (UnOp "not" type_is_ok) [badInput i e desc] []
  mapM_ entryPointInput $ zip3 (repeat i) (map Imp.TransparentValue vs) $
    map (Index (Field e "data") . IdxExp . Integer) [0..]

entryPointInput (i, Imp.TransparentValue (Imp.ScalarValue bt s name), e) = do
  let vname' = Var $ compileName name
      -- HACK: A Numpy int64 will signal an OverflowError if we pass
      -- it a number bigger than 2**63.  This does not happen if we
      -- pass e.g. int8 a number bigger than 2**7.  As a workaround,
      -- we first go through the corresponding ctypes type, which does
      -- not have this problem.
      ctobject = compilePrimType bt
      ctcall = simpleCall ctobject [e]
      npobject = compilePrimToNp bt
      npcall = simpleCall npobject [ctcall]
  stm $ Try [Assign vname' npcall]
    [Catch (Tuple [Var "TypeError", Var "AssertionError"])
     [badInput i e $ prettySigned (s==Imp.TypeUnsigned) bt]]

entryPointInput (i, Imp.TransparentValue (Imp.ArrayValue mem Imp.DefaultSpace t s dims), e) = do
  let type_is_wrong =
        UnOp "not" $
        BinOp "and"
        (BinOp "in" (simpleCall "type" [e]) (List [Var "np.ndarray"]))
        (BinOp "==" (Field e "dtype") (Var (compilePrimToExtNp t s)))
  stm $ If type_is_wrong
    [badInput i e $ concat (replicate (length dims) "[]") ++
     prettySigned (s==Imp.TypeUnsigned) t]
    []

  zipWithM_ (unpackDim e) dims [0..]
  let dest = Var $ compileName mem
      unwrap_call = simpleCall "unwrapArray" [e]

  stm $ Assign dest unwrap_call

entryPointInput (i, Imp.TransparentValue (Imp.ArrayValue mem (Imp.Space sid) bt ept dims), e) = do
  unpack_input <- asks envEntryInput
  unpack <- collect $ unpack_input mem sid bt ept dims e
  stm $ Try unpack
    [Catch (Tuple [Var "TypeError", Var "AssertionError"])
     [badInput i e $ concat (replicate (length dims) "[]") ++
     prettySigned (ept==Imp.TypeUnsigned) bt]]

extValueDescName :: Imp.ExternalValue -> String
extValueDescName (Imp.TransparentValue v) = extName $ valueDescName v
extValueDescName (Imp.OpaqueValue desc []) = extName $ zEncodeString desc
extValueDescName (Imp.OpaqueValue desc (v:_)) =
  extName $ zEncodeString desc ++ "_" ++ pretty (baseTag (valueDescVName v))

extName :: String -> String
extName = (++"_ext")

valueDescName :: Imp.ValueDesc -> String
valueDescName = compileName . valueDescVName

valueDescVName :: Imp.ValueDesc -> VName
valueDescVName (Imp.ScalarValue _ _ vname) = vname
valueDescVName (Imp.ArrayValue vname _ _ _ _) = vname

-- Key into the FUTHARK_PRIMTYPES dict.
readTypeEnum :: PrimType -> Imp.Signedness -> String
readTypeEnum (IntType Int8)  Imp.TypeUnsigned = "u8"
readTypeEnum (IntType Int16) Imp.TypeUnsigned = "u16"
readTypeEnum (IntType Int32) Imp.TypeUnsigned = "u32"
readTypeEnum (IntType Int64) Imp.TypeUnsigned = "u64"
readTypeEnum (IntType Int8)  Imp.TypeDirect   = "i8"
readTypeEnum (IntType Int16) Imp.TypeDirect   = "i16"
readTypeEnum (IntType Int32) Imp.TypeDirect   = "i32"
readTypeEnum (IntType Int64) Imp.TypeDirect   = "i64"
readTypeEnum (FloatType Float32) _ = "f32"
readTypeEnum (FloatType Float64) _ = "f64"
readTypeEnum Imp.Bool _ = "bool"
readTypeEnum Cert _ = error "readTypeEnum: cert"

readInput :: Imp.ExternalValue -> PyStmt
readInput (Imp.OpaqueValue desc _) =
  Raise $ simpleCall "Exception"
  [String $ "Cannot read argument of type " ++ desc ++ "."]

readInput decl@(Imp.TransparentValue (Imp.ScalarValue bt ept _)) =
  let type_name = readTypeEnum bt ept
  in Assign (Var $ extValueDescName decl) $ simpleCall "read_value" [String type_name]

readInput decl@(Imp.TransparentValue (Imp.ArrayValue _ _ bt ept dims)) =
  let type_name = readTypeEnum bt ept
  in Assign (Var $ extValueDescName decl) $ simpleCall "read_value"
     [String $ concat (replicate (length dims) "[]") ++ type_name]

printValue :: [(Imp.ExternalValue, PyExp)] -> CompilerM op s [PyStmt]
printValue = fmap concat . mapM (uncurry printValue')
  -- We copy non-host arrays to the host before printing.  This is
  -- done in a hacky way - we assume the value has a .get()-method
  -- that returns an equivalent Numpy array.  This works for PyOpenCL,
  -- but we will probably need yet another plugin mechanism here in
  -- the future.
  where printValue' (Imp.OpaqueValue desc _) _ =
          return [Exp $ simpleCall "sys.stdout.write"
                  [String $ "#<opaque " ++ desc ++ ">"]]
        printValue' (Imp.TransparentValue (Imp.ArrayValue mem (Space _) bt ept shape)) e =
          printValue' (Imp.TransparentValue (Imp.ArrayValue mem DefaultSpace bt ept shape)) $
          simpleCall (pretty e ++ ".get") []
        printValue' (Imp.TransparentValue _) e =
          return [Exp $ Call (Var "write_value")
                   [Arg e,
                    ArgKeyword "binary" (Var "binary_output")],
                  Exp $ simpleCall "sys.stdout.write" [String "\n"]]

prepareEntry :: (Name, Imp.Function op) -> CompilerM op s
                (String, [String], [PyStmt], [PyStmt], [PyStmt], [PyStmt],
                 [(Imp.ExternalValue, PyExp)], [PyStmt])
prepareEntry (fname, Imp.Function _ outputs inputs _ results args) = do
  let output_paramNames = map (compileName . Imp.paramName) outputs
      funTuple = tupleOrSingle $ fmap Var output_paramNames

  (argexps_mem_copies, prepare_run) <- collect' $ forM inputs $ \case
    Imp.MemParam name space -> do
      -- A program might write to its input parameters, so create a new memory
      -- block and copy the source there.  This way the program can be run more
      -- than once.
      name' <- newVName $ baseString name <> "_copy"
      copy <- asks envCopy
      allocate <- asks envAllocate
      let size = Var (extName (compileName name) ++ ".nbytes") -- FIXME
          dest = name'
          src = name
          offset = Integer 0
      case space of
        DefaultSpace ->
          stm $ Assign (Var (compileName name'))
                       (simpleCall "allocateMem" [size]) -- FIXME
        Space sid ->
          allocate name' size sid
      copy dest offset space src offset space size (IntType Int32) -- FIXME
      return $ Just $ compileName name'
    _ -> return Nothing

  prepareIn <- collect $ mapM_ entryPointInput $ zip3 [0..] args $
               map (Var . extValueDescName) args
  (res, prepareOut) <- collect' $ mapM entryPointOutput results

  let argexps_lib = map (compileName . Imp.paramName) inputs
      argexps_bin = zipWith fromMaybe argexps_lib argexps_mem_copies
      fname' = "self." ++ futharkFun (nameToString fname)
      call_lib = [Assign funTuple $ simpleCall fname' (fmap Var argexps_lib)]
      call_bin = [Assign funTuple $ simpleCall fname' (fmap Var argexps_bin)]

  return (nameToString fname, map extValueDescName args,
          prepareIn, call_lib, call_bin, prepareOut,
          zip results res, prepare_run)

copyMemoryDefaultSpace :: VName -> PyExp -> VName -> PyExp -> PyExp ->
                          CompilerM op s ()
copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes = do
  let offset_call1 = simpleCall "addressOffset"
                     [Var (compileName destmem), destidx, Var "ct.c_byte"]
  let offset_call2 = simpleCall "addressOffset"
                     [Var (compileName srcmem), srcidx, Var "ct.c_byte"]
  stm $ Exp $ simpleCall "ct.memmove" [offset_call1, offset_call2, nbytes]

compileEntryFun :: (Name, Imp.Function op)
                -> CompilerM op s (PyFunDef, (PyExp, PyExp))
compileEntryFun entry = do
  (fname', params, prepareIn, body_lib, _, prepareOut, res, _) <- prepareEntry entry
  let ret = Return $ tupleOrSingle $ map snd res
      (pts, rts) = entryTypes $ snd entry
  return (Def fname' ("self" : params) $
           prepareIn ++ body_lib ++ prepareOut ++ [ret],
          (String fname', Tuple [List (map String pts), List (map String rts)]))

entryTypes :: Imp.Function op -> ([String], [String])
entryTypes func = (map desc $ Imp.functionArgs func,
                   map desc $ Imp.functionResult func)
  where desc (Imp.OpaqueValue d _) = d
        desc (Imp.TransparentValue (Imp.ScalarValue pt s _)) = readTypeEnum pt s
        desc (Imp.TransparentValue (Imp.ArrayValue _ _ pt s dims)) =
          concat (replicate (length dims) "[]") ++ readTypeEnum pt s

callEntryFun :: [PyStmt] -> (Name, Imp.Function op)
             -> CompilerM op s (PyFunDef, String, PyExp)
callEntryFun pre_timing entry@(fname, Imp.Function _ _ _ _ _ decl_args) = do
  (_, _, prepareIn, _, body_bin, _, res, prepare_run) <- prepareEntry entry

  let str_input = map readInput decl_args

      exitcall = [Exp $ simpleCall "sys.exit" [Field (String "Assertion.{} failed") "format(e)"]]
      except' = Catch (Var "AssertionError") exitcall
      do_run = body_bin ++ pre_timing
      (do_run_with_timing, close_runtime_file) = addTiming do_run

      -- We ignore overflow errors and the like for executable entry
      -- points.  These are (somewhat) well-defined in Futhark.
      ignore s = ArgKeyword s $ String "ignore"
      errstate = Call (Var "np.errstate") $ map ignore ["divide", "over", "under", "invalid"]

      do_warmup_run =
        If (Var "do_warmup_run") (prepare_run ++ do_run) []

      do_num_runs =
        For "i" (simpleCall "range" [simpleCall "int" [Var "num_runs"]])
        (prepare_run ++ do_run_with_timing)

  str_output <- printValue res

  let fname' = "entry_" ++ nameToString fname

  return (Def fname' [] $
           str_input ++ prepareIn ++
           [Try [With errstate [do_warmup_run, do_num_runs]] [except']] ++
           [close_runtime_file] ++
           str_output,

          nameToString fname,

          Var fname')

addTiming :: [PyStmt] -> ([PyStmt], PyStmt)
addTiming statements =
  ([ Assign (Var "time_start") $ simpleCall "time.time" [] ] ++
   statements ++
   [ Assign (Var "time_end") $ simpleCall "time.time" []
   , If (Var "runtime_file") print_runtime [] ],

   If (Var "runtime_file") [Exp $ simpleCall "runtime_file.close" []] [])
  where print_runtime =
          [Exp $ simpleCall "runtime_file.write"
           [simpleCall "str"
            [BinOp "-"
             (toMicroseconds (Var "time_end"))
             (toMicroseconds (Var "time_start"))]],
           Exp $ simpleCall "runtime_file.write" [String "\n"]]
        toMicroseconds x =
          simpleCall "int" [BinOp "*" x $ Integer 1000000]

compileUnOp :: Imp.UnOp -> String
compileUnOp op =
  case op of
    Not -> "not"
    Complement{} -> "~"
    Abs{} -> "abs"
    FAbs{} -> "abs"
    SSignum{} -> "ssignum"
    USignum{} -> "usignum"

compileBinOpLike :: Monad m =>
                    Imp.Exp -> Imp.Exp
                 -> CompilerM op s (PyExp, PyExp, String -> m PyExp)
compileBinOpLike x y = do
  x' <- compileExp x
  y' <- compileExp y
  let simple s = return $ BinOp s x' y'
  return (x', y', simple)

-- | The ctypes type corresponding to a 'PrimType'.
compilePrimType :: PrimType -> String
compilePrimType t =
  case t of
    IntType Int8 -> "ct.c_int8"
    IntType Int16 -> "ct.c_int16"
    IntType Int32 -> "ct.c_int32"
    IntType Int64 -> "ct.c_int64"
    FloatType Float32 -> "ct.c_float"
    FloatType Float64 -> "ct.c_double"
    Imp.Bool -> "ct.c_bool"
    Cert -> "ct.c_bool"

-- | The ctypes type corresponding to a 'PrimType', taking sign into account.
compilePrimTypeExt :: PrimType -> Imp.Signedness -> String
compilePrimTypeExt t ept =
  case (t, ept) of
    (IntType Int8, Imp.TypeUnsigned) -> "ct.c_uint8"
    (IntType Int16, Imp.TypeUnsigned) -> "ct.c_uint16"
    (IntType Int32, Imp.TypeUnsigned) -> "ct.c_uint32"
    (IntType Int64, Imp.TypeUnsigned) -> "ct.c_uint64"
    (IntType Int8, _) -> "ct.c_int8"
    (IntType Int16, _) -> "ct.c_int16"
    (IntType Int32, _) -> "ct.c_int32"
    (IntType Int64, _) -> "ct.c_int64"
    (FloatType Float32, _) -> "ct.c_float"
    (FloatType Float64, _) -> "ct.c_double"
    (Imp.Bool, _) -> "ct.c_bool"
    (Cert, _) -> "ct.c_byte"

-- | The Numpy type corresponding to a 'PrimType'.
compilePrimToNp :: Imp.PrimType -> String
compilePrimToNp bt =
  case bt of
    IntType Int8 -> "np.int8"
    IntType Int16 -> "np.int16"
    IntType Int32 -> "np.int32"
    IntType Int64 -> "np.int64"
    FloatType Float32 -> "np.float32"
    FloatType Float64 -> "np.float64"
    Imp.Bool -> "np.byte"
    Cert -> "np.byte"

-- | The Numpy type corresponding to a 'PrimType', taking sign into account.
compilePrimToExtNp :: Imp.PrimType -> Imp.Signedness -> String
compilePrimToExtNp bt ept =
  case (bt,ept) of
    (IntType Int8, Imp.TypeUnsigned) -> "np.uint8"
    (IntType Int16, Imp.TypeUnsigned) -> "np.uint16"
    (IntType Int32, Imp.TypeUnsigned) -> "np.uint32"
    (IntType Int64, Imp.TypeUnsigned) -> "np.uint64"
    (IntType Int8, _) -> "np.int8"
    (IntType Int16, _) -> "np.int16"
    (IntType Int32, _) -> "np.int32"
    (IntType Int64, _) -> "np.int64"
    (FloatType Float32, _) -> "np.float32"
    (FloatType Float64, _) -> "np.float64"
    (Imp.Bool, _) -> "np.bool_"
    (Cert, _) -> "np.byte"

compilePrimValue :: Imp.PrimValue -> PyExp
compilePrimValue (IntValue (Int8Value v)) =
  simpleCall "np.int8" [Integer $ toInteger v]
compilePrimValue (IntValue (Int16Value v)) =
  simpleCall "np.int16" [Integer $ toInteger v]
compilePrimValue (IntValue (Int32Value v)) =
  simpleCall "np.int32" [Integer $ toInteger v]
compilePrimValue (IntValue (Int64Value v)) =
  simpleCall "np.int64" [Integer $ toInteger v]
compilePrimValue (FloatValue (Float32Value v))
  | isInfinite v =
      if v > 0 then Var "np.inf" else Var "-np.inf"
  | isNaN v =
      Var "np.nan"
  | otherwise = simpleCall "np.float32" [Float $ fromRational $ toRational v]
compilePrimValue (FloatValue (Float64Value v))
  | isInfinite v =
      if v > 0 then Var "np.inf" else Var "-np.inf"
  | isNaN v =
      Var "np.nan"
  | otherwise = simpleCall "np.float64" [Float $ fromRational $ toRational v]
compilePrimValue (BoolValue v) = Bool v
compilePrimValue Checked = Var "True"

compileExp :: Imp.Exp -> CompilerM op s PyExp

compileExp (Imp.ValueExp v) = return $ compilePrimValue v

compileExp (Imp.LeafExp (Imp.ScalarVar vname) _) =
  return $ Var $ compileName vname

compileExp (Imp.LeafExp (Imp.SizeOf t) _) =
  return $ simpleCall (compilePrimToNp $ IntType Int32) [Integer $ primByteSize t]

compileExp (Imp.LeafExp (Imp.Index src (Imp.Count iexp) bt DefaultSpace _) _) = do
  iexp' <- compileExp iexp
  let bt' = compilePrimType bt
  let nptype = compilePrimToNp bt
  return $ simpleCall "indexArray" [Var $ compileName src, iexp', Var bt', Var nptype]

compileExp (Imp.LeafExp (Imp.Index src (Imp.Count iexp) restype (Imp.Space space) _) _) =
  join $ asks envReadScalar
    <*> pure src <*> compileExp iexp
    <*> pure restype <*> pure space

compileExp (Imp.BinOpExp op x y) = do
  (x', y', simple) <- compileBinOpLike x y
  case op of
    Add{} -> simple "+"
    Sub{} -> simple "-"
    Mul{} -> simple "*"
    FAdd{} -> simple "+"
    FSub{} -> simple "-"
    FMul{} -> simple "*"
    FDiv{} -> simple "/"
    FMod{} -> simple "%"
    Xor{} -> simple "^"
    And{} -> simple "&"
    Or{} -> simple "|"
    Shl{} -> simple "<<"
    LogAnd{} -> simple "and"
    LogOr{} -> simple "or"
    _ -> return $ simpleCall (pretty op) [x', y']

compileExp (Imp.ConvOpExp conv x) = do
  x' <- compileExp x
  return $ simpleCall (pretty conv) [x']

compileExp (Imp.CmpOpExp cmp x y) = do
  (x', y', simple) <- compileBinOpLike x y
  case cmp of
    CmpEq{} -> simple "=="
    FCmpLt{} -> simple "<"
    FCmpLe{} -> simple "<="
    CmpLlt -> simple "<"
    CmpLle -> simple "<="
    _ -> return $ simpleCall (pretty cmp) [x', y']

compileExp (Imp.UnOpExp op exp1) =
  UnOp (compileUnOp op) <$> compileExp exp1

compileExp (Imp.FunExp h args _) =
  simpleCall (futharkFun (pretty h)) <$> mapM compileExp args

compileCode :: Imp.Code op -> CompilerM op s ()

compileCode Imp.DebugPrint{} =
  return ()

compileCode (Imp.Op op) =
  join $ asks envOpCompiler <*> pure op

compileCode (Imp.If cond tb fb) = do
  cond' <- compileExp cond
  tb' <- collect $ compileCode tb
  fb' <- collect $ compileCode fb
  stm $ If cond' tb' fb'

compileCode (c1 Imp.:>>: c2) = do
  compileCode c1
  compileCode c2

compileCode (Imp.While cond body) = do
  cond' <- compileExp cond
  body' <- collect $ compileCode body
  stm $ While cond' body'

compileCode (Imp.For i it bound body) = do
  bound' <- compileExp bound
  let i' = compileName i
  body' <- collect $ compileCode body
  counter <- pretty <$> newVName "counter"
  one <- pretty <$> newVName "one"
  stm $ Assign (Var i') $ simpleCall (compilePrimToNp (IntType it)) [Integer 0]
  stm $ Assign (Var one) $ simpleCall (compilePrimToNp (IntType it)) [Integer 1]
  stm $ For counter (simpleCall "range" [bound']) $
    body' ++ [AssignOp "+" (Var i') (Var one)]

compileCode (Imp.SetScalar vname exp1) = do
  let name' = Var $ compileName vname
  exp1' <- compileExp exp1
  stm $ Assign name' exp1'

compileCode Imp.DeclareMem{} = return ()
compileCode (Imp.DeclareScalar v Cert) =
  stm $ Assign (Var $ compileName v) $ Var "True"
compileCode Imp.DeclareScalar{} = return ()

compileCode (Imp.DeclareArray name DefaultSpace t vs) = do
  -- It is important to store the Numpy array in a temporary variable
  -- to prevent it from going "out-of-scope" before calling
  -- unwrapArray (which internally uses the .ctype method); see
  -- https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.ctypes.html
  atInit $ Assign (Field (Var "self") arr_name) $ case vs of
    Imp.ArrayValues vs' ->
      Call (Var "np.array")
      [Arg $ List $ map compilePrimValue vs',
       ArgKeyword "dtype" $ Var $ compilePrimToNp t]
    Imp.ArrayZeros n ->
      Call (Var "np.zeros")
      [Arg $ Integer $ fromIntegral n,
       ArgKeyword "dtype" $ Var $ compilePrimToNp t]
  atInit $
    Assign (Field (Var "self") name') $
    simpleCall "unwrapArray" [Field (Var "self") arr_name]
  stm $ Assign (Var name') $ Field (Var "self") name'
  where name' = compileName name
        arr_name = name' <> "_arr"

compileCode (Imp.DeclareArray name (Space space) t vs) =
  join $ asks envStaticArray <*>
  pure name <*> pure space <*> pure t <*> pure vs

compileCode (Imp.Comment s code) = do
  code' <- collect $ compileCode code
  stm $ Comment s code'

compileCode (Imp.Assert e (Imp.ErrorMsg parts) (loc,locs)) = do
  e' <- compileExp e
  let onPart (Imp.ErrorString s) = return ("%s", String s)
      onPart (Imp.ErrorInt32 x) = ("%d",) <$> compileExp x
  (formatstrs, formatargs) <- unzip <$> mapM onPart parts
  stm $ Assert e' (BinOp "%"
                   (String $ "Error at " ++ stacktrace ++ ": " ++ concat formatstrs)
                   (Tuple formatargs))
  where stacktrace = intercalate " -> " (reverse $ map locStr $ loc:locs)

compileCode (Imp.Call dests fname args) = do
  args' <- mapM compileArg args
  let dests' = tupleOrSingle $ fmap Var (map compileName dests)
      fname'
        | isBuiltInFunction fname = futharkFun (pretty  fname)
        | otherwise               = "self." ++ futharkFun (pretty  fname)
      call' = simpleCall fname' args'
  -- If the function returns nothing (is called only for side
  -- effects), take care not to assign to an empty tuple.
  stm $ if null dests
        then Exp call'
        else Assign dests' call'
  where compileArg (Imp.MemArg m) = return $ Var $ compileName m
        compileArg (Imp.ExpArg e) = compileExp e

compileCode (Imp.SetMem dest src _) = do
  let src' = Var (compileName src)
  let dest' = Var (compileName dest)
  stm $ Assign dest' src'

compileCode (Imp.Allocate name (Imp.Count e) DefaultSpace) = do
  e' <- compileExp e
  let allocate' = simpleCall "allocateMem" [e']
  let name' = Var (compileName name)
  stm $ Assign name' allocate'

compileCode (Imp.Free name _) =
  stm $ Assign (Var (compileName name)) None

compileCode (Imp.Allocate name (Imp.Count e) (Imp.Space space)) =
  join $ asks envAllocate
    <*> pure name
    <*> compileExp e
    <*> pure space

compileCode (Imp.Copy dest (Imp.Count destoffset) DefaultSpace src (Imp.Count srcoffset) DefaultSpace (Imp.Count size)) = do
  destoffset' <- compileExp destoffset
  srcoffset' <- compileExp srcoffset
  let dest' = Var (compileName dest)
  let src' = Var (compileName src)
  size' <- compileExp size
  let offset_call1 = simpleCall "addressOffset" [dest', destoffset', Var "ct.c_byte"]
  let offset_call2 = simpleCall "addressOffset" [src', srcoffset', Var "ct.c_byte"]
  stm $ Exp $ simpleCall "ct.memmove" [offset_call1, offset_call2, size']

compileCode (Imp.Copy dest (Imp.Count destoffset) destspace src (Imp.Count srcoffset) srcspace (Imp.Count size)) = do
  copy <- asks envCopy
  join $ copy
    <$> pure dest <*> compileExp destoffset <*> pure destspace
    <*> pure src <*> compileExp srcoffset <*> pure srcspace
    <*> compileExp size <*> pure (IntType Int32) -- FIXME

compileCode (Imp.Write dest (Imp.Count idx) elemtype DefaultSpace _ elemexp) = do
  idx' <- compileExp idx
  elemexp' <- compileExp elemexp
  let dest' = Var $ compileName dest
  let elemtype' = compilePrimType elemtype
  let ctype = simpleCall elemtype' [elemexp']
  stm $ Exp $ simpleCall "writeScalarArray" [dest', idx', ctype]

compileCode (Imp.Write dest (Imp.Count idx) elemtype (Imp.Space space) _ elemexp) =
  join $ asks envWriteScalar
    <*> pure dest
    <*> compileExp idx
    <*> pure elemtype
    <*> pure space
    <*> compileExp elemexp

compileCode Imp.Skip = return ()
