-- | A generic Python code generator which is polymorphic in the type
-- of the operations.  Concretely, we use this to handle both
-- sequential and PyOpenCL Python code.
module Futhark.CodeGen.Backends.GenericPython
  ( compileProg,
    CompilerMode,
    Constructor (..),
    emptyConstructor,
    compileName,
    compileVar,
    compileDim,
    compileExp,
    compilePrimExp,
    compileCode,
    compilePrimValue,
    compilePrimType,
    compilePrimTypeExt,
    compilePrimToNp,
    compilePrimToExtNp,
    fromStorage,
    toStorage,
    Operations (..),
    defaultOperations,
    unpackDim,
    CompilerM (..),
    OpCompiler,
    WriteScalar,
    ReadScalar,
    Allocate,
    Copy,
    EntryOutput,
    EntryInput,
    CompilerEnv (..),
    CompilerState (..),
    stm,
    atInit,
    collect',
    collect,
    simpleCall,
    copyMemoryDefaultSpace,
  )
where

import Control.Monad.Identity
import Control.Monad.RWS
import Data.Char (isAlpha, isAlphaNum)
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.CodeGen.Backends.GenericPython.Options
import Futhark.CodeGen.ImpCode qualified as Imp
import Futhark.CodeGen.RTS.Python
import Futhark.Compiler.Config (CompilerMode (..))
import Futhark.IR.Prop (isBuiltInFunction, subExpVars)
import Futhark.IR.Syntax.Core (Space (..))
import Futhark.MonadFreshNames
import Futhark.Util (zEncodeText)
import Futhark.Util.Pretty (prettyString, prettyText)
import Language.Futhark.Primitive hiding (Bool)

-- | A substitute expression compiler, tried before the main
-- compilation function.
type OpCompiler op s = op -> CompilerM op s ()

-- | Write a scalar to the given memory block with the given index and
-- in the given memory space.
type WriteScalar op s =
  PyExp ->
  PyExp ->
  PrimType ->
  Imp.SpaceId ->
  PyExp ->
  CompilerM op s ()

-- | Read a scalar from the given memory block with the given index and
-- in the given memory space.
type ReadScalar op s =
  PyExp ->
  PyExp ->
  PrimType ->
  Imp.SpaceId ->
  CompilerM op s PyExp

-- | Allocate a memory block of the given size in the given memory
-- space, saving a reference in the given variable name.
type Allocate op s =
  PyExp ->
  PyExp ->
  Imp.SpaceId ->
  CompilerM op s ()

-- | Copy from one memory block to another.
type Copy op s =
  PyExp ->
  PyExp ->
  Imp.Space ->
  PyExp ->
  PyExp ->
  Imp.Space ->
  PyExp ->
  PrimType ->
  CompilerM op s ()

-- | Construct the Python array being returned from an entry point.
type EntryOutput op s =
  VName ->
  Imp.SpaceId ->
  PrimType ->
  Imp.Signedness ->
  [Imp.DimSize] ->
  CompilerM op s PyExp

-- | Unpack the array being passed to an entry point.
type EntryInput op s =
  PyExp ->
  Imp.SpaceId ->
  PrimType ->
  Imp.Signedness ->
  [Imp.DimSize] ->
  PyExp ->
  CompilerM op s ()

data Operations op s = Operations
  { opsWriteScalar :: WriteScalar op s,
    opsReadScalar :: ReadScalar op s,
    opsAllocate :: Allocate op s,
    opsCopy :: Copy op s,
    opsCompiler :: OpCompiler op s,
    opsEntryOutput :: EntryOutput op s,
    opsEntryInput :: EntryInput op s
  }

-- | A set of operations that fail for every operation involving
-- non-default memory spaces.  Uses plain pointers and @malloc@ for
-- memory management.
defaultOperations :: Operations op s
defaultOperations =
  Operations
    { opsWriteScalar = defWriteScalar,
      opsReadScalar = defReadScalar,
      opsAllocate = defAllocate,
      opsCopy = defCopy,
      opsCompiler = defCompiler,
      opsEntryOutput = defEntryOutput,
      opsEntryInput = defEntryInput
    }
  where
    defWriteScalar _ _ _ _ _ =
      error "Cannot write to non-default memory space because I am dumb"
    defReadScalar _ _ _ _ =
      error "Cannot read from non-default memory space"
    defAllocate _ _ _ =
      error "Cannot allocate in non-default memory space"
    defCopy _ _ _ _ _ _ _ _ =
      error "Cannot copy to or from non-default memory space"
    defCompiler _ =
      error "The default compiler cannot compile extended operations"
    defEntryOutput _ _ _ _ =
      error "Cannot return array not in default memory space"
    defEntryInput _ _ _ _ =
      error "Cannot accept array not in default memory space"

data CompilerEnv op s = CompilerEnv
  { envOperations :: Operations op s,
    envVarExp :: M.Map String PyExp
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

envEntryOutput :: CompilerEnv op s -> EntryOutput op s
envEntryOutput = opsEntryOutput . envOperations

envEntryInput :: CompilerEnv op s -> EntryInput op s
envEntryInput = opsEntryInput . envOperations

newCompilerEnv :: Operations op s -> CompilerEnv op s
newCompilerEnv ops =
  CompilerEnv
    { envOperations = ops,
      envVarExp = mempty
    }

data CompilerState s = CompilerState
  { compNameSrc :: VNameSource,
    compInit :: [PyStmt],
    compUserState :: s
  }

newCompilerState :: VNameSource -> s -> CompilerState s
newCompilerState src s =
  CompilerState
    { compNameSrc = src,
      compInit = [],
      compUserState = s
    }

newtype CompilerM op s a = CompilerM (RWS (CompilerEnv op s) [PyStmt] (CompilerState s) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState (CompilerState s),
      MonadReader (CompilerEnv op s),
      MonadWriter [PyStmt]
    )

instance MonadFreshNames (CompilerM op s) where
  getNameSource = gets compNameSrc
  putNameSource src = modify $ \s -> s {compNameSrc = src}

collect :: CompilerM op s () -> CompilerM op s [PyStmt]
collect m = pass $ do
  ((), w) <- listen m
  pure (w, const mempty)

collect' :: CompilerM op s a -> CompilerM op s (a, [PyStmt])
collect' m = pass $ do
  (x, w) <- listen m
  pure ((x, w), const mempty)

atInit :: PyStmt -> CompilerM op s ()
atInit x = modify $ \s ->
  s {compInit = compInit s ++ [x]}

stm :: PyStmt -> CompilerM op s ()
stm x = tell [x]

futharkFun :: T.Text -> T.Text
futharkFun s = "futhark_" <> zEncodeText s

compileOutput :: [Imp.Param] -> [PyExp]
compileOutput = map (Var . compileName . Imp.paramName)

runCompilerM ::
  Operations op s ->
  VNameSource ->
  s ->
  CompilerM op s a ->
  a
runCompilerM ops src userstate (CompilerM m) =
  fst $ evalRWS m (newCompilerEnv ops) (newCompilerState src userstate)

standardOptions :: [Option]
standardOptions =
  [ Option
      { optionLongName = "tuning",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "open",
        optionAction = [Exp $ simpleCall "read_tuning_file" [Var "sizes", Var "optarg"]]
      },
    -- Does not actually do anything for Python backends.
    Option
      { optionLongName = "cache-file",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "str",
        optionAction = [Pass]
      },
    Option
      { optionLongName = "log",
        optionShortName = Just 'L',
        optionArgument = NoArgument,
        optionAction = [Pass]
      }
  ]

executableOptions :: [Option]
executableOptions =
  standardOptions
    ++ [ Option
           { optionLongName = "write-runtime-to",
             optionShortName = Just 't',
             optionArgument = RequiredArgument "str",
             optionAction =
               [ If
                   (Var "runtime_file")
                   [Exp $ simpleCall "runtime_file.close" []]
                   [],
                 Assign (Var "runtime_file") $
                   simpleCall "open" [Var "optarg", String "w"]
               ]
           },
         Option
           { optionLongName = "runs",
             optionShortName = Just 'r',
             optionArgument = RequiredArgument "str",
             optionAction =
               [ Assign (Var "num_runs") $ Var "optarg",
                 Assign (Var "do_warmup_run") $ Bool True
               ]
           },
         Option
           { optionLongName = "entry-point",
             optionShortName = Just 'e',
             optionArgument = RequiredArgument "str",
             optionAction =
               [Assign (Var "entry_point") $ Var "optarg"]
           },
         Option
           { optionLongName = "binary-output",
             optionShortName = Just 'b',
             optionArgument = NoArgument,
             optionAction = [Assign (Var "binary_output") $ Bool True]
           }
       ]

functionExternalValues :: Imp.EntryPoint -> [Imp.ExternalValue]
functionExternalValues entry =
  map snd (Imp.entryPointResults entry) ++ map snd (Imp.entryPointArgs entry)

-- | Is this name a valid Python identifier?  If not, it should be escaped
-- before being emitted.
isValidPyName :: T.Text -> Bool
isValidPyName = maybe True check . T.uncons
  where
    check (c, cs) = isAlpha c && T.all constituent cs
    constituent c = isAlphaNum c || c == '_'

-- | If the provided text is a valid identifier, then return it
-- verbatim.  Otherwise, escape it such that it becomes valid.
escapeName :: Name -> T.Text
escapeName v
  | isValidPyName v' = v'
  | otherwise = zEncodeText v'
  where
    v' = nameToText v

opaqueDefs :: Imp.Functions a -> M.Map T.Text [PyExp]
opaqueDefs (Imp.Functions funs) =
  mconcat
    . map evd
    . concatMap functionExternalValues
    . mapMaybe (Imp.functionEntry . snd)
    $ funs
  where
    evd Imp.TransparentValue {} = mempty
    evd (Imp.OpaqueValue name vds) = M.singleton (nameToText name) $ map (String . vd) vds
    vd (Imp.ScalarValue pt s _) =
      readTypeEnum pt s
    vd (Imp.ArrayValue _ _ pt s dims) =
      mconcat (replicate (length dims) "[]") <> readTypeEnum pt s

-- | The class generated by the code generator must have a
-- constructor, although it can be vacuous.
data Constructor = Constructor [String] [PyStmt]

-- | A constructor that takes no arguments and does nothing.
emptyConstructor :: Constructor
emptyConstructor = Constructor ["self"] [Pass]

constructorToFunDef :: Constructor -> [PyStmt] -> PyFunDef
constructorToFunDef (Constructor params body) at_init =
  Def "__init__" params $ body <> at_init

compileProg ::
  MonadFreshNames m =>
  CompilerMode ->
  String ->
  Constructor ->
  [PyStmt] ->
  [PyStmt] ->
  Operations op s ->
  s ->
  [PyStmt] ->
  [Option] ->
  Imp.Definitions op ->
  m T.Text
compileProg mode class_name constructor imports defines ops userstate sync options prog = do
  src <- getNameSource
  let prog' = runCompilerM ops src userstate compileProg'
  pure . prettyText . PyProg $
    imports
      ++ [ Import "argparse" Nothing,
           Assign (Var "sizes") $ Dict []
         ]
      ++ defines
      ++ [ Escape valuesPy,
           Escape memoryPy,
           Escape panicPy,
           Escape tuningPy,
           Escape scalarPy,
           Escape serverPy
         ]
      ++ prog'
  where
    Imp.Definitions _types consts (Imp.Functions funs) = prog
    compileProg' = withConstantSubsts consts $ do
      compileConstants consts

      definitions <- mapM compileFunc funs
      at_inits <- gets compInit

      let constructor' = constructorToFunDef constructor at_inits

      case mode of
        ToLibrary -> do
          (entry_points, entry_point_types) <-
            unzip . catMaybes <$> mapM (compileEntryFun sync DoNotReturnTiming) funs
          pure
            [ ClassDef $
                Class class_name $
                  Assign (Var "entry_points") (Dict entry_point_types)
                    : Assign
                      (Var "opaques")
                      (Dict $ zip (map String opaque_names) (map Tuple opaque_payloads))
                    : map FunDef (constructor' : definitions ++ entry_points)
            ]
        ToServer -> do
          (entry_points, entry_point_types) <-
            unzip . catMaybes <$> mapM (compileEntryFun sync ReturnTiming) funs
          pure $
            parse_options_server
              ++ [ ClassDef
                     ( Class class_name $
                         Assign (Var "entry_points") (Dict entry_point_types)
                           : Assign
                             (Var "opaques")
                             (Dict $ zip (map String opaque_names) (map Tuple opaque_payloads))
                           : map FunDef (constructor' : definitions ++ entry_points)
                     ),
                   Assign
                     (Var "server")
                     (simpleCall "Server" [simpleCall class_name []]),
                   Exp $ simpleCall "server.run" []
                 ]
        ToExecutable -> do
          let classinst = Assign (Var "self") $ simpleCall class_name []
          (entry_point_defs, entry_point_names, entry_points) <-
            unzip3 . catMaybes <$> mapM (callEntryFun sync) funs
          pure $
            parse_options_executable
              ++ ClassDef
                ( Class class_name $
                    map FunDef $
                      constructor' : definitions
                )
              : classinst
              : map FunDef entry_point_defs
              ++ selectEntryPoint entry_point_names entry_points

    parse_options_executable =
      Assign (Var "runtime_file") None
        : Assign (Var "do_warmup_run") (Bool False)
        : Assign (Var "num_runs") (Integer 1)
        : Assign (Var "entry_point") (String "main")
        : Assign (Var "binary_output") (Bool False)
        : generateOptionParser (executableOptions ++ options)

    parse_options_server =
      generateOptionParser (standardOptions ++ options)

    (opaque_names, opaque_payloads) =
      unzip $ M.toList $ opaqueDefs $ Imp.defFuns prog

    selectEntryPoint entry_point_names entry_points =
      [ Assign (Var "entry_points") $
          Dict $
            zip (map String entry_point_names) entry_points,
        Assign (Var "entry_point_fun") $
          simpleCall "entry_points.get" [Var "entry_point"],
        If
          (BinOp "==" (Var "entry_point_fun") None)
          [ Exp $
              simpleCall
                "sys.exit"
                [ Call
                    ( Field
                        (String "No entry point '{}'.  Select another with --entry point.  Options are:\n{}")
                        "format"
                    )
                    [ Arg $ Var "entry_point",
                      Arg $
                        Call
                          (Field (String "\n") "join")
                          [Arg $ simpleCall "entry_points.keys" []]
                    ]
                ]
          ]
          [Exp $ simpleCall "entry_point_fun" []]
      ]

withConstantSubsts :: Imp.Constants op -> CompilerM op s a -> CompilerM op s a
withConstantSubsts (Imp.Constants ps _) =
  local $ \env -> env {envVarExp = foldMap constExp ps}
  where
    constExp p =
      M.singleton
        (compileName $ Imp.paramName p)
        (Index (Var "self.constants") $ IdxExp $ String $ prettyText $ Imp.paramName p)

compileConstants :: Imp.Constants op -> CompilerM op s ()
compileConstants (Imp.Constants _ init_consts) = do
  atInit $ Assign (Var "self.constants") $ Dict []
  mapM_ atInit =<< collect (compileCode init_consts)

compileFunc :: (Name, Imp.Function op) -> CompilerM op s PyFunDef
compileFunc (fname, Imp.Function _ outputs inputs body) = do
  body' <- collect $ compileCode body
  let inputs' = map (compileName . Imp.paramName) inputs
  let ret = Return $ tupleOrSingle $ compileOutput outputs
  pure $
    Def (T.unpack $ futharkFun $ nameToText fname) ("self" : inputs') $
      body' ++ [ret]

tupleOrSingle :: [PyExp] -> PyExp
tupleOrSingle [e] = e
tupleOrSingle es = Tuple es

-- | A 'Call' where the function is a variable and every argument is a
-- simple 'Arg'.
simpleCall :: String -> [PyExp] -> PyExp
simpleCall fname = Call (Var fname) . map Arg

compileName :: VName -> String
compileName = T.unpack . zEncodeText . prettyText

compileDim :: Imp.DimSize -> CompilerM op s PyExp
compileDim (Imp.Constant v) = pure $ compilePrimValue v
compileDim (Imp.Var v) = compileVar v

unpackDim :: PyExp -> Imp.DimSize -> Int32 -> CompilerM op s ()
unpackDim arr_name (Imp.Constant c) i = do
  let shape_name = Field arr_name "shape"
  let constant_c = compilePrimValue c
  let constant_i = Integer $ toInteger i
  stm $
    Assert (BinOp "==" constant_c (Index shape_name $ IdxExp constant_i)) $
      String "Entry point arguments have invalid sizes."
unpackDim arr_name (Imp.Var var) i = do
  let shape_name = Field arr_name "shape"
      src = Index shape_name $ IdxExp $ Integer $ toInteger i
  var' <- compileVar var
  stm $
    If
      (BinOp "==" var' None)
      [Assign var' $ simpleCall "np.int64" [src]]
      [ Assert (BinOp "==" var' src) $
          String "Error: entry point arguments have invalid sizes."
      ]

entryPointOutput :: Imp.ExternalValue -> CompilerM op s PyExp
entryPointOutput (Imp.OpaqueValue desc vs) =
  simpleCall "opaque" . (String (prettyText desc) :)
    <$> mapM (entryPointOutput . Imp.TransparentValue) vs
entryPointOutput (Imp.TransparentValue (Imp.ScalarValue bt ept name)) = do
  name' <- compileVar name
  pure $ simpleCall tf [name']
  where
    tf = compilePrimToExtNp bt ept
entryPointOutput (Imp.TransparentValue (Imp.ArrayValue mem (Imp.Space sid) bt ept dims)) = do
  pack_output <- asks envEntryOutput
  pack_output mem sid bt ept dims
entryPointOutput (Imp.TransparentValue (Imp.ArrayValue mem _ bt ept dims)) = do
  mem' <- Cast <$> compileVar mem <*> pure (compilePrimTypeExt bt ept)
  dims' <- mapM compileDim dims
  pure $ simpleCall "createArray" [mem', Tuple dims', Var $ compilePrimToExtNp bt ept]

badInput :: Int -> PyExp -> T.Text -> PyStmt
badInput i e t =
  Raise $
    simpleCall
      "TypeError"
      [ Call
          (Field (String err_msg) "format")
          [Arg (String t), Arg $ simpleCall "type" [e], Arg e]
      ]
  where
    err_msg =
      T.unlines
        [ "Argument #" <> prettyText i <> " has invalid value",
          "Futhark type: {}",
          "Argument has Python type {} and value: {}"
        ]

badInputType :: Int -> PyExp -> T.Text -> PyExp -> PyExp -> PyStmt
badInputType i e t de dg =
  Raise $
    simpleCall
      "TypeError"
      [ Call
          (Field (String err_msg) "format")
          [Arg (String t), Arg $ simpleCall "type" [e], Arg e, Arg de, Arg dg]
      ]
  where
    err_msg =
      T.unlines
        [ "Argument #" <> prettyText i <> " has invalid value",
          "Futhark type: {}",
          "Argument has Python type {} and value: {}",
          "Expected array with elements of dtype: {}",
          "The array given has elements of dtype: {}"
        ]

badInputDim :: Int -> PyExp -> T.Text -> Int -> PyStmt
badInputDim i e typ dimf =
  Raise $
    simpleCall
      "TypeError"
      [ Call
          (Field (String err_msg) "format")
          [Arg eft, Arg aft]
      ]
  where
    eft = String (mconcat (replicate dimf "[]") <> typ)
    aft = BinOp "+" (BinOp "*" (String "[]") (Field e "ndim")) (String typ)
    err_msg =
      T.unlines
        [ "Argument #" <> prettyText i <> " has invalid value",
          "Dimensionality mismatch",
          "Expected Futhark type: {}",
          "Bad Python value passed",
          "Actual Futhark type: {}"
        ]

declEntryPointInputSizes :: [Imp.ExternalValue] -> CompilerM op s ()
declEntryPointInputSizes = mapM_ onSize . concatMap sizes
  where
    sizes (Imp.TransparentValue v) = valueSizes v
    sizes (Imp.OpaqueValue _ vs) = concatMap valueSizes vs
    valueSizes (Imp.ArrayValue _ _ _ _ dims) = subExpVars dims
    valueSizes Imp.ScalarValue {} = []
    onSize v = stm $ Assign (Var (compileName v)) None

entryPointInput :: (Int, Imp.ExternalValue, PyExp) -> CompilerM op s ()
entryPointInput (i, Imp.OpaqueValue desc vs, e) = do
  let type_is_ok =
        BinOp
          "and"
          (simpleCall "isinstance" [e, Var "opaque"])
          (BinOp "==" (Field e "desc") (String (nameToText desc)))
  stm $ If (UnOp "not" type_is_ok) [badInput i e (nameToText desc)] []
  mapM_ entryPointInput $
    zip3 (repeat i) (map Imp.TransparentValue vs) $
      map (Index (Field e "data") . IdxExp . Integer) [0 ..]
entryPointInput (i, Imp.TransparentValue (Imp.ScalarValue bt s name), e) = do
  vname' <- compileVar name
  let -- HACK: A Numpy int64 will signal an OverflowError if we pass
      -- it a number bigger than 2**63.  This does not happen if we
      -- pass e.g. int8 a number bigger than 2**7.  As a workaround,
      -- we first go through the corresponding ctypes type, which does
      -- not have this problem.
      ctobject = compilePrimType bt
      npobject = compilePrimToNp bt
      npcall =
        simpleCall
          npobject
          [ case bt of
              IntType Int64 -> simpleCall ctobject [e]
              _ -> e
          ]
  stm $
    Try
      [Assign vname' npcall]
      [ Catch
          (Tuple [Var "TypeError", Var "AssertionError"])
          [badInput i e $ prettySigned (s == Imp.Unsigned) bt]
      ]
entryPointInput (i, Imp.TransparentValue (Imp.ArrayValue mem (Imp.Space sid) bt ept dims), e) = do
  unpack_input <- asks envEntryInput
  mem' <- compileVar mem
  unpack <- collect $ unpack_input mem' sid bt ept dims e
  stm $
    Try
      unpack
      [ Catch
          (Tuple [Var "TypeError", Var "AssertionError"])
          [ badInput i e $
              mconcat (replicate (length dims) "[]")
                <> prettySigned (ept == Imp.Unsigned) bt
          ]
      ]
entryPointInput (i, Imp.TransparentValue (Imp.ArrayValue mem _ t s dims), e) = do
  let type_is_wrong = UnOp "not" $ BinOp "in" (simpleCall "type" [e]) $ List [Var "np.ndarray"]
  let dtype_is_wrong = UnOp "not" $ BinOp "==" (Field e "dtype") $ Var $ compilePrimToExtNp t s
  let dim_is_wrong = UnOp "not" $ BinOp "==" (Field e "ndim") $ Integer $ toInteger $ length dims
  stm $
    If
      type_is_wrong
      [ badInput i e $
          mconcat (replicate (length dims) "[]")
            <> prettySigned (s == Imp.Unsigned) t
      ]
      []
  stm $
    If
      dtype_is_wrong
      [ badInputType
          i
          e
          (mconcat (replicate (length dims) "[]") <> prettySigned (s == Imp.Unsigned) t)
          (simpleCall "np.dtype" [Var (compilePrimToExtNp t s)])
          (Field e "dtype")
      ]
      []
  stm $
    If
      dim_is_wrong
      [badInputDim i e (prettySigned (s == Imp.Unsigned) t) (length dims)]
      []

  zipWithM_ (unpackDim e) dims [0 ..]
  dest <- compileVar mem
  let unwrap_call = simpleCall "unwrapArray" [e]

  stm $ Assign dest unwrap_call

extValueDescName :: Imp.ExternalValue -> T.Text
extValueDescName (Imp.TransparentValue v) = extName $ T.pack $ compileName $ valueDescVName v
extValueDescName (Imp.OpaqueValue desc []) = extName $ zEncodeText $ nameToText desc
extValueDescName (Imp.OpaqueValue desc (v : _)) =
  extName $ zEncodeText (nameToText desc) <> "_" <> prettyText (baseTag (valueDescVName v))

extName :: T.Text -> T.Text
extName = (<> "_ext")

valueDescVName :: Imp.ValueDesc -> VName
valueDescVName (Imp.ScalarValue _ _ vname) = vname
valueDescVName (Imp.ArrayValue vname _ _ _ _) = vname

-- Key into the FUTHARK_PRIMTYPES dict.
readTypeEnum :: PrimType -> Imp.Signedness -> T.Text
readTypeEnum (IntType Int8) Imp.Unsigned = "u8"
readTypeEnum (IntType Int16) Imp.Unsigned = "u16"
readTypeEnum (IntType Int32) Imp.Unsigned = "u32"
readTypeEnum (IntType Int64) Imp.Unsigned = "u64"
readTypeEnum (IntType Int8) Imp.Signed = "i8"
readTypeEnum (IntType Int16) Imp.Signed = "i16"
readTypeEnum (IntType Int32) Imp.Signed = "i32"
readTypeEnum (IntType Int64) Imp.Signed = "i64"
readTypeEnum (FloatType Float16) _ = "f16"
readTypeEnum (FloatType Float32) _ = "f32"
readTypeEnum (FloatType Float64) _ = "f64"
readTypeEnum Imp.Bool _ = "bool"
readTypeEnum Unit _ = "bool"

readInput :: Imp.ExternalValue -> PyStmt
readInput (Imp.OpaqueValue desc _) =
  Raise $
    simpleCall
      "Exception"
      [String $ "Cannot read argument of type " <> nameToText desc <> "."]
readInput decl@(Imp.TransparentValue (Imp.ScalarValue bt ept _)) =
  let type_name = readTypeEnum bt ept
   in Assign (Var $ T.unpack $ extValueDescName decl) $ simpleCall "read_value" [String type_name]
readInput decl@(Imp.TransparentValue (Imp.ArrayValue _ _ bt ept dims)) =
  let type_name = readTypeEnum bt ept
   in Assign (Var $ T.unpack $ extValueDescName decl) $
        simpleCall
          "read_value"
          [String $ mconcat (replicate (length dims) "[]") <> type_name]

printValue :: [(Imp.ExternalValue, PyExp)] -> CompilerM op s [PyStmt]
printValue = fmap concat . mapM (uncurry printValue')
  where
    -- We copy non-host arrays to the host before printing.  This is
    -- done in a hacky way - we assume the value has a .get()-method
    -- that returns an equivalent Numpy array.  This works for PyOpenCL,
    -- but we will probably need yet another plugin mechanism here in
    -- the future.
    printValue' (Imp.OpaqueValue desc _) _ =
      pure
        [ Exp $
            simpleCall
              "sys.stdout.write"
              [String $ "#<opaque " <> nameToText desc <> ">"]
        ]
    printValue' (Imp.TransparentValue (Imp.ArrayValue mem (Space _) bt ept shape)) e =
      printValue' (Imp.TransparentValue (Imp.ArrayValue mem DefaultSpace bt ept shape)) $
        simpleCall (prettyString e ++ ".get") []
    printValue' (Imp.TransparentValue _) e =
      pure
        [ Exp $
            Call
              (Var "write_value")
              [ Arg e,
                ArgKeyword "binary" (Var "binary_output")
              ],
          Exp $ simpleCall "sys.stdout.write" [String "\n"]
        ]

prepareEntry ::
  Imp.EntryPoint ->
  (Name, Imp.Function op) ->
  CompilerM
    op
    s
    ( [String],
      [PyStmt],
      [PyStmt],
      [PyStmt],
      [(Imp.ExternalValue, PyExp)]
    )
prepareEntry (Imp.EntryPoint _ results args) (fname, Imp.Function _ outputs inputs _) = do
  let output_paramNames = map (compileName . Imp.paramName) outputs
      funTuple = tupleOrSingle $ fmap Var output_paramNames

  prepareIn <- collect $ do
    declEntryPointInputSizes $ map snd args
    mapM_ entryPointInput . zip3 [0 ..] (map snd args) $
      map (Var . T.unpack . extValueDescName . snd) args
  (res, prepareOut) <- collect' $ mapM (entryPointOutput . snd) results

  let argexps_lib = map (compileName . Imp.paramName) inputs
      fname' = "self." <> futharkFun (nameToText fname)

      -- We ignore overflow errors and the like for executable entry
      -- points.  These are (somewhat) well-defined in Futhark.
      ignore s = ArgKeyword s $ String "ignore"
      errstate = Call (Var "np.errstate") $ map ignore ["divide", "over", "under", "invalid"]

      call argexps =
        [ With
            errstate
            [Assign funTuple $ simpleCall (T.unpack fname') (fmap Var argexps)]
        ]

  pure
    ( map (T.unpack . extValueDescName . snd) args,
      prepareIn,
      call argexps_lib,
      prepareOut,
      zip (map snd results) res
    )

copyMemoryDefaultSpace ::
  PyExp ->
  PyExp ->
  PyExp ->
  PyExp ->
  PyExp ->
  CompilerM op s ()
copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes = do
  let offset_call1 =
        simpleCall
          "addressOffset"
          [destmem, destidx, Var "ct.c_byte"]
  let offset_call2 =
        simpleCall
          "addressOffset"
          [srcmem, srcidx, Var "ct.c_byte"]
  stm $ Exp $ simpleCall "ct.memmove" [offset_call1, offset_call2, nbytes]

data ReturnTiming = ReturnTiming | DoNotReturnTiming

compileEntryFun ::
  [PyStmt] ->
  ReturnTiming ->
  (Name, Imp.Function op) ->
  CompilerM op s (Maybe (PyFunDef, (PyExp, PyExp)))
compileEntryFun sync timing fun
  | Just entry <- Imp.functionEntry $ snd fun = do
      let ename = Imp.entryPointName entry
      (params, prepareIn, body_lib, prepareOut, res) <- prepareEntry entry fun
      let (maybe_sync, ret) =
            case timing of
              DoNotReturnTiming ->
                ( [],
                  Return $ tupleOrSingle $ map snd res
                )
              ReturnTiming ->
                ( sync,
                  Return $
                    Tuple
                      [ Var "runtime",
                        tupleOrSingle $ map snd res
                      ]
                )
          (pts, rts) = entryTypes entry

          do_run =
            Assign (Var "time_start") (simpleCall "time.time" [])
              : body_lib
              ++ maybe_sync
              ++ [ Assign (Var "runtime") $
                     BinOp
                       "-"
                       (toMicroseconds (simpleCall "time.time" []))
                       (toMicroseconds (Var "time_start"))
                 ]

      pure $
        Just
          ( Def (T.unpack (escapeName ename)) ("self" : params) $
              prepareIn ++ do_run ++ prepareOut ++ sync ++ [ret],
            ( String (nameToText ename),
              Tuple
                [ String (escapeName ename),
                  List (map String pts),
                  List (map String rts)
                ]
            )
          )
  | otherwise = pure Nothing

entryTypes :: Imp.EntryPoint -> ([T.Text], [T.Text])
entryTypes (Imp.EntryPoint _ res args) =
  (map descArg args, map desc res)
  where
    descArg ((_, u), d) = desc (u, d)
    desc (u, Imp.OpaqueValue d _) = prettyText u <> nameToText d
    desc (u, Imp.TransparentValue (Imp.ScalarValue pt s _)) = prettyText u <> readTypeEnum pt s
    desc (u, Imp.TransparentValue (Imp.ArrayValue _ _ pt s dims)) =
      prettyText u <> mconcat (replicate (length dims) "[]") <> readTypeEnum pt s

callEntryFun ::
  [PyStmt] ->
  (Name, Imp.Function op) ->
  CompilerM op s (Maybe (PyFunDef, T.Text, PyExp))
callEntryFun _ (_, Imp.Function Nothing _ _ _) = pure Nothing
callEntryFun pre_timing fun@(fname, Imp.Function (Just entry) _ _ _) = do
  let Imp.EntryPoint ename _ decl_args = entry
  (_, prepare_in, body_bin, _, res) <- prepareEntry entry fun

  let str_input = map (readInput . snd) decl_args
      end_of_input = [Exp $ simpleCall "end_of_input" [String $ prettyText fname]]

      exitcall = [Exp $ simpleCall "sys.exit" [Field (String "Assertion.{} failed") "format(e)"]]
      except' = Catch (Var "AssertionError") exitcall
      do_run = body_bin ++ pre_timing
      (do_run_with_timing, close_runtime_file) = addTiming do_run

      do_warmup_run =
        If (Var "do_warmup_run") do_run []

      do_num_runs =
        For
          "i"
          (simpleCall "range" [simpleCall "int" [Var "num_runs"]])
          do_run_with_timing

  str_output <- printValue res

  let fname' = "entry_" ++ T.unpack (escapeName fname)

  pure $
    Just
      ( Def fname' [] $
          str_input
            ++ end_of_input
            ++ prepare_in
            ++ [Try [do_warmup_run, do_num_runs] [except']]
            ++ [close_runtime_file]
            ++ str_output,
        nameToText ename,
        Var fname'
      )

addTiming :: [PyStmt] -> ([PyStmt], PyStmt)
addTiming statements =
  ( [Assign (Var "time_start") $ simpleCall "time.time" []]
      ++ statements
      ++ [ Assign (Var "time_end") $ simpleCall "time.time" [],
           If (Var "runtime_file") print_runtime []
         ],
    If (Var "runtime_file") [Exp $ simpleCall "runtime_file.close" []] []
  )
  where
    print_runtime =
      [ Exp $
          simpleCall
            "runtime_file.write"
            [ simpleCall
                "str"
                [ BinOp
                    "-"
                    (toMicroseconds (Var "time_end"))
                    (toMicroseconds (Var "time_start"))
                ]
            ],
        Exp $ simpleCall "runtime_file.write" [String "\n"],
        Exp $ simpleCall "runtime_file.flush" []
      ]

toMicroseconds :: PyExp -> PyExp
toMicroseconds x =
  simpleCall "int" [BinOp "*" x $ Integer 1000000]

compileUnOp :: Imp.UnOp -> String
compileUnOp op =
  case op of
    Not -> "not"
    Complement {} -> "~"
    Abs {} -> "abs"
    FAbs {} -> "abs"
    SSignum {} -> "ssignum"
    USignum {} -> "usignum"
    FSignum {} -> "np.sign"

compileBinOpLike ::
  Monad m =>
  (v -> m PyExp) ->
  Imp.PrimExp v ->
  Imp.PrimExp v ->
  m (PyExp, PyExp, String -> m PyExp)
compileBinOpLike f x y = do
  x' <- compilePrimExp f x
  y' <- compilePrimExp f y
  let simple s = pure $ BinOp s x' y'
  pure (x', y', simple)

-- | The ctypes type corresponding to a 'PrimType'.
compilePrimType :: PrimType -> String
compilePrimType t =
  case t of
    IntType Int8 -> "ct.c_int8"
    IntType Int16 -> "ct.c_int16"
    IntType Int32 -> "ct.c_int32"
    IntType Int64 -> "ct.c_int64"
    FloatType Float16 -> "ct.c_uint16"
    FloatType Float32 -> "ct.c_float"
    FloatType Float64 -> "ct.c_double"
    Imp.Bool -> "ct.c_bool"
    Unit -> "ct.c_bool"

-- | The ctypes type corresponding to a 'PrimType', taking sign into account.
compilePrimTypeExt :: PrimType -> Imp.Signedness -> String
compilePrimTypeExt t ept =
  case (t, ept) of
    (IntType Int8, Imp.Unsigned) -> "ct.c_uint8"
    (IntType Int16, Imp.Unsigned) -> "ct.c_uint16"
    (IntType Int32, Imp.Unsigned) -> "ct.c_uint32"
    (IntType Int64, Imp.Unsigned) -> "ct.c_uint64"
    (IntType Int8, _) -> "ct.c_int8"
    (IntType Int16, _) -> "ct.c_int16"
    (IntType Int32, _) -> "ct.c_int32"
    (IntType Int64, _) -> "ct.c_int64"
    (FloatType Float16, _) -> "ct.c_uint16"
    (FloatType Float32, _) -> "ct.c_float"
    (FloatType Float64, _) -> "ct.c_double"
    (Imp.Bool, _) -> "ct.c_bool"
    (Unit, _) -> "ct.c_byte"

-- | The Numpy type corresponding to a 'PrimType'.
compilePrimToNp :: Imp.PrimType -> String
compilePrimToNp bt =
  case bt of
    IntType Int8 -> "np.int8"
    IntType Int16 -> "np.int16"
    IntType Int32 -> "np.int32"
    IntType Int64 -> "np.int64"
    FloatType Float16 -> "np.float16"
    FloatType Float32 -> "np.float32"
    FloatType Float64 -> "np.float64"
    Imp.Bool -> "np.byte"
    Unit -> "np.byte"

-- | The Numpy type corresponding to a 'PrimType', taking sign into account.
compilePrimToExtNp :: Imp.PrimType -> Imp.Signedness -> String
compilePrimToExtNp bt ept =
  case (bt, ept) of
    (IntType Int8, Imp.Unsigned) -> "np.uint8"
    (IntType Int16, Imp.Unsigned) -> "np.uint16"
    (IntType Int32, Imp.Unsigned) -> "np.uint32"
    (IntType Int64, Imp.Unsigned) -> "np.uint64"
    (IntType Int8, _) -> "np.int8"
    (IntType Int16, _) -> "np.int16"
    (IntType Int32, _) -> "np.int32"
    (IntType Int64, _) -> "np.int64"
    (FloatType Float16, _) -> "np.float16"
    (FloatType Float32, _) -> "np.float32"
    (FloatType Float64, _) -> "np.float64"
    (Imp.Bool, _) -> "np.bool_"
    (Unit, _) -> "np.byte"

-- | Convert from scalar to storage representation for the given type.
toStorage :: PrimType -> PyExp -> PyExp
toStorage (FloatType Float16) e =
  simpleCall "ct.c_int16" [simpleCall "futhark_to_bits16" [e]]
toStorage t e = simpleCall (compilePrimType t) [e]

-- | Convert from storage to scalar representation for the given type.
fromStorage :: PrimType -> PyExp -> PyExp
fromStorage (FloatType Float16) e =
  simpleCall "futhark_from_bits16" [simpleCall "np.int16" [e]]
fromStorage t e = simpleCall (compilePrimToNp t) [e]

compilePrimValue :: Imp.PrimValue -> PyExp
compilePrimValue (IntValue (Int8Value v)) =
  simpleCall "np.int8" [Integer $ toInteger v]
compilePrimValue (IntValue (Int16Value v)) =
  simpleCall "np.int16" [Integer $ toInteger v]
compilePrimValue (IntValue (Int32Value v)) =
  simpleCall "np.int32" [Integer $ toInteger v]
compilePrimValue (IntValue (Int64Value v)) =
  simpleCall "np.int64" [Integer $ toInteger v]
compilePrimValue (FloatValue (Float16Value v))
  | isInfinite v =
      if v > 0 then Var "np.inf" else Var "-np.inf"
  | isNaN v =
      Var "np.nan"
  | otherwise = simpleCall "np.float16" [Float $ fromRational $ toRational v]
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
compilePrimValue UnitValue = Var "None"

compileVar :: VName -> CompilerM op s PyExp
compileVar v = asks $ fromMaybe (Var v') . M.lookup v' . envVarExp
  where
    v' = compileName v

-- | Tell me how to compile a @v@, and I'll Compile any @PrimExp v@ for you.
compilePrimExp :: Monad m => (v -> m PyExp) -> Imp.PrimExp v -> m PyExp
compilePrimExp _ (Imp.ValueExp v) = pure $ compilePrimValue v
compilePrimExp f (Imp.LeafExp v _) = f v
compilePrimExp f (Imp.BinOpExp op x y) = do
  (x', y', simple) <- compileBinOpLike f x y
  case op of
    Add {} -> simple "+"
    Sub {} -> simple "-"
    Mul {} -> simple "*"
    FAdd {} -> simple "+"
    FSub {} -> simple "-"
    FMul {} -> simple "*"
    FDiv {} -> simple "/"
    FMod {} -> simple "%"
    Xor {} -> simple "^"
    And {} -> simple "&"
    Or {} -> simple "|"
    Shl {} -> simple "<<"
    LogAnd {} -> simple "and"
    LogOr {} -> simple "or"
    _ -> pure $ simpleCall (prettyString op) [x', y']
compilePrimExp f (Imp.ConvOpExp conv x) = do
  x' <- compilePrimExp f x
  pure $ simpleCall (prettyString conv) [x']
compilePrimExp f (Imp.CmpOpExp cmp x y) = do
  (x', y', simple) <- compileBinOpLike f x y
  case cmp of
    CmpEq {} -> simple "=="
    FCmpLt {} -> simple "<"
    FCmpLe {} -> simple "<="
    CmpLlt -> simple "<"
    CmpLle -> simple "<="
    _ -> pure $ simpleCall (prettyString cmp) [x', y']
compilePrimExp f (Imp.UnOpExp op exp1) =
  UnOp (compileUnOp op) <$> compilePrimExp f exp1
compilePrimExp f (Imp.FunExp h args _) =
  simpleCall (T.unpack (futharkFun (prettyText h))) <$> mapM (compilePrimExp f) args

compileExp :: Imp.Exp -> CompilerM op s PyExp
compileExp = compilePrimExp compileVar

errorMsgString :: Imp.ErrorMsg Imp.Exp -> CompilerM op s (T.Text, [PyExp])
errorMsgString (Imp.ErrorMsg parts) = do
  let onPart (Imp.ErrorString s) = pure ("%s", String s)
      onPart (Imp.ErrorVal IntType {} x) = ("%d",) <$> compileExp x
      onPart (Imp.ErrorVal FloatType {} x) = ("%f",) <$> compileExp x
      onPart (Imp.ErrorVal Imp.Bool x) = ("%r",) <$> compileExp x
      onPart (Imp.ErrorVal Unit {} x) = ("%r",) <$> compileExp x
  (formatstrs, formatargs) <- mapAndUnzipM onPart parts
  pure (mconcat formatstrs, formatargs)

compileCode :: Imp.Code op -> CompilerM op s ()
compileCode Imp.DebugPrint {} =
  pure ()
compileCode Imp.TracePrint {} =
  pure ()
compileCode (Imp.Op op) =
  join $ asks envOpCompiler <*> pure op
compileCode (Imp.If cond tb fb) = do
  cond' <- compileExp $ Imp.untyped cond
  tb' <- collect $ compileCode tb
  fb' <- collect $ compileCode fb
  stm $ If cond' tb' fb'
compileCode (c1 Imp.:>>: c2) = do
  compileCode c1
  compileCode c2
compileCode (Imp.While cond body) = do
  cond' <- compileExp $ Imp.untyped cond
  body' <- collect $ compileCode body
  stm $ While cond' body'
compileCode (Imp.For i bound body) = do
  bound' <- compileExp bound
  let i' = compileName i
  body' <- collect $ compileCode body
  counter <- prettyString <$> newVName "counter"
  one <- prettyString <$> newVName "one"
  stm $ Assign (Var i') $ simpleCall (compilePrimToNp (Imp.primExpType bound)) [Integer 0]
  stm $ Assign (Var one) $ simpleCall (compilePrimToNp (Imp.primExpType bound)) [Integer 1]
  stm $
    For counter (simpleCall "range" [bound']) $
      body' ++ [AssignOp "+" (Var i') (Var one)]
compileCode (Imp.SetScalar name exp1) =
  stm =<< Assign <$> compileVar name <*> compileExp exp1
compileCode Imp.DeclareMem {} = pure ()
compileCode (Imp.DeclareScalar v _ Unit) = do
  v' <- compileVar v
  stm $ Assign v' $ Var "True"
compileCode Imp.DeclareScalar {} = pure ()
compileCode (Imp.DeclareArray name t vs) = do
  let arr_name = compileName name <> "_arr"
  -- It is important to store the Numpy array in a temporary variable
  -- to prevent it from going "out-of-scope" before calling
  -- unwrapArray (which internally uses the .ctype method); see
  -- https://docs.scipy.org/doc/numpy/reference/generated/numpy.ndarray.ctypes.html
  stm $ Assign (Var arr_name) $ case vs of
    Imp.ArrayValues vs' ->
      Call
        (Var "np.array")
        [ Arg $ List $ map compilePrimValue vs',
          ArgKeyword "dtype" $ Var $ compilePrimToNp t
        ]
    Imp.ArrayZeros n ->
      Call
        (Var "np.zeros")
        [ Arg $ Integer $ fromIntegral n,
          ArgKeyword "dtype" $ Var $ compilePrimToNp t
        ]
  name' <- compileVar name
  stm $ Assign name' $ simpleCall "unwrapArray" [Var arr_name]
compileCode (Imp.Comment s code) = do
  code' <- collect $ compileCode code
  stm $ Comment (T.unpack s) code'
compileCode (Imp.Assert e msg (loc, locs)) = do
  e' <- compileExp e
  (formatstr, formatargs) <- errorMsgString msg
  stm $
    Assert
      e'
      ( BinOp
          "%"
          (String $ "Error: " <> formatstr <> "\n\nBacktrace:\n" <> stacktrace)
          (Tuple formatargs)
      )
  where
    stacktrace = prettyStacktrace 0 $ map locText $ loc : locs
compileCode (Imp.Call dests fname args) = do
  args' <- mapM compileArg args
  dests' <- tupleOrSingle <$> mapM compileVar dests
  let fname'
        | isBuiltInFunction fname = futharkFun (prettyText fname)
        | otherwise = "self." <> futharkFun (prettyText fname)
      call' = simpleCall (T.unpack fname') args'
  -- If the function returns nothing (is called only for side
  -- effects), take care not to assign to an empty tuple.
  stm $
    if null dests
      then Exp call'
      else Assign dests' call'
  where
    compileArg (Imp.MemArg m) = compileVar m
    compileArg (Imp.ExpArg e) = compileExp e
compileCode (Imp.SetMem dest src _) =
  stm =<< Assign <$> compileVar dest <*> compileVar src
compileCode (Imp.Allocate name (Imp.Count (Imp.TPrimExp e)) (Imp.Space space)) =
  join $
    asks envAllocate
      <*> compileVar name
      <*> compileExp e
      <*> pure space
compileCode (Imp.Allocate name (Imp.Count (Imp.TPrimExp e)) _) = do
  e' <- compileExp e
  let allocate' = simpleCall "allocateMem" [e']
  stm =<< Assign <$> compileVar name <*> pure allocate'
compileCode (Imp.Free name _) =
  stm =<< Assign <$> compileVar name <*> pure None
compileCode (Imp.Copy _ dest (Imp.Count destoffset) DefaultSpace src (Imp.Count srcoffset) DefaultSpace (Imp.Count size)) = do
  destoffset' <- compileExp $ Imp.untyped destoffset
  srcoffset' <- compileExp $ Imp.untyped srcoffset
  dest' <- compileVar dest
  src' <- compileVar src
  size' <- compileExp $ Imp.untyped size
  let offset_call1 = simpleCall "addressOffset" [dest', destoffset', Var "ct.c_byte"]
  let offset_call2 = simpleCall "addressOffset" [src', srcoffset', Var "ct.c_byte"]
  stm $ Exp $ simpleCall "ct.memmove" [offset_call1, offset_call2, size']
compileCode (Imp.Copy pt dest (Imp.Count destoffset) destspace src (Imp.Count srcoffset) srcspace (Imp.Count size)) = do
  copy <- asks envCopy
  join $
    copy
      <$> compileVar dest
      <*> compileExp (Imp.untyped destoffset)
      <*> pure destspace
      <*> compileVar src
      <*> compileExp (Imp.untyped srcoffset)
      <*> pure srcspace
      <*> compileExp (Imp.untyped size)
      <*> pure pt
compileCode (Imp.Write _ _ Unit _ _ _) = pure ()
compileCode (Imp.Write dest (Imp.Count idx) elemtype (Imp.Space space) _ elemexp) =
  join $
    asks envWriteScalar
      <*> compileVar dest
      <*> compileExp (Imp.untyped idx)
      <*> pure elemtype
      <*> pure space
      <*> compileExp elemexp
compileCode (Imp.Write dest (Imp.Count idx) elemtype _ _ elemexp) = do
  idx' <- compileExp $ Imp.untyped idx
  elemexp' <- toStorage elemtype <$> compileExp elemexp
  dest' <- compileVar dest
  stm $ Exp $ simpleCall "writeScalarArray" [dest', idx', elemexp']
compileCode (Imp.Read x _ _ Unit _ _) =
  stm =<< Assign <$> compileVar x <*> pure (compilePrimValue UnitValue)
compileCode (Imp.Read x src (Imp.Count iexp) restype (Imp.Space space) _) = do
  x' <- compileVar x
  e <-
    join $
      asks envReadScalar
        <*> compileVar src
        <*> compileExp (Imp.untyped iexp)
        <*> pure restype
        <*> pure space
  stm $ Assign x' e
compileCode (Imp.Read x src (Imp.Count iexp) bt _ _) = do
  x' <- compileVar x
  iexp' <- compileExp $ Imp.untyped iexp
  let bt' = compilePrimType bt
  src' <- compileVar src
  stm $ Assign x' $ fromStorage bt $ simpleCall "indexArray" [src', iexp', Var bt']
compileCode Imp.Skip = pure ()
