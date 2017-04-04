{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Futhark.CodeGen.Backends.GenericPython
  ( compileProg
  , Constructor (..)
  , emptyConstructor

  , compileName
  , compileDim
  , compileExp
  , compileCode
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
  , EntryOutput
  , EntryInput

  , CompilerEnv(..)
  , CompilerState(..)
  , stm
  , stms
  , collect'
  , collect
  , simpleCall

  , compileSizeOfType
  ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Data.Maybe

import qualified Data.Map.Strict as M

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.AST.Attributes.Constants
import Futhark.Representation.AST.Syntax (Space(..))
import qualified Futhark.CodeGen.ImpCode as Imp hiding (dimSizeToExp)
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.CodeGen.Backends.GenericPython.Options
import Futhark.CodeGen.Backends.GenericPython.Definitions
import Futhark.Util.Pretty(pretty)
import Futhark.Util (zEncodeString)
import Futhark.Representation.AST.Attributes (builtInFunctions, isBuiltInFunction)

-- | A substitute expression compiler, tried before the main
-- compilation function.
type OpCompiler op s = op -> CompilerM op s ()

-- | Scatter a scalar to the given memory block with the given index and
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

-- | Construct the Python array being returned from an entry point.
type EntryOutput op s = VName -> Imp.SpaceId ->
                        PrimType -> Imp.Signedness ->
                        [Imp.DimSize] ->
                        CompilerM op s PyExp

-- | Unpack the array being passed to an entry point.
type EntryInput op s = VName -> Imp.MemSize -> Imp.SpaceId ->
                       PrimType -> Imp.Signedness ->
                       [Imp.DimSize] ->
                       PyExp ->
                       CompilerM op s ()


data Operations op s = Operations { opsWriteScalar :: WriteScalar op s
                                  , opsReadScalar :: ReadScalar op s
                                  , opsAllocate :: Allocate op s
                                  , opsCopy :: Copy op s
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
                               , opsCompiler = defCompiler
                               , opsEntryOutput = defEntryOutput
                               , opsEntryInput = defEntryInput
                               }
  where defWriteScalar _ _ _ _ _ =
          fail "Cannot write to non-default memory space because I am dumb"
        defReadScalar _ _ _ _ =
          fail "Cannot read from non-default memory space"
        defAllocate _ _ _ =
          fail "Cannot allocate in non-default memory space"
        defCopy _ _ _ _ _ _ _ _ =
          fail "Cannot copy to or from non-default memory space"
        defCompiler _ =
          fail "The default compiler cannot compile extended operations"
        defEntryOutput _ _ _ _ =
          fail "Cannot return array not in default memory space"
        defEntryInput _ _ _ _ =
          fail "Cannot accept array not in default memory space"

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
  , compUserState :: s
}

newCompilerState :: VNameSource -> s -> CompilerState s
newCompilerState src s = CompilerState { compNameSrc = src
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
         , optionArgument = RequiredArgument
         , optionAction =
           [
             If (Var "runtime_file")
             [Exp $ simpleCall "runtime_file.close" []] []
           , Assign (Var "runtime_file") $
             simpleCall "open" [Var "optarg", StringLiteral "w"]
           ]
         },
  Option { optionLongName = "runs"
         , optionShortName = Just 'r'
         , optionArgument = RequiredArgument
         , optionAction =
           [ Assign (Var "num_runs") $ Var "optarg"
           , Assign (Var "do_warmup_run") $ Constant $ value True
           ]
         },
  Option { optionLongName = "entry-point"
         , optionShortName = Just 'e'
         , optionArgument = RequiredArgument
         , optionAction =
           [ Assign (Var "entry_point") $ Var "optarg" ]
         }
  ]


-- | The class generated by the code generator must have a
-- constructor, although it can be vacuous.
data Constructor = Constructor [String] [PyStmt]

-- | A constructor that takes no arguments and does nothing.
emptyConstructor :: Constructor
emptyConstructor = Constructor ["self"] [Pass]

constructorToFunDef :: Constructor -> PyFunDef
constructorToFunDef (Constructor params body) =
  Def "__init__" params body

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
            [Import "argparse" Nothing] ++
            defines ++
            [Escape pyUtility] ++
            prog')
  where constructor' = constructorToFunDef constructor
        compileProg' = do
          definitions <- mapM compileFunc funs
          case module_name of
            Just name -> do
              entry_points <- mapM compileEntryFun $ filter (Imp.functionEntry . snd) funs
              return [ClassDef $ Class name $ map FunDef $
                      constructor' : definitions ++ entry_points]
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
          Assign (Var "do_warmup_run") (Constant $ value False) :
          Assign (Var "num_runs") (Constant $ value (1::Int32)) :
          Assign (Var "entry_point") (StringLiteral "main") :
          generateOptionParser (standardOptions ++ options)

        selectEntryPoint entry_point_names entry_points =
          [ Assign (Var "entry_points") $
              Dict $ zip (map StringLiteral entry_point_names) entry_points,
            Assign (Var "entry_point_fun") $
              simpleCall "entry_points.get" [Var "entry_point"],
            If (BinOp "==" (Var "entry_point_fun") None)
              [Exp $ simpleCall "sys.exit"
                  [Call (Field
                          (StringLiteral "No entry point '{}'.  Select another with --entry point.  Options are:\n{}")
                          "format")
                    [Arg $ Var "entry_point",

                     Arg $ Call (Field (StringLiteral "\n") "join")
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
compileDim (Imp.ConstSize i) = Constant $ value i
compileDim (Imp.VarSize v) = Var $ compileName v

unpackDim :: PyExp -> Imp.DimSize -> Int32 -> CompilerM op s ()
unpackDim arr_name (Imp.ConstSize c) i = do
  let shape_name = Field arr_name "shape"
  let constant_c = Constant $ value c
  let constant_i = Constant $ value i
  stm $ Assert (BinOp "==" constant_c (Index shape_name $ IdxExp constant_i)) "constant dimension wrong"

unpackDim arr_name (Imp.VarSize var) i = do
  let shape_name = Field arr_name "shape"
  let src = Index shape_name $ IdxExp $ Constant $ value i
  let dest = Var $ compileName var
  let makeNumpy = simpleCall "np.int32" [src]
  stm $ Try [Assert (BinOp "==" dest makeNumpy) "variant dimension wrong"]
        [Catch (Var "NameError") [Assign dest makeNumpy]]

entryPointOutput :: Imp.ExternalValue -> CompilerM op s PyExp
entryPointOutput (Imp.OpaqueValue desc vs) =
  simpleCall "opaque" . (StringLiteral (pretty desc):) <$>
  mapM (entryPointOutput . Imp.TransparentValue) vs
entryPointOutput (Imp.TransparentValue (Imp.ScalarValue bt ept name)) =
  return $ simpleCall tf [Var $ compileName name]
  where tf = compilePrimToExtNp bt ept
entryPointOutput (Imp.TransparentValue (Imp.ArrayValue mem _ Imp.DefaultSpace bt ept dims)) = do
  let cast = Cast (Var $ compileName mem) (compilePrimTypeExt bt ept)
  return $ simpleCall "createArray" [cast, Tuple $ map compileDim dims]
entryPointOutput (Imp.TransparentValue (Imp.ArrayValue mem _ (Imp.Space sid) bt ept dims)) = do
  pack_output <- asks envEntryOutput
  pack_output mem sid bt ept dims

entryPointInput :: Imp.ExternalValue -> PyExp -> CompilerM op s ()
entryPointInput (Imp.OpaqueValue _ vs) e =
  zipWithM_ entryPointInput (map Imp.TransparentValue vs)
  (map (Index (Field e "data") . IdxExp . Constant . value) [(0::Int32)..])

entryPointInput (Imp.TransparentValue (Imp.ScalarValue bt _ name)) e = do
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
  stm $ Assign vname' npcall

entryPointInput (Imp.TransparentValue (Imp.ArrayValue mem memsize Imp.DefaultSpace _ _ dims)) e = do
  zipWithM_ (unpackDim e) dims [0..]
  let dest = Var $ compileName mem
      unwrap_call = simpleCall "unwrapArray" [e]

  case memsize of
    Imp.VarSize sizevar ->
      stm $ Assign (Var $ compileName sizevar) $
      simpleCall "np.int32" [Field e "nbytes"]
    Imp.ConstSize _ ->
      return ()

  stm $ Assign dest unwrap_call

entryPointInput (Imp.TransparentValue (Imp.ArrayValue mem memsize (Imp.Space sid) bt ept dims)) e = do
  unpack_input <- asks envEntryInput
  unpack_input mem memsize sid bt ept dims e

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
valueDescVName (Imp.ArrayValue vname _ _ _ _ _) = vname

readFun :: PrimType -> Imp.Signedness -> String
readFun (FloatType Float32) _ = "read_f32"
readFun (FloatType Float64) _ = "read_f64"
readFun (IntType Int8)  Imp.TypeUnsigned = "read_u8"
readFun (IntType Int16) Imp.TypeUnsigned = "read_u16"
readFun (IntType Int32) Imp.TypeUnsigned = "read_u32"
readFun (IntType Int64) Imp.TypeUnsigned = "read_u64"
readFun (IntType Int8)  Imp.TypeDirect   = "read_i8"
readFun (IntType Int16) Imp.TypeDirect   = "read_i16"
readFun (IntType Int32) Imp.TypeDirect   = "read_i32"
readFun (IntType Int64) Imp.TypeDirect   = "read_i64"
readFun Bool _          = "read_bool"
readFun Cert _          = error "Cert is never used. ReaderElem doesn't handle this"

-- The value returned will be used when reading binary arrays, to indicate what
-- the expected type is
readTypeEnum :: PrimType -> Imp.Signedness -> String
readTypeEnum (IntType Int8)  Imp.TypeUnsigned = "FUTHARK_UINT8"
readTypeEnum (IntType Int16) Imp.TypeUnsigned = "FUTHARK_UINT16"
readTypeEnum (IntType Int32) Imp.TypeUnsigned = "FUTHARK_UINT32"
readTypeEnum (IntType Int64) Imp.TypeUnsigned = "FUTHARK_UINT64"
readTypeEnum (IntType Int8)  Imp.TypeDirect   = "FUTHARK_INT8"
readTypeEnum (IntType Int16) Imp.TypeDirect   = "FUTHARK_INT16"
readTypeEnum (IntType Int32) Imp.TypeDirect   = "FUTHARK_INT32"
readTypeEnum (IntType Int64) Imp.TypeDirect   = "FUTHARK_INT64"
readTypeEnum (FloatType Float32) _ = "FUTHARK_FLOAT32"
readTypeEnum (FloatType Float64) _ = "FUTHARK_FLOAT64"
readTypeEnum Bool _ = "FUTHARK_BOOL"
readTypeEnum Cert _ = error "Cert is never used. readTypeEnum doesn't handle this"

readInput :: Imp.ExternalValue -> PyStmt
readInput (Imp.OpaqueValue desc _) =
  Raise $ simpleCall "Exception"
  [StringLiteral $ "Cannot read argument of type " ++ desc ++ "."]

readInput decl@(Imp.TransparentValue (Imp.ScalarValue bt ept _)) =
  let reader' = readFun bt ept
      stdin = Var "input_stream"
  in Assign (Var $ extValueDescName decl) $ simpleCall reader' [stdin]

-- TODO: If the type identifier of 'Float32' is changed, currently the error
-- messages for reading binary input will not use this new name. This is also a
-- problem for the C runtime system.
readInput decl@(Imp.TransparentValue (Imp.ArrayValue _ _ _ bt ept dims)) =
  let rank' = Var $ show $ length dims
      type_enum = Var $ readTypeEnum bt ept
      ct = Var $ compilePrimType bt
      stdin = Var "input_stream"
  in Assign (Var $ extValueDescName decl) $ simpleCall "read_array"
     [stdin, type_enum, StringLiteral $ pretty bt, rank', ct]

printPrimStm :: PyExp -> PrimType -> Imp.Signedness -> PyStmt
printPrimStm val t ept =
  case (t, ept) of
    (IntType Int8, Imp.TypeUnsigned) -> p "%uu8"
    (IntType Int16, Imp.TypeUnsigned) -> p "%uu16"
    (IntType Int32, Imp.TypeUnsigned) -> p "%uu32"
    (IntType Int64, Imp.TypeUnsigned) -> p "%uu64"
    (IntType Int8, _) -> p "%di8"
    (IntType Int16, _) -> p "%di16"
    (IntType Int32, _) -> p "%di32"
    (IntType Int64, _) -> p "%di64"
    (Bool, _) -> If val
      [Exp $ simpleCall "sys.stdout.write" [StringLiteral "true"]]
      [Exp $ simpleCall "sys.stdout.write" [StringLiteral "false"]]
    (Cert, _) -> Exp $ simpleCall "sys.stdout.write" [StringLiteral "Checked"]
    (FloatType Float32, _) -> p "%.6ff32"
    (FloatType Float64, _) -> p "%.6ff64"
  where p s =
          Exp $ simpleCall "sys.stdout.write"
          [BinOp "%" (StringLiteral s) val]

printStm :: Imp.ValueDesc -> PyExp -> CompilerM op s PyStmt
printStm (Imp.ScalarValue bt ept _) e =
  return $ printPrimStm e bt ept
printStm (Imp.ArrayValue _ _ _ bt ept []) e =
  return $ printPrimStm e bt ept
printStm (Imp.ArrayValue mem memsize space bt ept (outer:shape)) e = do
  v <- newVName "print_elem"
  first <- newVName "print_first"
  let size = simpleCall "np.product" [List $ map compileDim $ outer:shape]
      emptystr = "empty(" ++ ppArrayType bt (length shape) ++ ")"
  printelem <- printStm (Imp.ArrayValue mem memsize space bt ept shape) $ Var $ compileName v
  return $ If (BinOp "==" size (Constant (value (0::Int32))))
    [puts emptystr]
    [Assign (Var $ pretty first) $ Var "True",
     puts "[",
     For (pretty v) e [
        If (simpleCall "not" [Var $ pretty first])
        [puts ", "] [],
        printelem,
        Assign (Var $ pretty first) $ Var "False"
    ],
    puts "]"]
    where ppArrayType :: PrimType -> Int -> String
          ppArrayType t 0 = pretty t
          ppArrayType t n = "[]" ++ ppArrayType t (n-1)

          puts s = Exp $ simpleCall "sys.stdout.write" [StringLiteral s]

printValue :: [(Imp.ExternalValue, PyExp)] -> CompilerM op s [PyStmt]
printValue = fmap concat . mapM (uncurry printValue')
  -- We copy non-host arrays to the host before printing.  This is
  -- done in a hacky way - we assume the value has a .get()-method
  -- that returns an equivalent Numpy array.  This works for PyOpenCL,
  -- but we will probably need yet another plugin mechanism here in
  -- the future.
  where printValue' (Imp.OpaqueValue desc _) _ =
          return [Exp $ simpleCall "sys.stdout.write"
                  [StringLiteral $ "#<opaque " ++ desc ++ ">"]]
        printValue' (Imp.TransparentValue (Imp.ArrayValue mem memsize (Space _) bt ept shape)) e =
          printValue' (Imp.TransparentValue (Imp.ArrayValue mem memsize DefaultSpace bt ept shape)) $
          simpleCall (pretty e ++ ".get") []
        printValue' (Imp.TransparentValue r) e = do
          p <- printStm r e
          return [p, Exp $ simpleCall "sys.stdout.write" [StringLiteral "\n"]]

prepareEntry :: (Name, Imp.Function op)
             -> CompilerM op s
                (String, [String], [PyStmt], [PyStmt], [PyStmt],
                 [(Imp.ExternalValue, PyExp)])
prepareEntry (fname, Imp.Function _ outputs inputs _ results args) = do
  let output_paramNames = map (compileName . Imp.paramName) outputs
      funTuple = tupleOrSingle $ fmap Var output_paramNames

  prepareIn <- collect $ zipWithM_ entryPointInput args $
               map (Var . extValueDescName) args
  (res, prepareOut) <- collect' $ mapM entryPointOutput results

  let inputArgs = map (compileName . Imp.paramName) inputs
      fname' = "self." ++ futharkFun (nameToString fname)
      funCall = simpleCall fname' (fmap Var inputArgs)
      call = [Assign funTuple funCall]

  return (nameToString fname, map extValueDescName args,
          prepareIn, call, prepareOut,
          zip results res)

compileEntryFun :: (Name, Imp.Function op)
                -> CompilerM op s PyFunDef
compileEntryFun entry = do
  (fname', params, prepareIn, body, prepareOut, res) <- prepareEntry entry
  let ret = Return $ tupleOrSingle $ map snd res
  return $ Def fname' ("self" : params) $
    prepareIn ++ body ++ prepareOut ++ [ret]

callEntryFun :: [PyStmt] -> (Name, Imp.Function op)
             -> CompilerM op s (PyFunDef, String, PyExp)
callEntryFun pre_timing entry@(fname, Imp.Function _ _ _ _ _ decl_args) = do
  (_, _, prepareIn, body, _, res) <- prepareEntry entry

  let str_input = map readInput decl_args

      exitcall = [Exp $ simpleCall "sys.exit" [Field (StringLiteral "Assertion.{} failed") "format(e)"]]
      except' = Catch (Var "AssertionError") exitcall
      do_run = body ++ pre_timing
      (do_run_with_timing, close_runtime_file) = addTiming do_run

      -- We ignore overflow errors and the like for executable entry
      -- points.  These are (somewhat) well-defined in Futhark.
      ignore s = ArgKeyword s $ StringLiteral "ignore"
      errstate = Call (Var "np.errstate") $ map ignore ["divide", "over", "under", "invalid"]

      do_warmup_run =
        If (Var "do_warmup_run") do_run []

      do_num_runs =
        For "i" (simpleCall "range" [simpleCall "int" [Var "num_runs"]])
        do_run_with_timing

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
           Exp $ simpleCall "runtime_file.write" [StringLiteral "\n"]]
        toMicroseconds x =
          simpleCall "int" [BinOp "*" x $ Constant $ value (1000000::Int32)]

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

compileSizeOfType :: PrimType -> String
compileSizeOfType t =
  case t of
    IntType Int8 -> "1"
    IntType Int16 -> "2"
    IntType Int32 -> "4"
    IntType Int64 -> "8"
    FloatType Float32 -> "4"
    FloatType Float64 -> "8"
    Bool -> "1"
    Cert -> "1"

compilePrimType :: PrimType -> String
compilePrimType t =
  case t of
    IntType Int8 -> "ct.c_int8"
    IntType Int16 -> "ct.c_int16"
    IntType Int32 -> "ct.c_int32"
    IntType Int64 -> "ct.c_int64"
    FloatType Float32 -> "ct.c_float"
    FloatType Float64 -> "ct.c_double"
    Bool -> "ct.c_bool"
    Cert -> "ct.c_bool"

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
    (Bool, _) -> "ct.c_bool"
    (Cert, _) -> "ct.c_bool"

compilePrimToNp :: Imp.PrimType -> String
compilePrimToNp bt =
  case bt of
    IntType Int8 -> "np.int8"
    IntType Int16 -> "np.int16"
    IntType Int32 -> "np.int32"
    IntType Int64 -> "np.int64"
    FloatType Float32 -> "np.float32"
    FloatType Float64 -> "np.float64"
    Bool -> "np.byte"
    Cert -> "np.byte"

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
    (Bool, _) -> "np.byte"
    (Cert, _) -> "np.byte"

compilePrimValue :: Imp.PrimValue -> PyExp
compilePrimValue (IntValue (Int8Value v)) = Constant $ value v
compilePrimValue (IntValue (Int16Value v)) = Constant $ value v
compilePrimValue (IntValue (Int32Value v)) = Constant $ value v
compilePrimValue (IntValue (Int64Value v)) = Constant $ value v
compilePrimValue (FloatValue (Float32Value v))
  | isInfinite v =
      if v > 0 then Var "np.inf" else Var "-np.inf"
  | isNaN v =
      Var "np.nan"
  | otherwise = Constant $ value v
compilePrimValue (FloatValue (Float64Value v))
  | isInfinite v =
      if v > 0 then Var "np.inf" else Var "-np.inf"
  | isNaN v =
      Var "np.nan"
  | otherwise = Constant $ value v
compilePrimValue (BoolValue v) = simpleCall "bool" [Constant $ BoolValue v]
compilePrimValue Checked = Var "Cert"

compileExp :: Imp.Exp -> CompilerM op s PyExp

compileExp (Imp.ValueExp v) = return $ compilePrimValue v

compileExp (Imp.LeafExp (Imp.ScalarVar vname) _) =
  return $ Var $ compileName vname

compileExp (Imp.LeafExp (Imp.SizeOf t) _) = do
  let t' = compileSizeOfType t
  let readInt = read t' :: Int32
  return $ Constant $ value readInt

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
    _ -> return $ simpleCall (pretty cmp) [x', y']

compileExp (Imp.UnOpExp op exp1) = do
  exp1' <- compileExp exp1
  return $ UnOp (compileUnOp op) exp1'

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
  stm $ Assign (Var i') $ Constant $ IntValue $ intValue it (0::Int)
  stm $ Assign (Var one) $ Constant $ IntValue $ intValue it (1::Int)
  stm $ For counter (simpleCall "range" [bound']) $
    body' ++ [AssignOp "+" (Var i') (Var one)]

compileCode (Imp.SetScalar vname exp1) = do
  let name' = Var $ compileName vname
  exp1' <- compileExp exp1
  stm $ Assign name' exp1'

compileCode Imp.DeclareMem{} = return ()
compileCode Imp.DeclareScalar{} = return ()

compileCode (Imp.Comment s code) = do
  code' <- collect $ compileCode code
  stm $ Comment s code'

compileCode (Imp.Assert e loc) = do
  e' <- compileExp e
  stm $ Assert e' $ locStr loc

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

compileCode (Imp.Scatter dest (Imp.Count idx) elemtype DefaultSpace _ elemexp) = do
  idx' <- compileExp idx
  elemexp' <- compileExp elemexp
  let dest' = Var $ compileName dest
  let elemtype' = compilePrimType elemtype
  let ctype = simpleCall elemtype' [elemexp']
  stm $ Exp $ simpleCall "writeScalarArray" [dest', idx', ctype]

compileCode (Imp.Scatter dest (Imp.Count idx) elemtype (Imp.Space space) _ elemexp) =
  join $ asks envWriteScalar
    <*> pure dest
    <*> compileExp idx
    <*> pure elemtype
    <*> pure space
    <*> compileExp elemexp

compileCode Imp.Skip = return ()
