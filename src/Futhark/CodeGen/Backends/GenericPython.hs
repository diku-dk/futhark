{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Futhark.CodeGen.Backends.GenericPython
  ( compileProg
  , Constructor (..)
  , emptyConstructor

  , compileDim
  , compileExp
  , compileCode
  , compilePrimType
  , compilePrimToNp

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

import qualified Data.HashMap.Lazy as HM

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

-- | Construct the Python array being returned from an entry point.
type EntryOutput op s = VName -> Imp.SpaceId ->
                        PrimType -> [Imp.DimSize] ->
                        CompilerM op s PyExp

-- | Unpack the array being passed to an entry point.
type EntryInput op s = VName -> Imp.MemSize -> Imp.SpaceId ->
                       PrimType -> [Imp.DimSize] ->
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
  , envFtable     :: HM.HashMap Name [Imp.Type]
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
  where ftable = HM.fromList $ map funReturn funs
        funReturn (name, Imp.Function _ outparams _ _ _ _) = (name, paramsTypes outparams)
        builtinFtable = HM.map (map Imp.Scalar . snd) builtInFunctions

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
  where paramType (Imp.MemParam _ size space) = Imp.Mem size space
        paramType (Imp.ScalarParam _ t) = Imp.Scalar t

compileOutput :: [Imp.Param] -> [PyExp]
compileOutput = map (Var . textual . Imp.paramName)

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
              mainfunc <- case lookup defaultEntryPoint funs of
                Nothing   -> fail "No main function"
                Just func -> return func
              let classinst = Assign (Var "self") $ simpleCall "internal" []
              maincall <- callEntryFun pre_timing options (defaultEntryPoint, mainfunc)
              return $ ClassDef (Class "internal" $ map FunDef $
                                 constructor' : definitions) :
                       classinst :
                       maincall

compileFunc :: (Name, Imp.Function op) -> CompilerM op s PyFunDef
compileFunc (fname, Imp.Function _ outputs inputs body _ _) = do
  body' <- collect $ compileCode body
  let inputs' = map (pretty . Imp.paramName) inputs
  let ret = Return $ tupleOrSingle $ compileOutput outputs
  return $ Def (futharkFun . nameToString $ fname) ("self" : inputs') (body'++[ret])

tupleOrSingle :: [PyExp] -> PyExp
tupleOrSingle [e] = e
tupleOrSingle es = Tuple es

-- | A 'Call' where every argument is a simple 'Arg'.
simpleCall :: String -> [PyExp] -> PyExp
simpleCall fname = Call fname . map Arg

compileDim :: Imp.DimSize -> PyExp
compileDim (Imp.ConstSize i) = Constant $ value i
compileDim (Imp.VarSize v) = Var $ pretty v

unpackDim :: PyExp -> Imp.DimSize -> Int32 -> CompilerM op s ()
unpackDim arr_name (Imp.ConstSize c) i = do
  let shape_name = Field arr_name "shape"
  let constant_c = Constant $ value c
  let constant_i = Constant $ value i
  stm $ Assert (BinOp "==" constant_c (Index shape_name $ IdxExp constant_i)) "constant dimension wrong"

unpackDim arr_name (Imp.VarSize var) i = do
  let shape_name = Field arr_name "shape"
  let src = Index shape_name $ IdxExp $ Constant $ value i
  let dest = Var $ pretty var
  let makeNumpy = simpleCall "np.int32" [src]
  stm $ Try [Assert (BinOp "==" dest makeNumpy) "variant dimension wrong"]
        [Catch (Var "NameError") [Assign dest makeNumpy]]

hashSizeVars :: [Imp.Param] -> HM.HashMap VName VName
hashSizeVars = mconcat . map hashSizeVars'
  where hashSizeVars' (Imp.MemParam parname (Imp.VarSize memsizename) _) =
          HM.singleton parname memsizename
        hashSizeVars' _ =
          HM.empty

hashSpace :: [Imp.Param] -> HM.HashMap VName Space
hashSpace = mconcat . map hashSpace'
  where hashSpace' (Imp.MemParam parname _ space) =
          HM.singleton parname space
        hashSpace' _ =
          HM.empty

-- | A description of a value returned by an entry point.
data EntryPointValue =
  -- | Return a scalar of the given type stored in the given variable.
    ScalarValue PrimType VName
  -- | Return an array, given memory block, size, space, and
  -- dimensions of the array.
  | ArrayValue VName Imp.MemSize Space PrimType [Imp.DimSize]

createValue :: HM.HashMap VName VName
             -> HM.HashMap VName Imp.Space -> Imp.ValueDecl
             -> CompilerM op s EntryPointValue
createValue _ _ (Imp.ScalarValue t name) =
  return $ ScalarValue t name
createValue sizes spaces (Imp.ArrayValue mem bt dims) = do
  size <- maybe noMemSize return $ HM.lookup mem sizes
  space <- maybe noMemSpace return $ HM.lookup mem spaces
  return $ ArrayValue mem (Imp.VarSize size) space bt dims
  where noMemSpace = fail $ "createValue: could not find space of memory block " ++ pretty mem
        noMemSize = fail $ "createValue: could not find size of memory block " ++ pretty mem

entryPointOutput :: EntryPointValue -> CompilerM op s PyExp
entryPointOutput (ScalarValue _ name) =
  return $ Var $ pretty name
entryPointOutput (ArrayValue mem _ Imp.DefaultSpace bt dims) = do
  let cast = Cast (Var $ pretty mem) (compilePrimType bt)
  return $ simpleCall "createArray" [cast, Tuple $ map compileDim dims]
entryPointOutput (ArrayValue mem _ (Imp.Space sid) bt dims) = do
  pack_output <- asks envEntryOutput
  pack_output mem sid bt dims

entryPointInput :: EntryPointValue -> PyExp -> CompilerM op s ()
entryPointInput (ScalarValue bt name) e = do
  let vname' = Var $ pretty name
      npobject = compilePrimToNp bt
      call = simpleCall npobject [e]
  stm $ Assign vname' call
entryPointInput (ArrayValue mem memsize Imp.DefaultSpace _ dims) e = do
  zipWithM_ (unpackDim e) dims [0..]
  let dest = Var $ pretty mem
      unwrap_call = simpleCall "unwrapArray" [e]

  case memsize of
    Imp.VarSize sizevar ->
      stm $ Assign (Var $ pretty sizevar) $
      simpleCall "np.int32" [Field e "nbytes"]
    Imp.ConstSize _ ->
      return ()

  stm $ Assign dest unwrap_call

entryPointInput (ArrayValue mem memsize (Imp.Space sid) bt dims) e = do
  unpack_input <- asks envEntryInput
  unpack_input mem memsize sid bt dims e

extValueDeclName :: Imp.ValueDecl -> String
extValueDeclName = extName . valueDeclName

extName :: String -> String
extName = (++"_ext")

valueDeclName :: Imp.ValueDecl -> String
valueDeclName (Imp.ScalarValue _ vname) = pretty vname
valueDeclName (Imp.ArrayValue vname _ _) = pretty vname

readerElem :: PrimType -> String
readerElem bt = case bt of
  FloatType Float32 -> "read_float"
  FloatType Float64 -> "read_double_signed"
  IntType{}         -> "read_int"
  Bool              -> "read_bool"
  Cert              -> error "Cert is never used. ReaderElem doesn't handle this"

readInput :: Imp.ValueDecl -> PyStmt
readInput decl@(Imp.ScalarValue bt _) =
  let reader' = readerElem bt
      stdin = Var "sys.stdin"
  in Assign (Var $ extValueDeclName decl) $ simpleCall reader' [stdin]

readInput decl@(Imp.ArrayValue _ bt dims) =
  let rank' = Var $ show $ length dims
      reader' = Var $ readerElem bt
      bt' = Var $ compilePrimType bt
      stdin = Var "sys.stdin"
  in Assign (Var $ extValueDeclName decl) $ simpleCall "read_array"
     [stdin, reader', StringLiteral $ pretty bt, rank', bt']

printPrimStm :: PyExp -> PrimType -> PyStmt
printPrimStm val t =
  case t of
    IntType Int8 -> p "%di8"
    IntType Int16 -> p "%di16"
    IntType Int32 -> p "%di32"
    IntType Int64 -> p "%di64"
    Bool -> If val
      [Exp $ simpleCall "sys.stdout.write" [StringLiteral "True"]]
      [Exp $ simpleCall "sys.stdout.write" [StringLiteral "False"]]
    Cert -> Exp $ simpleCall "sys.stdout.write" [StringLiteral "Checked"]
    FloatType Float32 -> p "%.6ff32"
    FloatType Float64 -> p "%.6ff64"
  where p s =
          Exp $ simpleCall "sys.stdout.write"
          [BinOp "%" (StringLiteral s) val]

printStm :: EntryPointValue -> PyExp -> CompilerM op s PyStmt
printStm (ScalarValue bt _) e =
  return $ printPrimStm e bt
printStm (ArrayValue _ _ _ bt []) e =
  return $ printPrimStm e bt
printStm (ArrayValue mem memsize space bt (outer:shape)) e = do
  v <- newVName "print_elem"
  first <- newVName "print_first"
  let size = simpleCall "np.product" [List $ map compileDim $ outer:shape]
      emptystr = "empty(" ++ ppArrayType bt (length shape) ++ ")"
  printelem <- printStm (ArrayValue mem memsize space bt shape) $ Var $ pretty v
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

printValue :: [(EntryPointValue, PyExp)] -> CompilerM op s [PyStmt]
printValue = fmap concat . mapM (uncurry printValue')
  -- We copy non-host arrays to the host before printing.  This is
  -- done in a hacky way - we assume the value has a .get()-method
  -- that returns an equivalent Numpy array.  This works for PyOpenCL,
  -- but we will probably need yet another plugin mechanism here in
  -- the future.
  where printValue' (ArrayValue mem memsize (Space _) bt shape) e =
          printValue' (ArrayValue mem memsize DefaultSpace bt shape) $
          simpleCall (pretty e ++ ".get") []
        printValue' r e = do
          p <- printStm r e
          return [p, Exp $ simpleCall "sys.stdout.write" [StringLiteral "\n"]]

prepareEntry :: (Name, Imp.Function op)
             -> CompilerM op s
                (String, [String], [PyStmt], [PyStmt], [PyStmt],
                 [(EntryPointValue, PyExp)])
prepareEntry (fname, Imp.Function _ outputs inputs _ decl_outputs decl_args) = do
  let output_paramNames = map (pretty . Imp.paramName) outputs
      funTuple = tupleOrSingle $ fmap Var output_paramNames
      hashSizeInput = hashSizeVars inputs
      hashSizeOutput = hashSizeVars outputs
      hashSpaceInput = hashSpace inputs
      hashSpaceOutput = hashSpace outputs

  args <- mapM (createValue hashSizeInput hashSpaceInput) decl_args
  prepareIn <- collect $ zipWithM_ entryPointInput args $
               map (Var . extValueDeclName) decl_args
  results <- mapM (createValue hashSizeOutput hashSpaceOutput) decl_outputs
  (res, prepareOut) <- collect' $ mapM entryPointOutput results

  let inputArgs = map (pretty . Imp.paramName) inputs
      fname' = "self." ++ futharkFun (nameToString fname)
      funCall = simpleCall fname' (fmap Var inputArgs)
      call = [Assign funTuple funCall]

  return (nameToString fname, map extValueDeclName decl_args,
          prepareIn, call, prepareOut,
          zip results res)

compileEntryFun :: (Name, Imp.Function op)
                -> CompilerM op s PyFunDef
compileEntryFun entry = do
  (fname', params, prepareIn, body, prepareOut, res) <- prepareEntry entry
  let ret = Return $ tupleOrSingle $ map snd res
  return $ Def fname' ("self" : params) $
    prepareIn ++ body ++ prepareOut ++ [ret]

callEntryFun :: [PyStmt] -> [Option] -> (Name, Imp.Function op)
             -> CompilerM op s [PyStmt]
callEntryFun pre_timing options entry@(_, Imp.Function _ _ _ _ _ decl_args) = do
  (_, _, prepareIn, body, _, res) <- prepareEntry entry

  let str_input = map readInput decl_args

      exitcall = [Exp $ simpleCall "sys.exit" [Field (StringLiteral "Assertion.{} failed") "format(e)"]]
      except' = Catch (Var "AssertionError") exitcall
      do_run = body ++ pre_timing
      (do_run_with_timing, close_runtime_file) = addTiming do_run

      do_warmup_run =
        If (Var "do_warmup_run") do_run []

      do_num_runs =
        For "i" (simpleCall "range" [simpleCall "int" [Var "num_runs"]])
        do_run_with_timing

  str_output <- printValue res

  return $ parse_options ++ str_input ++ prepareIn ++
    [Try [do_warmup_run, do_num_runs] [except']] ++
    [close_runtime_file] ++
    str_output
  where parse_options =
          Assign (Var "runtime_file") None :
          Assign (Var "do_warmup_run") (Constant $ value False) :
          Assign (Var "num_runs") (Constant $ value (1::Int32)) :
          generateOptionParser (standardOptions ++ options)

addTiming :: [PyStmt] -> ([PyStmt], PyStmt)
addTiming statements =
  ([ Assign (Var "time_start") $ Call "time.time" [] ] ++
   statements ++
   [ Assign (Var "time_end") $ Call "time.time" []
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

compilePrimValue :: Imp.PrimValue -> PyExp
compilePrimValue (IntValue (Int8Value v)) = Constant $ value v
compilePrimValue (IntValue (Int16Value v)) = Constant $ value v
compilePrimValue (IntValue (Int32Value v)) = Constant $ value v
compilePrimValue (IntValue (Int64Value v)) = Constant $ value v
compilePrimValue (FloatValue (Float32Value v)) = Constant $ value v
compilePrimValue (FloatValue (Float64Value v)) = Constant $ value v
compilePrimValue (BoolValue v) = simpleCall "bool" [Constant $ BoolValue v]
compilePrimValue Checked = Var "Cert"

compileExp :: Imp.Exp -> CompilerM op s PyExp

compileExp (Imp.ValueExp v) = return $ compilePrimValue v

compileExp (Imp.LeafExp (Imp.ScalarVar vname) _) =
  return $ Var $ pretty vname

compileExp (Imp.LeafExp (Imp.SizeOf t) _) = do
  let t' = compileSizeOfType t
  let readInt = read t' :: Int32
  return $ Constant $ value readInt

compileExp (Imp.LeafExp (Imp.Index src (Imp.Count iexp) bt DefaultSpace _) _) = do
  iexp' <- compileExp iexp
  let bt' = compilePrimType bt
  let nptype = compilePrimToNp bt
  return $ simpleCall "indexArray" [Var $ pretty src, iexp', Var bt', Var nptype]

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
  let i' = pretty i
  body' <- collect $ compileCode body
  counter <- pretty <$> newVName "counter"
  one <- pretty <$> newVName "one"
  stm $ Assign (Var i') $ Constant $ IntValue $ intValue it (0::Int)
  stm $ Assign (Var one) $ Constant $ IntValue $ intValue it (1::Int)
  stm $ For counter (simpleCall "range" [bound']) $
    body' ++ [AssignOp "+" (Var i') (Var one)]

compileCode (Imp.SetScalar vname exp1) = do
  let name' = Var $ pretty vname
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
  let dests' = tupleOrSingle $ fmap Var (map pretty dests)
      fname'
        | isBuiltInFunction fname = futharkFun (pretty  fname)
        | otherwise               = "self." ++ futharkFun (pretty  fname)
      call' = simpleCall fname' args'
  stm $ Assign dests' call'
  where compileArg (Imp.MemArg m) = return $ Var $ pretty m
        compileArg (Imp.ExpArg e) = compileExp e

compileCode (Imp.SetMem dest src _) = do
  let src' = Var (pretty src)
  let dest' = Var (pretty dest)
  stm $ Assign dest' src'

compileCode (Imp.Allocate name (Imp.Count e) DefaultSpace) = do
  e' <- compileExp e
  let allocate' = simpleCall "allocateMem" [e']
  let name' = Var (pretty name)
  stm $ Assign name' allocate'

compileCode (Imp.Allocate name (Imp.Count e) (Imp.Space space)) =
  join $ asks envAllocate
    <*> pure name
    <*> compileExp e
    <*> pure space

compileCode (Imp.Copy dest (Imp.Count destoffset) DefaultSpace src (Imp.Count srcoffset) DefaultSpace (Imp.Count size)) = do
  destoffset' <- compileExp destoffset
  srcoffset' <- compileExp srcoffset
  let dest' = Var (pretty dest)
  let src' = Var (pretty src)
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
  let dest' = Var $ pretty dest
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
