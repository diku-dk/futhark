{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Futhark.CodeGen.Backends.GenericPython
  ( compileProg
  , compileExp
  , compileCode
  , compilePrimType
  , compilePrimToNp

  , Operations (..)
  , defaultOperations

  , CompilerM (..)
  , OpCompiler
  , OpCompilerResult(..)
  , WriteScalar
  , ReadScalar
  , Allocate
  , Copy

  , CompilerEnv(..)
  , CompilerState(..)
  , stm
  , stms
  , collect'
  , collect

  , compileSizeOfType
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS

import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.AST.Attributes.Constants
import Futhark.Representation.AST.Syntax (Space(..))
import qualified Futhark.CodeGen.ImpCode as Imp hiding (dimSizeToExp)
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.CodeGen.Backends.GenericPython.Options
import Futhark.Util.Pretty(pretty)
import Futhark.Representation.AST.Attributes (builtInFunctions)

-- | A substitute expression compiler, tried before the main
-- compilation function.
type OpCompiler op s = op -> CompilerM op s (OpCompilerResult op)

-- | The result of the substitute expression compiler.
data OpCompilerResult op = CompileCode (Imp.Code op) -- ^ Equivalent to this code.
                         | Done -- ^ Code added via monadic interface.

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

data Operations op s = Operations { opsWriteScalar :: WriteScalar op s
                                  , opsReadScalar :: ReadScalar op s
                                  , opsAllocate :: Allocate op s
                                  , opsCopy :: Copy op s
                                  , opsCompiler :: OpCompiler op s
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
futharkFun s = "futhark_" ++ s

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

timingOption :: Option
timingOption =
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
  }

compileProg :: MonadFreshNames m =>
               Bool
            -> [PyStmt]
            -> [PyStmt]
            -> Operations op s
            -> s
            -> [PyStmt]
            -> [Option]
            -> Imp.Functions op
            -> m String
compileProg as_module imports defines ops userstate pre_timing options prog@(Imp.Functions funs)  = do
  src <- getNameSource
  let (prog', maincall) = runCompilerM prog ops src userstate compileProg'
      (maybe_shebang, maybe_maincall)
        | as_module = ("", [])
        | otherwise = ("#!/usr/bin/env python\n", [maincall])
  return $ maybe_shebang ++
    pretty (PyProg $ imports ++ [Import "argparse" Nothing] ++ defines ++ prog' ++ maybe_maincall)
  where compileProg' = do
          definitions <- mapM compileFunc funs
          if as_module then do
            entry_points <- mapM compileEntryFun $ filter (Imp.functionEntry . snd) funs
            return (map FunDef $ definitions ++ entry_points, Pass)
          else do
            maincall <- case lookup defaultEntryPoint funs of
              Nothing   -> fail "No main function"
              Just func -> callEntryFun pre_timing options (defaultEntryPoint, func)
            return (map FunDef definitions, maincall)

compileFunc :: (Name, Imp.Function op) -> CompilerM op s PyFunDef
compileFunc (fname, Imp.Function _ outputs inputs body _ _) = do
  body' <- collect $ compileCode body
  let inputs' = map (pretty . Imp.paramName) inputs
  let ret = Return $ tupleOrSingle $ compileOutput outputs
  return $ Def (futharkFun . nameToString $ fname) inputs' (body'++[ret])

tupleOrSingle :: [PyExp] -> PyExp
tupleOrSingle [e] = e
tupleOrSingle es = Tuple es

-- | A 'Call' where every argument is a simple 'Arg'.
simpleCall :: String -> [PyExp] -> PyExp
simpleCall fname = Call fname . map Arg

compileDim :: Imp.DimSize -> PyExp
compileDim (Imp.ConstSize i) = Constant $ value i
compileDim (Imp.VarSize v) = Var $ pretty v

unpackDim :: String -> Imp.DimSize -> Int32 -> CompilerM op s ()
unpackDim arr_name (Imp.ConstSize c) i = do
  let shape_name = Var $ arr_name  ++ ".shape"
  let constant_c = Constant $ value c
  let constant_i = Constant $ value i
  stm $ Assert (BinaryOp "==" constant_c (Index shape_name $ IdxExp constant_i)) "shape dimension is incorrect for the constant dimension"

unpackDim arr_name (Imp.VarSize var) i = do
  let shape_name = Var $ arr_name  ++ ".shape"
  let src = Index shape_name $ IdxExp $ Constant $ value i
  let dest = Var $ pretty var
  let makeNumpy = simpleCall "np.int32" [src]
  stm $ Assign dest makeNumpy

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

packArg :: HM.HashMap VName VName
        -> HM.HashMap VName Imp.Space
        -> Imp.ValueDecl
        -> CompilerM op s ()
packArg _ _ decl@(Imp.ScalarValue bt vname) = do
  let vname' = Var $ pretty vname
      npobject = compilePrimToNp bt
      call = simpleCall npobject [Var $ valueDeclName decl]
  stm $ Assign vname' call

packArg memsizes spacemap decl@(Imp.ArrayValue vname bt dims) = do
  let extname = valueDeclName decl
  zipWithM_ (unpackDim extname) dims [0..]
  let src_size = Var $ extname ++ ".nbytes"
      makeNumpy = simpleCall "np.int32" [src_size]
      dest = Var $ pretty vname
      unwrap_call = simpleCall "unwrapArray" [Var extname]

  sizevar <- case HM.lookup vname memsizes of
    Nothing -> error "Param name does not exist in array declarations"
    Just sizevar -> do stm $ Assign (Var $ pretty sizevar) makeNumpy
                       return sizevar

  case HM.lookup vname spacemap of
    Just (Imp.Space space) -> do copy <- asks envCopy
                                 alloc <- asks envAllocate
                                 name <- newVName $ baseString vname <> "_" <> space
                                 alloc name (Var $ pretty sizevar) space
                                 stm $ Assign dest $ Var $ valueDeclName decl
                                 copy
                                   name (Constant $ value (0 :: Int32)) (Imp.Space $ pretty space)
                                   vname (Constant $ value (0 :: Int32)) Imp.DefaultSpace src_size bt
                                 stm $ Assign dest (Var $ pretty name)

    Just Imp.DefaultSpace -> stm $ Assign dest unwrap_call
    Nothing -> error "Space is not set correctly"

unpackOutput :: HM.HashMap VName VName -> HM.HashMap VName Imp.Space -> Imp.ValueDecl
             -> CompilerM op s ()
unpackOutput _ _ Imp.ScalarValue{} =
  return ()

unpackOutput sizeHash spacemap (Imp.ArrayValue vname bt dims) = do
  let cast = Cast (Var $ pretty vname) (compilePrimType bt)
  let funCall = simpleCall "createArray" [cast, Tuple $ map compileDim dims]
  let dest = Var $ pretty vname

  let size = case HM.lookup vname sizeHash of
               Nothing -> error "Couldn't find memparam in size hash"
               Just s -> pretty s


  case HM.lookup vname spacemap of
    Just (Imp.Space space) -> do copy <- asks envCopy
                                 name <- newVName $ baseString vname <> "_" <> space
                                 let name' = Var $ pretty name
                                 let bt'' = compilePrimType bt
                                 let emptyArray = Call "np.empty"
                                                  [Arg $ Tuple $ map compileDim dims,
                                                   ArgKeyword "dtype" (Var bt'')]
                                 stm $ Assign name' emptyArray
                                 copy
                                   name (Constant $ value (0::Int32)) Imp.DefaultSpace
                                   vname (Constant $ value (0::Int32)) (Space $ pretty space) (Var size)
                                   bt
                                 stm $ Assign (Var $ pretty vname) name'
    Just Imp.DefaultSpace -> stm $ Assign dest funCall
    Nothing -> error "Space is not set correctly"

valueDeclName :: Imp.ValueDecl -> String
valueDeclName (Imp.ScalarValue _ vname) = pretty vname ++ "_ext"
valueDeclName (Imp.ArrayValue vname _ _) = pretty vname ++ "_ext"

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
  in Assign (Var $ valueDeclName decl) $ simpleCall reader' [stdin]

readInput decl@(Imp.ArrayValue _ bt dims) =
  let rank' = Var $ show $ length dims
      reader' = Var $ readerElem bt
      bt' = Var $ compilePrimType bt
      stdin = Var "sys.stdin"
  in Assign (Var $ valueDeclName decl) $ simpleCall "read_array" [stdin, reader', rank', bt']

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
          [BinaryOp "%" (StringLiteral s) val]

printStm :: Imp.ValueDecl -> CompilerM op s PyStmt
printStm (Imp.ScalarValue bt name) =
  return $ printPrimStm (Var $ textual name) bt
printStm (Imp.ArrayValue name bt []) =
  return $ printPrimStm (Var $ textual name) bt
printStm (Imp.ArrayValue mem bt (_:shape)) = do
  v <- newVName "print_elem"
  first <- newVName "print_first"
  let size = simpleCall "np.product" [Var $ pretty mem ++ ".shape"]
      emptystr = "empty(" ++ ppArrayType bt (length shape) ++ ")"
  printelem <- printStm $ Imp.ArrayValue v bt shape
  return $ If (BinaryOp "==" size (Constant (value (0::Int32))))
    [puts emptystr]
    [Assign (Var $ pretty first) $ Var "True",
     puts "[",
     For (pretty v) (Var $ pretty mem) [
        If (simpleCall "not" [Var $ pretty first])
        [puts ", "] [],
        printelem,
        Assign (Var $ pretty first) $ Var "False"
    ],
    puts "]"]
    where ppArrayType :: PrimType -> Int -> String
          ppArrayType t 0 = pretty t
          ppArrayType t n = "[" ++ ppArrayType t (n-1) ++ "]"

          puts s = Exp $ simpleCall "sys.stdout.write" [StringLiteral s]

printResult :: [Imp.ValueDecl] -> CompilerM op s [PyStmt]
printResult vs = fmap concat $ forM vs $ \v -> do
  p <- printStm v
  return [p, Exp $ simpleCall "sys.stdout.write" [StringLiteral "\n"]]

prepareEntry :: (Name, Imp.Function op)
             -> CompilerM op s
                (String, [String], [PyStmt], [PyStmt], [PyStmt], [PyExp])
prepareEntry (fname, Imp.Function _ outputs inputs _ decl_outputs decl_args) = do
  let output_paramNames = map (pretty . Imp.paramName) outputs
      funTuple = tupleOrSingle $ fmap Var output_paramNames
      hashSizeInput = hashSizeVars inputs
      hashSizeOutput = hashSizeVars outputs
      hashSpaceInput = hashSpace inputs
      hashSpaceOutput = hashSpace outputs

  prepareIn <- collect $ mapM_ (packArg hashSizeInput hashSpaceInput) decl_args
  prepareOut <- collect $ mapM_ (unpackOutput hashSizeOutput hashSpaceOutput) decl_outputs

  let inputArgs = map (pretty . Imp.paramName) inputs
      funCall = simpleCall (futharkFun . nameToString $ fname) (fmap Var inputArgs)
      call = [Assign funTuple funCall]
      res = map (Var . valueDeclName) decl_outputs

  return (nameToString fname, map valueDeclName decl_args,
          prepareIn, call, prepareOut,
          res)

compileEntryFun :: (Name, Imp.Function op)
                -> CompilerM op s PyFunDef
compileEntryFun entry = do
  (fname', params, prepareIn, body, prepareOut, res) <- prepareEntry entry
  let ret = Return $ tupleOrSingle res
  return $ Def fname' params $
    prepareIn ++ body ++ prepareOut ++ [ret]

callEntryFun :: [PyStmt] -> [Option] -> (Name, Imp.Function op)
             -> CompilerM op s PyStmt
callEntryFun pre_timing options entry@(_, Imp.Function _ _ _ _ decl_outputs decl_args) = do
  (_, _, prepareIn, body, prepareOut, _) <- prepareEntry entry

  let str_input = map readInput decl_args

      exitcall = [Exp $ simpleCall "sys.exit" [Field (StringLiteral "Assertion.{} failed") "format(e)"]]
      except' = Catch (Var "AssertionError") exitcall
      main_with_timing = addTiming $ body ++ pre_timing
      trys = Try main_with_timing [except']

  str_output <- printResult decl_outputs

  return $
    If (BinaryOp "==" (Var "__name__") (StringLiteral "__main__"))
    (parse_options ++ str_input ++ prepareIn ++ [trys] ++ prepareOut ++ str_output)
    []
  where parse_options =
          Assign (Var "runtime_file") None :
          generateOptionParser (timingOption : options)

addTiming :: [PyStmt] -> [PyStmt]
addTiming statements =
    [ Assign (Var "time_start") $ Call "time.time" [] ] ++
    statements ++
    [ Assign (Var "time_end") $ Call "time.time" []
    , If (Var "runtime_file") print_runtime [] ]
  where print_runtime =
          [Exp $ simpleCall "runtime_file.write"
           [simpleCall "str"
            [BinaryOp "-"
             (toMicroseconds (Var "time_end"))
             (toMicroseconds (Var "time_start"))]],
           Exp $ simpleCall "runtime_file.write" [StringLiteral "\n"],
           Exp $ simpleCall "runtime_file.close" []]
        toMicroseconds x =
          simpleCall "int" [BinaryOp "*" x $ Constant $ value (1000000::Int32)]

compileUnOp :: Imp.UnOp -> String
compileUnOp op =
  case op of
    Not -> "not"
    Complement{} -> "~"
    Abs{} -> "abs"
    FAbs{} -> "abs"
    SSignum{} -> "ssignum"
    USignum{} -> "usignum"

compileBinOp :: BinOp -> Imp.Exp -> Imp.Exp -> CompilerM op s PyExp
compileBinOp op x y = do
  x' <- compileExp x
  y' <- compileExp y
  let simple s = return $ BinaryOp s x' y'
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
    Bool -> "bool"
    Cert -> "bool"

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

-- Had to explicitly declare each constant value because memmove
-- typeclashes with python types and numpy types
compileExp (Imp.Constant v) = return $ compilePrimValue v

compileExp (Imp.ScalarVar vname) = return (Var $ pretty vname)

compileExp (Imp.BinOp op exp1 exp2) =
  compileBinOp op exp1 exp2

compileExp (Imp.ConvOp conv x) = do
  x' <- compileExp x
  return $ simpleCall (pretty conv) [x']

compileExp (Imp.CmpOp cmp x y) = do
  x' <- compileExp x
  y' <- compileExp y
  let simple s = return $ BinaryOp s x' y'
  case cmp of
    CmpEq{} -> simple "=="
    FCmpLt{} -> simple "<"
    FCmpLe{} -> simple "<="
    _ -> return $ simpleCall (pretty cmp) [x', y']

compileExp (Imp.UnOp op exp1) = do
  exp1' <- compileExp exp1
  return $ UnOp (compileUnOp op) exp1'

compileExp (Imp.Cond exp1 exp2 exp3) = do
  exp1' <- compileExp exp1
  exp2' <- compileExp exp2
  exp3' <- compileExp exp3
  return $ Cond exp1' exp2' exp3'

compileExp (Imp.SizeOf t) = do
  let t' = compileSizeOfType t
  let readInt = read t' :: Int32
  return $ Constant $ value readInt

compileExp (Imp.Index src (Imp.Count iexp) bt DefaultSpace) = do
  iexp' <- compileExp iexp
  let bt' = compilePrimType bt
  let nptype = compilePrimToNp bt
  return $ simpleCall "indexArray" [Var $ pretty src, iexp', Var bt', Var nptype]

compileExp (Imp.Index src (Imp.Count iexp) restype (Imp.Space space)) =
  join $ asks envReadScalar
    <*> pure src <*> compileExp iexp
    <*> pure restype <*> pure space

compileCode :: Imp.Code op -> CompilerM op s ()

compileCode (Imp.Op op) = do
  opc <- asks envOpCompiler
  res <- opc op
  case res of Done             -> return ()
              CompileCode code -> compileCode code

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

compileCode (Imp.For i bound body) = do
  bound' <- compileExp bound
  let i' = pretty i
  body' <- collect $ compileCode body
  stm $ For i' (simpleCall "range" [bound']) (Assign (Var i') (simpleCall "np.int32" [Var i']) : body')

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
  args' <- mapM compileExp args
  let dests' = tupleOrSingle $ fmap Var (map pretty dests)
  let call' = simpleCall (futharkFun . pretty $ fname) args'
  stm $ Assign dests' call'

compileCode (Imp.SetMem dest src) = do
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

compileCode (Imp.Write dest (Imp.Count idx) elemtype DefaultSpace elemexp) = do
  idx' <- compileExp idx
  elemexp' <- compileExp elemexp
  let dest' = Var $ pretty dest
  let elemtype' = compilePrimType elemtype
  let ctype = simpleCall elemtype' [elemexp']
  stm $ Exp $ simpleCall "writeScalarArray" [dest', idx', ctype]

compileCode (Imp.Write dest (Imp.Count idx) elemtype (Imp.Space space) elemexp) =
  join $ asks envWriteScalar
    <*> pure dest
    <*> compileExp idx
    <*> pure elemtype
    <*> pure space
    <*> compileExp elemexp

compileCode Imp.Skip = return ()
