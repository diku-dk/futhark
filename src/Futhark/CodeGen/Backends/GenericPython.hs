{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Futhark.CodeGen.Backends.GenericPython
  ( compileProg
  , compileExp
  , compileCode
  , compileBasicType
  , compileBasicToNp

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
  , asscalar

  , compileSizeOfType
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS

import NeatInterpolation()

import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.AST.Syntax (BinOp (..), Space(..))

import qualified Futhark.CodeGen.ImpCode as Imp hiding (dimSizeToExp)
--import qualified Futhark.CodeGen.ImpCode.Sequential as Imp

import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.Util.Pretty(pretty)


-- | A substitute expression compiler, tried before the main
-- compilation function.
type OpCompiler op s = op -> CompilerM op s (OpCompilerResult op)

-- | The result of the substitute expression compiler.
data OpCompilerResult op = CompileCode (Imp.Code op) -- ^ Equivalent to this code.
                         | Done -- ^ Code added via monadic interface.

-- | Write a scalar to the given memory block with the given index and
-- in the given memory space.
type WriteScalar op s = VName -> PyExp -> BasicType -> Imp.SpaceId -> PyExp
                        -> CompilerM op s ()

-- | Read a scalar from the given memory block with the given index and
-- in the given memory space.
type ReadScalar op s = VName -> PyExp -> BasicType -> Imp.SpaceId
                       -> CompilerM op s PyExp

-- | Allocate a memory block of the given size in the given memory
-- space, saving a reference in the given variable name.
type Allocate op s = VName -> PyExp -> Imp.SpaceId
                     -> CompilerM op s ()

-- | Copy from one memory block to another.
type Copy op s = VName -> PyExp -> Imp.Space ->
                 VName -> PyExp -> Imp.Space ->
                 PyExp -> BasicType ->
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
  , envTimeit     :: Bool
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

newCompilerEnv :: Imp.Functions op -> Operations op s -> Bool -> CompilerEnv op s
newCompilerEnv (Imp.Functions funs) ops timeit =
  CompilerEnv { envOperations = ops
              , envFtable = ftable <> builtinFtable
              , envTimeit = timeit }
  where ftable = HM.fromList $ map funReturn funs
        funReturn (name, Imp.Function outparams _ _ _ _) = (name, paramsTypes outparams)
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

instrumentation :: [PyStmt] -> String -> CompilerM op s ()
instrumentation statements s = do
  check <- asks envTimeit
  if check
  then do
    time_start <- newVName "time_start"
    time_end <- newVName "time_end"
    time_diff <- newVName "time_diff"
    let time_start' = Var $ pretty time_start
    let time_end'   = Var $ pretty time_end
    let time_diff'  = Var $ pretty time_diff
    stm $ Assign time_start' (Call "time.time" [])
    stms statements
    stm $ Assign time_end' (Call "time.time" [])
    stm $ Assign time_diff' (BinaryOp "-" time_end' time_start')
    stm $ Exp $ Call "print" [StringLiteral s, time_diff']
  else
    stms statements

--we replace the entry points with appended _
replaceFuncName :: String -> PyFunc -> PyFunc
replaceFuncName key funs@(PyFunc str args body)  = if str == key
  then PyFunc ('_':str) args body
  else funs
replaceFuncName _ _ = error "This should only replace PyFuncs"

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
             -> Bool
             -> a
runCompilerM prog ops src userstate (CompilerM m) timeit =
  fst $ evalRWS m (newCompilerEnv prog ops timeit) (newCompilerState src userstate)

compileProg :: MonadFreshNames m =>
               Bool
            -> [PyImport]
            -> [PyDefinition]
            -> Operations op s
            -> s
            -> Imp.Functions op
                        -> m String
compileProg timeit imports defines ops userstate prog@(Imp.Functions funs)  = do
  src <- getNameSource
  let prog' = runCompilerM prog ops src userstate compileProg' timeit
  return $ pretty (PyProg prog' imports defines)
  where compileProg' = do
          definitions <- mapM compileFunc funs
          let mainname = nameFromString "main"
          main <- case lookup mainname funs of
                    Nothing   -> fail "No main function"
                    Just func -> compileEntryFun (mainname, func)

          let renamed = map (replaceFuncName "futhark_main") definitions

          return (renamed ++ main) --not sure if better method, but the point is that there will be more entry points, not just the main in the future.

compileFunc :: (Name, Imp.Function op) -> CompilerM op s PyFunc
compileFunc (fname, Imp.Function outputs inputs body _ _) = do
  body' <- collect $ compileCode body
  let inputs' = map (pretty . Imp.paramName) inputs
  let ret = Return $ tupleOrSingle $ compileOutput outputs
  return $ PyFunc (futharkFun . nameToString $ fname) inputs' (body'++[ret])

tupleOrSingle :: [PyExp] -> PyExp
tupleOrSingle [e] = e
tupleOrSingle es = Tuple es

compileDim :: Imp.DimSize -> PyExp
compileDim (Imp.ConstSize i) = Constant $ IntVal i
compileDim (Imp.VarSize v) = Var $ pretty v

unpackDim :: VName -> Imp.DimSize -> Int32 -> CompilerM op s ()
unpackDim arr_name (Imp.ConstSize c) i = do
  let shape_name = Var $ pretty arr_name  ++ ".shape"
  let constant_c = Constant $ IntVal c
  let constant_i = Constant $ IntVal i
  stm $ Assert (BinaryOp "==" constant_c (Index shape_name $ IdxExp constant_i)) "shape dimension is incorrect for the constant dimension"

unpackDim arr_name (Imp.VarSize var) i = do
  let shape_name = Var $ pretty arr_name  ++ ".shape"
  let src = Index shape_name $ IdxExp $ Constant $ IntVal i
  let dest = Var $ pretty var
  let makeNumpy = Call "int32" [src]
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
packArg _ _ (Imp.ScalarValue bt vname) = do
  let vname' = Var $ pretty vname
  let npobject = compileBasicToNp bt
  let call = Call npobject [vname']
  stm $ Assign vname' call

packArg memsizes spacemap (Imp.ArrayValue vname bt dims) = do
  zipWithM_ (unpackDim vname) dims [0..]
  let src_size = Var $ pretty vname ++ ".nbytes"
  let makeNumpy = Call "int32" [src_size]
  let src_data = Var $ pretty vname
  let unwrap_call = Call "unwrapArray" [Var $ pretty vname]

  sizevar <- case HM.lookup vname memsizes of
    Nothing -> error "Param name does not exist in array declarations"
    Just sizevar -> do stm $ Assign (Var $ pretty sizevar) makeNumpy
                       return sizevar

  case HM.lookup vname spacemap of
    Just (Imp.Space space) -> do copy <- asks envCopy
                                 alloc <- asks envAllocate
                                 name <- newVName $ baseString vname <> "_" <> space
                                 alloc name (Var $ pretty sizevar) space
                                 copy name (Constant $ IntVal 0) (Imp.Space $ pretty space) vname (Constant $ IntVal 0) Imp.DefaultSpace src_size bt
                                 stm $ Assign src_data (Var $ pretty name)

    Just Imp.DefaultSpace -> stm $ Assign src_data unwrap_call
    Nothing -> error "Space is not set correctly"

unpackOutput :: HM.HashMap VName VName -> HM.HashMap VName Imp.Space -> Imp.ValueDecl -> CompilerM op s ()
unpackOutput _ _ (Imp.ScalarValue _ vname) = do
  let vname' = Var $ pretty vname
  let newbt = asscalar vname'
  stm $ Assign vname' newbt

unpackOutput sizeHash spacemap (Imp.ArrayValue vname bt dims) = do
  let cast = Cast (Var $ pretty vname) (compileBasicType bt)
  let funCall = Call "createArray" [cast, Tuple $ map compileDim dims]
  let dest = Var $ pretty vname

  let size = case HM.lookup vname sizeHash of
               Nothing -> error "Couldn't find memparam in size hash"
               Just s -> pretty s


  case HM.lookup vname spacemap of
    Just (Imp.Space space) -> do copy <- asks envCopy
                                 name <- newVName $ baseString vname <> "_" <> space
                                 let name' = Var $ pretty name
                                 let bt'' = compileBasicType bt
                                 let emptyArray = Call "empty" [Tuple $ map compileDim dims, ParamAssign (Var "dtype") (Var bt'')]
                                 stm $ Assign name' emptyArray
                                 copy name (Constant $ IntVal 0) Imp.DefaultSpace vname (Constant $ IntVal 0) (Space $ pretty space) (Var size) bt
                                 stm $ Assign (Var $ pretty vname) name'
    Just Imp.DefaultSpace -> stm $ Assign dest funCall
    Nothing -> error "Space is not set correctly"

valueDeclName :: Imp.ValueDecl -> String
valueDeclName (Imp.ScalarValue _ vname) = pretty vname
valueDeclName (Imp.ArrayValue vname _ _) = pretty vname

readerElem :: BasicType -> String
readerElem bt = case bt of
                Float32 -> "read_float"
                Float64 -> "read_double_signed"
                Int   -> "read_int"
                Bool    -> "read_bool"
                Char    -> "read_char"
                Cert  -> error "Cert is never used. ReaderElem doesn't handle this"


--since all constants are numpy types, we would sometimes like to use python types, and this function allows us to convert to python.
asscalar :: PyExp -> PyExp
asscalar (Constant v) = Constant v
asscalar (Call "int32" [Constant v]) = Constant v
asscalar (Call "float32" [Constant v]) = Constant v
asscalar (Call "float64" [Constant v]) = Constant v
asscalar (Call "bool_" [Constant v]) = Constant v
asscalar (Call "uint8" [Constant v]) = Constant v
asscalar e = Call "asscalar" [e]

readInput :: Imp.ValueDecl -> PyStmt
readInput (Imp.ScalarValue bt vname) =
  let name = Var $ pretty vname
      reader' = readerElem bt
      stdin = Var "sys.stdin"
  in Assign name $ Call reader' [stdin]

readInput (Imp.ArrayValue vname bt dims) =
  let vname' = Var $ pretty vname
      rank' = Var $ show $ length dims
      reader' = Var $ readerElem bt
      bt' = Var $ compileBasicType bt
      stdin = Var "sys.stdin"
  in Assign vname' $ Call "read_array" [stdin, reader', rank', bt']

writeOutput :: Imp.ValueDecl -> PyStmt
writeOutput (Imp.ScalarValue bt vname) =
  let name = Var $ pretty vname
  in case bt of
    Char -> Exp $ Call "print" [Field name ".decode()"]
    _ -> Exp $ Call "print" [name]

writeOutput (Imp.ArrayValue vname bt _) =
  let name = Var $ pretty vname
      bt' = StringLiteral $ compileBasicType bt
      stdout = Var "sys.stdout"
  in case bt of
    Char -> Exp $ Call "write_chars" [stdout, name]
    _ -> Exp $ Call "write_array" [stdout, name, bt']

compileEntryFun :: (Name, Imp.Function op) -> CompilerM op s [PyFunc]
compileEntryFun (fname, Imp.Function outputs inputs _ decl_outputs decl_args) = do
  let output_paramNames = map (pretty . Imp.paramName) outputs
  let funName = pretty fname
  let funTuple = tupleOrSingle $ fmap Var output_paramNames
  let ret = Return $ tupleOrSingle $ map (Var . valueDeclName) decl_outputs
  let hashSizeInput = hashSizeVars inputs
  let hashSizeOutput = hashSizeVars outputs
  let hashSpaceInput = hashSpace inputs
  let hashSpaceOutput = hashSpace outputs

  prepareIn <- collect $ mapM_ (packArg hashSizeInput hashSpaceInput) decl_args
  prepareOut <- collect $ mapM_ (unpackOutput hashSizeOutput hashSpaceOutput) decl_outputs

  let inputArgs = map (pretty . Imp.paramName) inputs
  let funCall = Call ('_' : (futharkFun . pretty $ fname)) (fmap Var inputArgs)
  let body' = prepareIn ++ [Assign funTuple funCall] ++ prepareOut
  let str_input = map readInput decl_args
  let str_output = map writeOutput decl_outputs
  let decl_output_names = tupleOrSingle $ fmap Var (map valueDeclName decl_outputs)
  let decl_input_names = fmap Var (map valueDeclName decl_args)

  let callmain = Call "main" decl_input_names
  let exitcall = [Exp $ Call "sys.exit" [Field (StringLiteral "Assertion.{} failed") "format(e)"]]
  let except' = Catch (Var "AssertionError") exitcall
  instrumentations <- collect $ instrumentation [Assign decl_output_names callmain] "main took so long: "
  let trys = Try instrumentations [except']
  let iff = If (BinaryOp "==" (Var "__name__") (StringLiteral "__main__"))
            (str_input ++ [trys] ++ str_output)
            [Pass]

  return [PyFunc funName (map valueDeclName decl_args) (body'++[ret]),
          PyMainTest (pretty iff)]

compileUnOp :: Imp.UnOp -> String
compileUnOp op =
  case op of
    Imp.Not -> "not"
    Imp.Complement -> "~"
    Imp.Negate -> "-"
    Imp.Abs -> "abs"
    Imp.Signum -> "sign" -- python does not implement sign, so we use numpy for this, and we have to use numpy for pyopencl anyway.

compileBinOp :: BinOp -> Imp.Exp -> Imp.Exp -> CompilerM op s PyExp
compileBinOp op x y = do
  x' <- compileExp x
  y' <- compileExp y
  let simple s = return $ BinaryOp s x' y'
  case op of
    Plus -> simple "+"
    Minus -> simple "-"
    Div -> simple "//"
    Times -> simple "*"
    Equal -> simple "=="
    Mod -> simple "%"
    ShiftR -> simple ">>"
    ShiftL -> simple "<<"
    Band -> simple "&"
    Xor -> simple "^"
    Bor -> simple "|"
    LogAnd -> simple "and"
    LogOr -> simple "or"
    Less -> simple "<"
    Leq -> simple "<="
    Pow -> simple "**"
    Rem -> return $ Call "fmod" [x', y']
    Quot -> let toFloat1 = Call "float" [x']
                toFloat2 = Call "float" [y']
            in return $ Call "int32" [BinaryOp "/" toFloat1 toFloat2]
    FloatDiv -> return $ BinaryOp "/" x' y'

compileSizeOfType :: BasicType -> String
compileSizeOfType t =
  case t of
    Int -> "4"
    Char -> "1"
    Float32 -> "4"
    Float64 -> "8"
    Bool -> "1"
    Cert -> "1"

compileBasicType :: BasicType -> String
compileBasicType t =
  case t of
    Int -> "c_int"
    Char -> "c_char"
    Float32 -> "c_float"
    Float64 -> "c_double"
    Bool -> "c_bool"
    Cert -> "c_byte"

compileBasicToNp :: Imp.BasicType -> String
compileBasicToNp bt =
  case bt of
    Int -> "int32"
    Char -> "uint8"
    Float32 -> "float32"
    Float64 -> "float64"
    Bool -> "bool_"
    Cert -> "int8"

compileExp :: Imp.Exp -> CompilerM op s PyExp

-- Had to explicitly declare each constant value because memmove
-- typeclashes with python types and numpy types
compileExp (Imp.Constant (IntVal v)) = return $ Call "int32" [Constant $ IntVal v]
compileExp (Imp.Constant (Float32Val v)) = return $ Call "float32" [Constant $ Float32Val v]
compileExp (Imp.Constant (Float64Val v)) = return $ Call "float64" [Constant $ Float64Val v]
compileExp (Imp.Constant (LogVal v)) = return $ Call "bool_" [Constant $ LogVal v]
compileExp (Imp.Constant (CharVal v)) = return $ Constant $ CharVal v
compileExp (Imp.Constant Checked) = return $ Var "Cert"

compileExp (Imp.ScalarVar vname) = return (Var $ pretty vname)

compileExp (Imp.BinOp op exp1 exp2) =
  compileBinOp op exp1 exp2

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
  return $ Constant $ IntVal readInt

compileExp (Imp.Index src (Imp.Count iexp) bt DefaultSpace) = do
  iexp' <- compileExp iexp
  let bt' = compileBasicType bt
  let nptype = compileBasicToNp bt
  return $ Call "indexArray" [Var $ pretty src, iexp', Var bt', Var nptype]

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
  stm $ For i' bound' (Assign (Var i') (Call "int32" [Var i']) : body')

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
  let call' = Call (futharkFun . pretty $ fname) args'
  stm $ Assign dests' call'

compileCode (Imp.SetMem dest src) = do
  let src' = Var (pretty src)
  let dest' = Var (pretty dest)
  stm $ Assign dest' src'

compileCode (Imp.Allocate name (Imp.Count e) DefaultSpace) = do
  e' <- compileExp e
  let allocate' = Call "allocateMem" [e']
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
  let offset_call1 = Call "addressOffset" [dest', destoffset', Var "c_byte"]
  let offset_call2 = Call "addressOffset" [src', srcoffset', Var "c_byte"]
  stm $ Exp $ Call "memmove" [offset_call1, offset_call2, size']

compileCode (Imp.Copy dest (Imp.Count destoffset) destspace src (Imp.Count srcoffset) srcspace (Imp.Count size)) = do
  copy <- asks envCopy
  join $ copy
    <$> pure dest <*> compileExp destoffset <*> pure destspace
    <*> pure src <*> compileExp srcoffset <*> pure srcspace
    <*> compileExp size <*> pure Int

compileCode (Imp.Write dest (Imp.Count idx) elemtype DefaultSpace elemexp) = do
  idx' <- compileExp idx
  elemexp' <- compileExp elemexp
  let dest' = Var $ pretty dest
  let elemtype' = compileBasicType elemtype
  let ctype = Call elemtype' [elemexp']
  stm $ Exp $ Call "writeScalarArray" [dest', idx', ctype]

compileCode (Imp.Write dest (Imp.Count idx) elemtype (Imp.Space space) elemexp) =
  join $ asks envWriteScalar
    <*> pure dest
    <*> compileExp idx
    <*> pure elemtype
    <*> pure space
    <*> compileExp elemexp

compileCode Imp.Skip = stm Pass
