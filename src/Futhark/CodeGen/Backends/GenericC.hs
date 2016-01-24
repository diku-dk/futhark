{-# LANGUAGE QuasiQuotes, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | C code generator framework.
module Futhark.CodeGen.Backends.GenericC
  ( compileProg
  -- * Pluggable compiler
  , Operations (..)
  , defaultOperations
  , OpCompiler
  , OpCompilerResult(..)

  , PointerQuals
  , MemoryType
  , WriteScalar
  , writeScalarPointerWithQuals
  , ReadScalar
  , readScalarPointerWithQuals
  , Allocate
  , Copy
  -- * Monadic compiler interface
  , CompilerM
  , CompilerState (compUserState)
  , getUserState
  , putUserState
  , modifyUserState
  , runCompilerM
  , collect
  , compileFun
  , compileCode
  , compileExp
  , compileExpToName
  , dimSizeToExp
  , item
  , stm
  , stms
  , decl
  -- * Building Blocks
  , primTypeToCType
  ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import qualified Data.HashMap.Lazy as HM
import Data.List
import Data.Maybe

import Prelude

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.CodeGen.ImpCode hiding (dimSizeToExp)
import Futhark.MonadFreshNames
import Futhark.CodeGen.Backends.SimpleRepresentation
import Futhark.CodeGen.Backends.GenericC.Reading
import qualified Futhark.CodeGen.Backends.CUtils as C
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.Util.Pretty hiding (space)
import Futhark.Representation.AST.Attributes (builtInFunctions)

data CompilerState s = CompilerState {
    compTypeStructs :: [([Type], (C.Type, C.Definition))]
  , compVarDefinitions :: [C.Definition]
  , compInit :: [C.Stm]
  , compNameSrc :: VNameSource
  , compUserState :: s
  }

newCompilerState :: VNameSource -> s -> CompilerState s
newCompilerState src s = CompilerState { compTypeStructs = []
                                       , compVarDefinitions = []
                                       , compInit = []
                                       , compNameSrc = src
                                       , compUserState = s
                                       }

-- | A substitute expression compiler, tried before the main
-- compilation function.
type OpCompiler op s = op -> CompilerM op s (OpCompilerResult op)

-- | The result of the substitute expression compiler.
data OpCompilerResult op = CompileCode (Code op) -- ^ Equivalent to this code.
                         | Done -- ^ Code added via monadic interface.

-- | The address space qualifiers for a pointer of the given type with
-- the given annotation.
type PointerQuals op s = String -> CompilerM op s [C.TypeQual]

-- | The type of a memory block in the given memory space.
type MemoryType op s = SpaceId -> CompilerM op s C.Type

-- | Write a scalar to the given memory block with the given index and
-- in the given memory space.
type WriteScalar op s = VName -> C.Exp -> C.Type -> SpaceId -> C.Exp
                        -> CompilerM op s ()

-- | Read a scalar from the given memory block with the given index and
-- in the given memory space.
type ReadScalar op s = VName -> C.Exp -> C.Type -> SpaceId
                       -> CompilerM op s C.Exp

-- | Allocate a memory block of the given size in the given memory
-- space, saving a reference in the given variable name.
type Allocate op s = VName -> C.Exp -> SpaceId
                     -> CompilerM op s ()

-- | Copy from one memory block to another.
type Copy op s = VName -> C.Exp -> Space ->
                 VName -> C.Exp -> Space ->
                 C.Exp ->
                 CompilerM op s ()

data Operations op s = Operations { opsWriteScalar :: WriteScalar op s
                                  , opsReadScalar :: ReadScalar op s
                                  , opsAllocate :: Allocate op s
                                  , opsCopy :: Copy op s

                                  , opsMemoryType :: MemoryType op s
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
                               , opsMemoryType = defMemoryType
                               , opsCompiler = defCompiler
                               }
  where defWriteScalar _ _ _ _ _ =
          fail "Cannot write to non-default memory space because I am dumb"
        defReadScalar _ _ _ _ =
          fail "Cannot read from non-default memory space"
        defAllocate _ _ _ =
          fail "Cannot allocate in non-default memory space"
        defCopy _ _ _ _ _ _ _ =
          fail "Cannot copy to or from non-default memory space"
        defMemoryType _ =
          fail "Has no type for non-default memory space"
        defCompiler _ =
          fail "The default compiler cannot compile extended operations"

data CompilerEnv op s = CompilerEnv {
    envOperations :: Operations op s
  , envFtable     :: HM.HashMap Name [Type]
  }

envOpCompiler :: CompilerEnv op s -> OpCompiler op s
envOpCompiler = opsCompiler . envOperations

envMemoryType :: CompilerEnv op s -> MemoryType op s
envMemoryType = opsMemoryType . envOperations

envReadScalar :: CompilerEnv op s -> ReadScalar op s
envReadScalar = opsReadScalar . envOperations

envWriteScalar :: CompilerEnv op s -> WriteScalar op s
envWriteScalar = opsWriteScalar . envOperations

envAllocate :: CompilerEnv op s -> Allocate op s
envAllocate = opsAllocate . envOperations

envCopy :: CompilerEnv op s -> Copy op s
envCopy = opsCopy . envOperations

newCompilerEnv :: Functions op -> Operations op s
               -> CompilerEnv op s
newCompilerEnv (Functions funs) ops =
  CompilerEnv { envOperations = ops
              , envFtable = ftable <> builtinFtable
              }
  where ftable = HM.fromList $ map funReturn funs
        funReturn (name, Function outparams _ _ _ _) =
          (name, paramsTypes outparams)
        builtinFtable =
          HM.map (map Scalar . snd) builtInFunctions

-- | Return a list of struct definitions for the tuples and arrays
-- seen during compilation.  The list is sorted according to
-- dependencies, such that a struct at index N only depends on structs
-- at positions >N.
typeDefinitions :: CompilerState s -> [C.Definition]
typeDefinitions = reverse . map (snd . snd) . compTypeStructs

newtype CompilerM op s a = CompilerM (RWS (CompilerEnv op s) [C.BlockItem] (CompilerState s) a)
  deriving (Functor, Applicative, Monad,
            MonadState (CompilerState s),
            MonadReader (CompilerEnv op s),
            MonadWriter [C.BlockItem])

instance MonadFreshNames (CompilerM op s) where
  getNameSource = gets compNameSrc
  putNameSource src = modify $ \s -> s { compNameSrc = src }

runCompilerM :: Functions op -> Operations op s -> VNameSource -> s
             -> CompilerM op s a
             -> (a, CompilerState s)
runCompilerM prog ops src userstate (CompilerM m) =
  let (x, s, _) = runRWS m (newCompilerEnv prog ops) (newCompilerState src userstate)
  in (x, s)

getUserState :: CompilerM op s s
getUserState = gets compUserState

putUserState :: s -> CompilerM op s ()
putUserState s = modify $ \compstate -> compstate { compUserState = s }

modifyUserState :: (s -> s) -> CompilerM op s ()
modifyUserState f = modify $ \compstate ->
  compstate { compUserState = f $ compUserState compstate }

collect :: CompilerM op s () -> CompilerM op s [C.BlockItem]
collect m = pass $ do
  ((), w) <- listen m
  return (w, const mempty)

collect' :: CompilerM op s a -> CompilerM op s (a, [C.BlockItem])
collect' m = pass $ do
  (x, w) <- listen m
  return ((x, w), const mempty)

lookupFunction :: Name -> CompilerM op s [Type]
lookupFunction name = do
  res <- asks $ HM.lookup name . envFtable
  case res of
    Nothing -> fail $ "Function " ++ nameToString name ++ " not found."
    Just ts -> return ts

item :: C.BlockItem -> CompilerM op s ()
item x = tell [x]

instance C.ToIdent VName where
  toIdent = C.toIdent . sanitise . textual
  -- FIXME: this sanitising is incomplete.
    where sanitise = map sanitise'
          sanitise' '\'' = '_'
          sanitise' c    = c

stm :: C.Stm -> CompilerM op s ()
stm (C.Block items _) = mapM_ item items
stm (C.Default s _) = stm s
stm s = item [C.citem|$stm:s|]

stms :: [C.Stm] -> CompilerM op s ()
stms = mapM_ stm

decl :: C.InitGroup -> CompilerM op s ()
decl x = item [C.citem|$decl:x;|]

valueTypeName :: Type -> String
valueTypeName (Scalar t) = pretty $ primTypeToCType t
valueTypeName (Mem _ (Space space)) = space ++ "_mem"
valueTypeName (Mem _ DefaultSpace) = "mem"

typeName :: [Type] -> String
typeName [t] = valueTypeName t
typeName ts  = "tuple_" ++ intercalate "_" (map (typeName . pure) ts)

memToCType :: Space -> CompilerM op s C.Type
memToCType DefaultSpace =
  return defaultMemBlockType
memToCType (Space space) =
  join $ asks envMemoryType <*> pure space

typeToCType :: [Type] -> CompilerM op s C.Type
typeToCType [Scalar bt] = return $ primTypeToCType bt
typeToCType [Mem _ space] = memToCType space
typeToCType t = do
  ty <- gets $ find (sameRepresentation t . fst) . compTypeStructs
  case ty of
    Just (_, (cty, _)) -> return cty
    Nothing -> do
      members <- zipWithM field t [(0::Int)..]
      let name = typeName t
          struct = [C.cedecl|struct $id:name { $sdecls:members };|]
          stype  = [C.cty|struct $id:name|]
      modify $ \s -> s { compTypeStructs = (t, (stype,struct)) : compTypeStructs s }
      return stype
        where field et i = do
                ct <- typeToCType [et]
                return [C.csdecl|$ty:ct $id:(tupleField i);|]

printPrimStm :: C.Exp -> PrimType -> C.Stm
printPrimStm val (IntType Int8) = [C.cstm|printf("%di8", $exp:val);|]
printPrimStm val (IntType Int16) = [C.cstm|printf("%di16", $exp:val);|]
printPrimStm val (IntType Int32) = [C.cstm|printf("%di32", $exp:val);|]
printPrimStm val (IntType Int64) = [C.cstm|printf("%lldi64", $exp:val);|]
printPrimStm val Char = [C.cstm|printf("'%c'", $exp:val);|]
printPrimStm val Bool = [C.cstm|printf($exp:val ? "True" : "False");|]
printPrimStm val (FloatType Float32) = [C.cstm|printf("%.6ff32", $exp:val);|]
printPrimStm val (FloatType Float64) = [C.cstm|printf("%.6ff64", $exp:val);|]
printPrimStm _ Cert = [C.cstm|printf("Checked");|]

-- | Return a statement printing the given value.
printStm :: ValueDecl -> CompilerM op s C.Stm
printStm (ScalarValue bt name) =
  return $ printPrimStm (C.var name) bt
printStm (ArrayValue mem bt []) =
  return $ printPrimStm val bt
  where val = [C.cexp|*$id:mem|]
printStm (ArrayValue mem Char [size]) = do
  i <- newVName "print_i"
  let size' = dimSizeToExp size
  return [C.cstm|{
          int $id:i;
          printf("\"");
          for ($id:i = 0; $id:i < $exp:size'; $id:i++) {
            printf("%c", ((char*)$id:mem)[$id:i]);
          }
          printf("\"");
          }|]
printStm (ArrayValue mem bt (dim:shape)) = do
  i <- newVName "print_i"
  v <- newVName "print_elem"
  let dim' = dimSizeToExp dim
      shape' = C.product $ map dimSizeToExp shape
      bt'  = primTypeToCType bt
  printelem <- printStm $ ArrayValue v bt shape
  return [C.cstm|{
               if ($exp:dim' == 0) {
                   printf("empty(%s)", $exp:(ppArrayType bt (length shape)));
               } else {
                   int $id:i;
                   putchar('[');
                   for ($id:i = 0; $id:i < $exp:dim'; $id:i++) {
                           $ty:bt' *$id:v = (($ty:bt'*) $id:mem) + $id:i * $exp:shape';
                           $stm:printelem
                           if ($id:i != $exp:dim'-1) {
                             printf(", ");
                           }
                   }
               putchar(']');
               }
             }|]

readFun :: PrimType -> Maybe String
readFun (IntType Int8) = Just "read_int8"
readFun (IntType Int16) = Just "read_int16"
readFun (IntType Int32) = Just "read_int32"
readFun (IntType Int64) = Just "read_int64"
readFun Char = Just "read_char"
readFun Bool = Just "read_bool"
readFun (FloatType Float32) = Just "read_float"
readFun (FloatType Float64) = Just "read_double"
readFun _    = Nothing

paramsTypes :: [Param] -> [Type]
paramsTypes = map paramType
  where paramType (MemParam _ size space) = Mem size space
        paramType (ScalarParam _ t) = Scalar t

readPrimStm :: C.Exp -> PrimType -> C.Stm
readPrimStm place t
  | Just f <- readFun t =
    [C.cstm|if ($id:f(&$exp:place) != 0) {
          errx(1, "Syntax error when reading %s.\n", $string:(pretty t));
        }|]
readPrimStm _ Cert =
  [C.cstm|;|]
readPrimStm _ t =
  [C.cstm|{
        errx(1, "Cannot read %s.\n", $string:(pretty t));
      }|]

-- | Our strategy for main() is to parse everything into host memory
-- ('DefaultSpace-) and copy the result into host memory after the
-- @fut_main()@ function has returned.  We have some ad-hoc frobbery
-- to copy the host-level memory blocks to another memory space if
-- necessary.  This will break if @fut_main@ uses non-trivial index
-- functions for its input or output.
--
-- The idea here is to keep the nastyness in main(), whilst not
-- messing up anything else.
mainCall :: [C.Stm] -> Name -> Function op -> CompilerM op s C.Stm
mainCall pre_timing fname (Function outputs inputs _ results args) = do
  crettype <- typeToCType $ paramsTypes outputs
  ret <- newVName "main_ret"
  let readstms = readInputs inputs args
  (argexps, prepare) <- collect' $ mapM prepareArg inputs
  -- unpackResults may copy back to DefaultSpace.
  unpackstms <- unpackResults ret outputs
  -- paramDecl will always create DefaultSpace memory.
  paramdecls <- liftM2 (++) (mapM paramDecl outputs) (mapM paramDecl inputs)
  printstms <- printResult results
  return [C.cstm|{
               $decls:paramdecls
               $ty:crettype $id:ret;
               $stms:readstms
               $items:prepare
               gettimeofday(&t_start, NULL);
               $id:ret = $id:(funName fname)($args:argexps);
               $stms:pre_timing
               gettimeofday(&t_end, NULL);
               $items:unpackstms
               $stms:printstms
             }|]
  where paramDecl (MemParam name _ _) = do
          ty <- memToCType DefaultSpace
          return [C.cdecl|$ty:ty $id:name;|]
        paramDecl (ScalarParam name ty) = do
          let ty' = primTypeToCType ty
          return [C.cdecl|$ty:ty' $id:name;|]

prepareArg :: Param -> CompilerM op s C.Exp
prepareArg (MemParam name size (Space space)) = do
  -- Futhark main expects some other memory space than default, so
  -- create a new memory block and copy it there.
  name' <- newVName $ baseString name <> "_" <> space
  ty <- memToCType $ Space space
  copy <- asks envCopy
  alloc <- asks envAllocate
  let size' = dimSizeToExp size
  decl [C.cdecl|$ty:ty $id:name';|]
  alloc name' size' space
  copy name' [C.cexp|0|] (Space space) name [C.cexp|0|] DefaultSpace size'
  return [C.cexp|$id:name'|]

prepareArg p = return $ C.var $ paramName p

readInputs :: [Param] -> [ValueDecl] -> [C.Stm]
readInputs inputparams = map $ readInput memsizes
  where memsizes = sizeVars inputparams

readInput :: HM.HashMap VName VName -> ValueDecl -> C.Stm
readInput _ (ScalarValue t name) =
  readPrimStm (C.var name) t
readInput memsizes (ArrayValue name t shape)
  | Just f <- readFun t =
  -- We need to create an array for the array parser to put
  -- the shapes.
  let t' = primTypeToCType t
      rank = length shape
      maybeCopyDim (ConstSize _) _ =
        Nothing
      maybeCopyDim (VarSize dimname) i =
        Just [C.cstm|$id:dimname = shape[$int:i];|]
      copyshape = catMaybes $ zipWith maybeCopyDim shape [0..rank-1]
      memsize = C.product $ [C.cexp|sizeof($ty:t')|] :
                             [ [C.cexp|shape[$int:i]|] |
                              i <- [0..rank-1] ]
      copymemsize = case HM.lookup name memsizes of
        Nothing -> []
        Just sizevar -> [[C.cstm|$id:sizevar = $exp:memsize;|]]
  in [C.cstm|{
        typename int64_t shape[$int:rank];
        if (read_array(sizeof($ty:t'),
                       $id:f,
                       (void**)& $id:name,
                       shape,
                       $int:(length shape))
            != 0) {
          errx(1, "Syntax error when reading %s.\n", $string:(ppArrayType t rank));
        }
        $stms:copyshape
        $stms:copymemsize
      }|]
  | otherwise =
    [C.cstm|errx(1, "Cannot read %s.\n", $string:(pretty t));|]

sizeVars :: [Param] -> HM.HashMap VName VName
sizeVars = mconcat . map sizeVars'
  where sizeVars' (MemParam parname (VarSize memsizename) _) =
          HM.singleton parname memsizename
        sizeVars' _ =
          HM.empty

printResult :: [ValueDecl] -> CompilerM op s [C.Stm]
printResult vs = liftM concat $ forM vs $ \v -> do
  p <- printStm v
  return [p, [C.cstm|printf("\n");|]]

unpackResults :: VName -> [Param] -> CompilerM op s [C.BlockItem]
unpackResults ret [p] =
  collect $ unpackResult ret p
unpackResults ret outparams =
  collect $ zipWithM_ assign outparams [0..]
  where assign param i = do
          ret_field_tmp <- newVName "ret_field_tmp"
          field_t <- case param of
                       ScalarParam _ bt ->
                         return $ primTypeToCType bt
                       MemParam _ _ space ->
                         memToCType space
          let field_e = tupleFieldExp (C.var ret) i
          item [C.citem|$ty:field_t $id:ret_field_tmp = $exp:field_e;|]
          unpackResult ret_field_tmp param

unpackResult :: VName -> Param -> CompilerM op s ()
unpackResult ret (ScalarParam name _) =
  stm [C.cstm|$id:name = $id:ret;|]
unpackResult ret (MemParam name _ DefaultSpace) =
  stm [C.cstm|$id:name = $id:ret;|]
unpackResult ret (MemParam name size (Space srcspace)) = do
  copy <- asks envCopy
  let size' = dimSizeToExp size
  stm [C.cstm|$id:name = malloc($exp:size');|]
  copy name [C.cexp|0|] DefaultSpace ret [C.cexp|0|] (Space srcspace) size'

timingOption :: Option
timingOption =
  Option { optionLongName = "write-runtime-to"
         , optionShortName = Just 't'
         , optionArgument = RequiredArgument
         , optionAction =
           [C.cstm|{
  runtime_file = fopen(optarg, "w");
  if (runtime_file == NULL) {
    errx(1, "Cannot open %s: %s", optarg, strerror(errno));
  }
  }|]
  }

-- | Compile imperative program to a C program.  Always uses the
-- function named "main" as entry point, so make sure it is defined.
compileProg :: MonadFreshNames m =>
               Operations op s
            -> s
            -> [C.Definition] -> [C.Stm] -> [C.Stm] -> [C.Stm]
            -> [Option]
            -> Functions op
            -> m String
compileProg ops userstate decls pre_main_stms pre_timing post_main_stms options prog@(Functions funs) = do
  src <- getNameSource
  let ((prototypes, definitions, main), endstate) =
        runCompilerM prog ops src userstate compileProg'
  return $ pretty [C.cunit|
$esc:("#include <stdio.h>")
$esc:("#include <stdlib.h>")
$esc:("#include <string.h>")
$esc:("#include <stdint.h>")
$esc:("#include <math.h>")
$esc:("#include <sys/time.h>")
$esc:("#include <ctype.h>")
$esc:("#include <errno.h>")
$esc:("#include <assert.h>")
$esc:("#include <err.h>")
$esc:("#include <getopt.h>")

$edecls:(typeDefinitions endstate)

$edecls:(compVarDefinitions endstate)

$edecls:prototypes

$edecls:builtin

int timeval_subtract(struct timeval *result, struct timeval *t2, struct timeval *t1)
{
    unsigned int resolution=1000000;
    long int diff = (t2->tv_usec + resolution * t2->tv_sec) - (t1->tv_usec + resolution * t1->tv_sec);
    result->tv_sec = diff / resolution;
    result->tv_usec = diff % resolution;
    return (diff<0);
}

$edecls:readerFunctions

$edecls:decls

$edecls:(map funcToDef definitions)

static typename FILE *runtime_file;

$func:(generateOptionParser "parse_options" (timingOption:options))

int main(int argc, char** argv) {
  struct timeval t_start, t_end, t_diff;
  unsigned long int elapsed_usec;
  $stms:(compInit endstate)
  int parsed_options = parse_options(argc, argv);
  argc -= parsed_options;
  argv += parsed_options;
  $stms:pre_main_stms
  $stm:main;
  $stms:post_main_stms
  if (runtime_file != NULL) {
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed_usec = t_diff.tv_sec*1e6+t_diff.tv_usec;
    fprintf(runtime_file, "%ld\n", elapsed_usec);
    fclose(runtime_file);
  }
  return 0;
}

|]
  where compileProg' = do
          (prototypes, definitions) <- unzip <$> mapM compileFun funs
          let mainname = nameFromString "main"
          main <- case lookup mainname funs of
                    Nothing   -> fail "GenericC.compileProg: No main function"
                    Just func -> mainCall pre_timing mainname func
          return (prototypes, definitions, main)
        funcToDef func = C.FuncDef func loc
          where loc = case func of
                        C.OldFunc _ _ _ _ _ _ l -> l
                        C.Func _ _ _ _ _ l      -> l

        builtin = map asDecl builtInFunctionDefs ++
                  cIntOps ++ cFloat32Ops ++ cFloat64Ops ++ cFloatConvOps
          where asDecl fun = [C.cedecl|$func:fun|]

compileFun :: (Name, Function op) -> CompilerM op s (C.Definition, C.Func)
compileFun (fname, Function outputs inputs body _ _) = do
  args' <- mapM compileInput inputs
  body' <- collect $ do
             mapM_ compileOutput outputs
             compileFunBody outputs body
  crettype <- typeToCType $ paramsTypes outputs
  return ([C.cedecl|static $ty:crettype $id:(funName fname)( $params:args' );|],
          [C.cfun|static $ty:crettype $id:(funName fname)( $params:args' ) { $items:body' }|])
  where compileInput (ScalarParam name bt) = do
          let ctp = primTypeToCType bt
          return [C.cparam|$ty:ctp $id:name|]
        compileInput (MemParam name _ space) = do
          ty <- memToCType space
          return [C.cparam|$ty:ty $id:name|]

        compileOutput (ScalarParam name bt) = do
          let ctp = primTypeToCType bt
          decl [C.cdecl|$ty:ctp $id:name;|]
        compileOutput (MemParam name _ space) = do
          ty <- memToCType space
          decl [C.cdecl|$ty:ty $id:name;|]

compilePrimValue :: PrimValue -> C.Exp

compilePrimValue (IntValue (Int8Value k)) = [C.cexp|$int:k|]
compilePrimValue (IntValue (Int16Value k)) = [C.cexp|$int:k|]
compilePrimValue (IntValue (Int32Value k)) = [C.cexp|$int:k|]
compilePrimValue (IntValue (Int64Value k)) = [C.cexp|$int:k|]

compilePrimValue (FloatValue (Float64Value x)) =
  [C.cexp|$double:(toRational x)|]
compilePrimValue (FloatValue (Float32Value x)) =
  [C.cexp|$float:(toRational x)|]

compilePrimValue (BoolValue b) =
  [C.cexp|$int:b'|]
  where b' :: Int
        b' = if b then 1 else 0

compilePrimValue (CharValue c) =
  [C.cexp|$char:c|]

compilePrimValue Checked =
  [C.cexp|0|]

dimSizeToExp :: DimSize -> C.Exp
dimSizeToExp (ConstSize x) = [C.cexp|$int:x|]
dimSizeToExp (VarSize v)   = C.var v

derefPointer :: VName -> C.Exp -> C.Type -> C.Exp
derefPointer ptr i res_t =
  [C.cexp|*(($ty:res_t)&($id:ptr[$exp:i]))|]

writeScalarPointerWithQuals :: PointerQuals op s -> WriteScalar op s
writeScalarPointerWithQuals quals_f dest i elemtype space v = do
  quals <- quals_f space
  let deref = derefPointer dest i
              [C.cty|$tyquals:quals $ty:elemtype*|]
  stm [C.cstm|$exp:deref = $exp:v;|]

readScalarPointerWithQuals :: PointerQuals op s -> ReadScalar op s
readScalarPointerWithQuals quals_f dest i elemtype space = do
  quals <- quals_f space
  return $ derefPointer dest i [C.cty|$tyquals:quals $ty:elemtype*|]

compileExpToName :: String -> PrimType -> Exp -> CompilerM op s VName
compileExpToName _ _ (ScalarVar v) =
  return v
compileExpToName desc t e = do
  desc' <- newVName desc
  e' <- compileExp e
  decl [C.cdecl|$ty:(primTypeToCType t) $id:desc' = $e';|]
  return desc'

compileExp :: Exp -> CompilerM op s C.Exp

compileExp (Constant val) = return $ compilePrimValue val

compileExp (ScalarVar src) =
  return [C.cexp|$id:src|]

compileExp (Index src (Count iexp) restype DefaultSpace) =
  derefPointer src
  <$> compileExp iexp
  <*> pure [C.cty|$ty:(primTypeToCType restype)*|]

compileExp (Index src (Count iexp) restype (Space space)) =
  join $ asks envReadScalar
    <*> pure src <*> compileExp iexp
    <*> pure (primTypeToCType restype) <*> pure space

compileExp (UnOp Complement{} x) = do
  x' <- compileExp x
  return [C.cexp|~$exp:x'|]

compileExp (UnOp Not{} x) = do
  x' <- compileExp x
  return [C.cexp|!$exp:x'|]

compileExp (UnOp Abs{} x) = do
  x' <- compileExp x
  return [C.cexp|abs($exp:x')|]

compileExp (UnOp (FAbs Float32) x) = do
  x' <- compileExp x
  return [C.cexp|fabsf($exp:x')|]

compileExp (UnOp (FAbs Float64) x) = do
  x' <- compileExp x
  return [C.cexp|fabs($exp:x')|]

compileExp (UnOp Signum{} x) = do
  x' <- compileExp x
  return [C.cexp|($exp:x' > 0) - ($exp:x' < 0)|]

compileExp (CmpOp cmp x y) = do
  x' <- compileExp x
  y' <- compileExp y
  return $ case cmp of
    CmpEq{} -> [C.cexp|$exp:x' == $exp:y'|]

    FCmpLt{} -> [C.cexp|$exp:x' < $exp:y'|]
    FCmpLe{} -> [C.cexp|$exp:x' <= $exp:y'|]

    _ -> [C.cexp|$id:(pretty cmp)($exp:x', $exp:y')|]

compileExp (ConvOp conv x) = do
  x' <- compileExp x
  return $ case conv of
    Trunc _ to -> [C.cexp|($ty:(intTypeToCType to)) $exp:x'|]
    _ -> [C.cexp|$id:(pretty conv)($exp:x')|]

compileExp (BinOp bop x y) = do
  x' <- compileExp x
  y' <- compileExp y
  return $ case bop of
             Add{} -> [C.cexp|$exp:x' + $exp:y'|]
             FAdd{} -> [C.cexp|$exp:x' + $exp:y'|]
             Sub{} -> [C.cexp|$exp:x' - $exp:y'|]
             FSub{} -> [C.cexp|$exp:x' - $exp:y'|]
             Mul{} -> [C.cexp|$exp:x' * $exp:y'|]
             FMul{} -> [C.cexp|$exp:x' * $exp:y'|]
             FDiv{} -> [C.cexp|$exp:x' / $exp:y'|]
             Xor{} -> [C.cexp|$exp:x' ^ $exp:y'|]
             And{} -> [C.cexp|$exp:x' & $exp:y'|]
             Or{} -> [C.cexp|$exp:x' | $exp:y'|]
             Shl{} -> [C.cexp|$exp:x' << $exp:y'|]
             LogAnd{} -> [C.cexp|$exp:x' && $exp:y'|]
             LogOr{} -> [C.cexp|$exp:x' || $exp:y'|]
             _ -> [C.cexp|$id:(pretty bop)($exp:x', $exp:y')|]

compileExp (SizeOf t) =
  return [C.cexp|(sizeof($ty:t'))|]
  where t' = primTypeToCType t

compileExp (Cond c t f) = do
  c' <- compileExp c
  t' <- compileExp t
  f' <- compileExp f
  return [C.cexp|$exp:c' ? $exp:t' : $exp:f'|]

compileCode :: Code op -> CompilerM op s ()

compileCode (Op op) = do
  opc <- asks envOpCompiler
  res <- opc op
  case res of Done             -> return ()
              CompileCode code -> compileCode code

compileCode Skip = return ()

compileCode (Comment s code) = do
  items <- collect $ compileCode code
  stm [C.cstm|$comment:("// " ++ s)
              { $items:items }
             |]

compileCode c
  | Just (name, t, e, c') <- declareAndSet c = do
    let ct = primTypeToCType t
    e' <- compileExp e
    item [C.citem|$ty:ct $id:name = $exp:e';|]
    compileCode c'

compileCode (c1 :>>: c2) = compileCode c1 >> compileCode c2

compileCode (Assert e loc) = do
  e' <- compileExp e
  stm [C.cstm|{
            if (!$exp:e') {
                   fprintf(stderr, "Assertion %s at %s failed.\n",
                                   $string:(pretty e), $string:(locStr loc));
                   abort();
                 }
          }|]

compileCode (Allocate name (Count e) DefaultSpace) = do
  size' <- compileExp e
  stm [C.cstm|$id:name = malloc($exp:size');|]

compileCode (Allocate name (Count e) (Space space)) =
  join $ asks envAllocate <*> pure name <*> compileExp e <*> pure space

compileCode (For i bound body) = do
  let i' = textual i
  bound' <- compileExp bound
  body'  <- collect $ compileCode body
  stm [C.cstm|for (int $id:i' = 0; $id:i' < $exp:bound'; $id:i'++) {
            $items:body'
          }|]

compileCode (While cond body) = do
  cond' <- compileExp cond
  body' <- collect $ compileCode body
  stm [C.cstm|while ($exp:cond') {
            $items:body'
          }|]

compileCode (If cond tbranch fbranch) = do
  cond' <- compileExp cond
  tbranch' <- collect $ compileCode tbranch
  fbranch' <- collect $ compileCode fbranch
  stm $ case (tbranch', fbranch') of
    (_, []) ->
      [C.cstm|if ($exp:cond') { $items:tbranch' }|]
    ([], _) ->
      [C.cstm|if (!($exp:cond')) { $items:fbranch' }|]
    _ ->
      [C.cstm|if ($exp:cond') { $items:tbranch' } else { $items:fbranch' }|]

compileCode (Copy dest (Count destoffset) DefaultSpace src (Count srcoffset) DefaultSpace (Count size)) = do
  destoffset' <- compileExp destoffset
  srcoffset' <- compileExp srcoffset
  size' <- compileExp size
  stm [C.cstm|memmove($id:dest + $exp:destoffset',
                      $id:src + $exp:srcoffset',
                      $exp:size');|]

compileCode (Copy dest (Count destoffset) destspace src (Count srcoffset) srcspace (Count size)) = do
  copy <- asks envCopy
  join $ copy
    <$> pure dest <*> compileExp destoffset <*> pure destspace
    <*> pure src <*> compileExp srcoffset <*> pure srcspace
    <*> compileExp size

compileCode (Write dest (Count idx) elemtype DefaultSpace elemexp) = do
  deref <- derefPointer dest
           <$> compileExp idx
           <*> pure [C.cty|$ty:(primTypeToCType elemtype)*|]
  elemexp' <- compileExp elemexp
  stm [C.cstm|$exp:deref = $exp:elemexp';|]

compileCode (Write dest (Count idx) elemtype (Space space) elemexp) =
  join $ asks envWriteScalar
    <*> pure dest
    <*> compileExp idx
    <*> pure (primTypeToCType elemtype)
    <*> pure space
    <*> compileExp elemexp

compileCode (DeclareMem name space) = do
  ty <- memToCType space
  decl [C.cdecl|$ty:ty $id:name;|]

compileCode (DeclareScalar name t) = do
  let ct = primTypeToCType t
  decl [C.cdecl|$ty:ct $id:name;|]

-- For assignments of the form 'x = x OP e', we generate C assignment
-- operators to make the resulting code slightly nicer.  This has no
-- effect on performance.
compileCode (SetScalar dest (BinOp op (ScalarVar x) y))
  | dest == x, Just f <- assignmentOperator op = do
      y' <- compileExp y
      stm [C.cstm|$exp:(f dest y');|]

compileCode (SetScalar dest src) = do
  src' <- compileExp src
  stm [C.cstm|$id:dest = $exp:src';|]

compileCode (SetMem dest src) =
  stm [C.cstm|$id:dest = $id:src;|]

compileCode (Call dests fname args) = do
  args' <- mapM compileExp args
  outtypes <- lookupFunction fname
  crestype <- typeToCType outtypes
  case dests of
    [dest] ->
      stm [C.cstm|$id:dest = $id:(funName fname)($args:args');|]
    _        -> do
      ret <- newVName "call_ret"
      decl [C.cdecl|$ty:crestype $id:ret;|]
      stm [C.cstm|$id:ret = $id:(funName fname)($args:args');|]
      forM_ (zip [0..] dests) $ \(i,dest) ->
        stm [C.cstm|$id:dest = $exp:(tupleFieldExp (C.var ret) i);|]

compileFunBody :: [Param] -> Code op -> CompilerM op s ()
compileFunBody outputs code = do
  retval <- newVName "retval"
  bodytype <- typeToCType $ paramsTypes outputs
  compileCode code
  decl [C.cdecl|$ty:bodytype $id:retval;|]
  let setRetVal' i output =
        stm [C.cstm|$exp:(tupleFieldExp (C.var retval) i) = $id:(paramName output);|]
  case outputs of
    [output] -> stm [C.cstm|$id:retval = $id:(paramName output);|]
    _        -> zipWithM_ setRetVal' [0..] outputs
  stm [C.cstm|return $id:retval;|]

ppArrayType :: PrimType -> Int -> String
ppArrayType t 0 = pretty t
ppArrayType t n = "[" ++ ppArrayType t (n-1) ++ "]"

declareAndSet :: Code op -> Maybe (VName, PrimType, Exp, Code op)
declareAndSet (DeclareScalar name t :>>: (SetScalar dest e :>>: c))
  | name == dest = Just (name, t, e, c)
declareAndSet ((DeclareScalar name t :>>: SetScalar dest e) :>>: c)
  | name == dest = Just (name, t, e, c)
declareAndSet _ = Nothing

assignmentOperator :: BinOp -> Maybe (VName -> C.Exp -> C.Exp)
assignmentOperator Add{}  = Just $ \d e -> [C.cexp|$id:d += $exp:e|]
assignmentOperator Sub{} = Just $ \d e -> [C.cexp|$id:d -= $exp:e|]
assignmentOperator Mul{} = Just $ \d e -> [C.cexp|$id:d *= $exp:e|]
assignmentOperator _     = Nothing
