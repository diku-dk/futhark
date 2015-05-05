{-# LANGUAGE QuasiQuotes, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | C code generator framework.
module Futhark.CodeGen.Backends.GenericC
  ( compileProg
  -- * Pluggable compiler
  , OpCompiler
  , OpCompilerResult(..)
  , PointerQuals
  -- * Monadic compiler interface
  , CompilerM
  , runCompilerM
  , collect
  , compileFun
  , compileCode
  , compileExp
  , dimSizeToExp
  , item
  , stm
  , stms
  , decl
  -- * Building Blocks
  , scalarTypeToCType
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
import Data.Loc

import Prelude

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Text.PrettyPrint.Mainland hiding (space)

import Futhark.CodeGen.ImpCode
import Futhark.MonadFreshNames
import Futhark.Representation.AST.Syntax (BinOp (..))
import Futhark.CodeGen.Backends.SimpleRepresentation
import Futhark.CodeGen.Backends.GenericCReading
import qualified Futhark.CodeGen.Backends.CUtils as C

data CompilerState = CompilerState {
    compTypeStructs :: [([Type], (C.Type, C.Definition))]
  , compVarDefinitions :: [C.Definition]
  , compInit :: [C.Stm]
  , compNameSrc :: VNameSource
  }

newCompilerState :: VNameSource -> CompilerState
newCompilerState src = CompilerState {
                         compTypeStructs = []
                       , compVarDefinitions = []
                       , compInit = []
                       , compNameSrc = src
                       }


-- | A substitute expression compiler, tried before the main
-- compilation function.
type OpCompiler op = op -> CompilerM op (OpCompilerResult op)

-- | The result of the substitute expression compiler.
data OpCompilerResult op = CompileCode (Code op) -- ^ Equivalent to this code.
                         | Done -- ^ Code added via monadic interface.

-- | The address space qualifiers for a pointer of the given type with
-- the given annotation.
type PointerQuals op = String -> CompilerM op [C.TypeQual]

data CompilerEnv op = CompilerEnv {
    envOpCompiler :: OpCompiler op
  , envPointerQuals :: PointerQuals op
  , envFtable     :: HM.HashMap Name [Type]
  }

newCompilerEnv :: Program op -> OpCompiler op -> PointerQuals op
               -> CompilerEnv op
newCompilerEnv (Program funs) ec pc =
  CompilerEnv { envOpCompiler = ec
              , envPointerQuals = pc
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
typeDefinitions :: CompilerState -> [C.Definition]
typeDefinitions = reverse . map (snd . snd) . compTypeStructs

newtype CompilerM op a = CompilerM (RWS (CompilerEnv op) [C.BlockItem] CompilerState a)
  deriving (Functor, Applicative, Monad,
            MonadState CompilerState,
            MonadReader (CompilerEnv op),
            MonadWriter [C.BlockItem])

instance MonadFreshNames (CompilerM op) where
  getNameSource = gets compNameSrc
  putNameSource src = modify $ \s -> s { compNameSrc = src }

runCompilerM :: Program op -> OpCompiler op -> PointerQuals op -> VNameSource
             -> CompilerM op a
             -> (a, CompilerState)
runCompilerM prog ec pc src (CompilerM m) =
  let (x, s, _) = runRWS m (newCompilerEnv prog ec pc) (newCompilerState src)
  in (x, s)

collect :: CompilerM op () -> CompilerM op [C.BlockItem]
collect m = pass $ do
  ((), w) <- listen m
  return (w, const mempty)

lookupFunction :: Name -> CompilerM op [Type]
lookupFunction name = do
  res <- asks $ HM.lookup name . envFtable
  case res of
    Nothing -> fail $ "Function " ++ nameToString name ++ " not found."
    Just ts -> return ts

item :: C.BlockItem -> CompilerM op ()
item x = tell [x]

instance C.ToIdent VName where
  toIdent = C.toIdent . textual

stm :: C.Stm -> CompilerM op ()
stm (C.Block items _) = mapM_ item items
stm (C.Default s _) = stm s
stm s = item [C.citem|$stm:s|]

stms :: [C.Stm] -> CompilerM op ()
stms = mapM_ stm

decl :: C.InitGroup -> CompilerM op ()
decl x = item [C.citem|$decl:x;|]

valueTypeName :: Type -> String
valueTypeName (Scalar Int)  = "int"
valueTypeName (Scalar Bool) = "bool"
valueTypeName (Scalar Char) = "char"
valueTypeName (Scalar Real) = "real"
valueTypeName (Scalar Cert) = "cert"
valueTypeName (Mem _ (Just space)) = space ++ "_mem"
valueTypeName (Mem _ Nothing) = "mem"

typeName :: [Type] -> String
typeName [t] = valueTypeName t
typeName ts  = "tuple_" ++ intercalate "_" (map (typeName . pure) ts)

scalarTypeToCType :: BasicType -> C.Type
scalarTypeToCType bt =
  C.Type (C.DeclSpec [] [] (scalarTypeToCTypeSpec bt) noLoc)
  (C.DeclRoot noLoc)
  noLoc

scalarTypeToCTypeSpec :: BasicType -> C.TypeSpec
scalarTypeToCTypeSpec Int  = C.Tint Nothing noLoc
scalarTypeToCTypeSpec Bool = C.Tint Nothing noLoc
scalarTypeToCTypeSpec Char = C.Tchar Nothing noLoc
scalarTypeToCTypeSpec Real = C.Tdouble noLoc
scalarTypeToCTypeSpec Cert = C.Tint Nothing noLoc

qualsFromSpace :: Space -> CompilerM op [C.TypeQual]
qualsFromSpace Nothing =
  return []
qualsFromSpace (Just space) =
  join $ asks envPointerQuals <*> pure space

pointerTypeFromElemSpec :: C.TypeSpec -> Space
                        -> CompilerM op C.Type
pointerTypeFromElemSpec espec space = do
  quals <- qualsFromSpace space
  let spec = C.DeclSpec [] quals espec noLoc
      tdecl = C.Ptr [] (C.DeclRoot noLoc) noLoc
  return $ C.Type spec tdecl noLoc

pointerType :: BasicType -> Space -> CompilerM op C.Type
pointerType =
  pointerTypeFromElemSpec . scalarTypeToCTypeSpec

memToCType :: Space -> CompilerM op C.Type
memToCType =
  pointerTypeFromElemSpec $ C.Tchar (Just $ C.Tunsigned noLoc) noLoc

typeToCType :: [Type] -> CompilerM op C.Type
typeToCType [Scalar bt] = return $ scalarTypeToCType bt
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

printBasicStm :: C.Exp -> BasicType -> C.Stm
printBasicStm val Int = [C.cstm|printf("%d", $exp:val);|]
printBasicStm val Char = [C.cstm|printf("'%c'", $exp:val);|]
printBasicStm val Bool = [C.cstm|printf($exp:val ? "True" : "False");|]
printBasicStm val Real = [C.cstm|printf("%.6f", $exp:val);|]
printBasicStm _ Cert = [C.cstm|printf("Checked");|]

-- | Return a statement printing the given value.
printStm :: ValueDecl -> CompilerM op C.Stm
printStm (ScalarValue bt name) =
  return $ printBasicStm (C.var name) bt
printStm (ArrayValue mem bt []) =
  return $ printBasicStm val bt
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
      bt'  = scalarTypeToCType bt
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

readFun :: BasicType -> Maybe String
readFun Int  = Just "read_int"
readFun Char = Just "read_char"
readFun Real = Just "read_double"
readFun _    = Nothing

paramsTypes :: [Param] -> [Type]
paramsTypes = map paramType
  where paramType (MemParam _ size space) = Mem size space
        paramType (ScalarParam _ t) = Scalar t

readBasicStm :: C.Exp -> BasicType -> C.Stm
readBasicStm place t
  | Just f <- readFun t =
    [C.cstm|if ($id:f(&$exp:place) != 0) {
          fprintf(stderr, "Syntax error when reading %s.\n", $string:(prettyPrint t));
                 exit(1);
        }|]
readBasicStm _ Cert =
  [C.cstm|;|]
readBasicStm _ t =
  [C.cstm|{
        fprintf(stderr, "Cannot read %s.\n", $string:(prettyPrint t));
        exit(1);
      }|]

sizeVars :: [Param] -> HM.HashMap VName VName
sizeVars = mconcat . map sizeVars'
  where sizeVars' (MemParam parname (VarSize memsizename) _) =
          HM.singleton parname memsizename
        sizeVars' _ =
          HM.empty

readInput :: HM.HashMap VName VName -> ValueDecl -> C.Stm
readInput _ (ScalarValue t name) =
  readBasicStm (C.var name) t
readInput memsizes (ArrayValue name t shape)
  | Just f <- readFun t =
  -- We need to create an array for the array parser to put
  -- the shapes.
  let t' = scalarTypeToCType t
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
          fprintf(stderr, "Syntax error when reading %s.\n", $string:(ppArrayType t rank));
          exit(1);
        }
        $stms:copyshape
        $stms:copymemsize
      }|]
  | otherwise =
    [C.cstm|{
       fprintf(stderr, "Cannot read %s.\n", $string:(prettyPrint t));
               exit(1);
    }|]

readInputs :: [Param] -> [ValueDecl] -> [C.Stm]
readInputs inputparams = map $ readInput memsizes
  where memsizes = sizeVars inputparams

printResult :: [ValueDecl] -> CompilerM op [C.Stm]
printResult vs = liftM concat $ forM vs $ \v -> do
  p <- printStm v
  return [p, [C.cstm|printf("\n");|]]

unpackResults :: VName -> [Param] -> [C.Stm]
unpackResults ret [ScalarParam name _] =
  [[C.cstm|$id:name = $id:ret;|]]
unpackResults ret [MemParam name _ _] =
 [[C.cstm|$id:name = $id:ret;|]]
unpackResults ret outparams = zipWith assign outparams [0..]
  where assign param i =
          let e = tupleFieldExp (C.var ret) i
          in [C.cstm|$id:(paramName param) = $exp:e;|]

mainCall :: Name -> Function op -> CompilerM op C.Stm
mainCall fname (Function outputs inputs _ results args) = do
  crettype <- typeToCType $ paramsTypes outputs
  ret <- newVName "main_ret"
  let argexps = map (C.var . paramName) inputs
      unpackstms = unpackResults ret outputs
      readstms = readInputs inputs args
  paramdecls <- liftM2 (++) (mapM paramDecl outputs) (mapM paramDecl inputs)
  printstms <- printResult results
  return [C.cstm|{
               $decls:paramdecls
               $ty:crettype $id:ret;
               $stms:readstms
               gettimeofday(&t_start, NULL);
               $id:ret = $id:(funName fname)($args:argexps);
               gettimeofday(&t_end, NULL);
               $stms:unpackstms
               $stms:printstms
             }|]
  where paramDecl (MemParam name _ space) = do
          ty <- memToCType space
          return [C.cdecl|$ty:ty $id:name;|]
        paramDecl (ScalarParam name ty) = do
          let ty' = scalarTypeToCType ty
          return [C.cdecl|$ty:ty' $id:name;|]

-- | Compile imperative program to a C program.  Always uses the
-- function named "main" as entry point, so make sure it is defined.
compileProg :: OpCompiler op -> PointerQuals op
            -> [C.Definition] -> [C.Stm]
            -> Program op
            -> String
compileProg ec pc decls mainstms prog@(Program funs) =
  let ((prototypes, definitions, main), endstate) =
        runCompilerM prog ec pc blankNameSource compileProg'
  in pretty 80 $ ppr [C.cunit|
$esc:("#include <stdio.h>")
$esc:("#include <stdlib.h>")
$esc:("#include <string.h>")
$esc:("#include <stdint.h>")
$esc:("#include <math.h>")
$esc:("#include <sys/time.h>")
$esc:("#include <ctype.h>")
$esc:("#include <errno.h>")
$esc:("#include <assert.h>")

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

int main(int argc, char** argv) {
  struct timeval t_start, t_end, t_diff;
  unsigned long int elapsed_usec;
  $stms:(compInit endstate)
  $stms:mainstms
  $stm:main;
  if (argc == 3 && strcmp(argv[1], "-t") == 0) {
    FILE* runtime_file;
    runtime_file = fopen(argv[2], "w");
    if (runtime_file == NULL) {
      fprintf(stderr, "Cannot open %s: %s\n", argv[2], strerror(errno));
      exit(1);
    }
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed_usec = t_diff.tv_sec*1e6+t_diff.tv_usec;
    fprintf(runtime_file, "%ld\n", elapsed_usec / 1000);
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
                    Just func -> mainCall mainname func
          return (prototypes, definitions, main)
        funcToDef func = C.FuncDef func loc
          where loc = case func of
                        C.OldFunc _ _ _ _ _ _ l -> l
                        C.Func _ _ _ _ _ l      -> l

        builtin = map asDecl builtInFunctionDefs
          where asDecl fun = [C.cedecl|$func:fun|]

compileFun :: (Name, Function op) -> CompilerM op (C.Definition, C.Func)
compileFun (fname, Function outputs inputs body _ _) = do
  args' <- mapM compileInput inputs
  body' <- collect $ do
             mapM_ compileOutput outputs
             compileFunBody outputs body
  crettype <- typeToCType $ paramsTypes outputs
  return ([C.cedecl|static $ty:crettype $id:(funName fname)( $params:args' );|],
          [C.cfun|static $ty:crettype $id:(funName fname)( $params:args' ) { $items:body' }|])
  where compileInput (ScalarParam name bt) = do
          let ctp = scalarTypeToCType bt
          return [C.cparam|$ty:ctp $id:name|]
        compileInput (MemParam name _ space) = do
          ty <- memToCType space
          return [C.cparam|$ty:ty $id:name|]

        compileOutput (ScalarParam name bt) = do
          let ctp = scalarTypeToCType bt
          decl [C.cdecl|$ty:ctp $id:name;|]
        compileOutput (MemParam name _ space) = do
          ty <- memToCType space
          decl [C.cdecl|$ty:ty $id:name;|]

compileBasicValue :: BasicValue -> C.Exp

compileBasicValue (IntVal k) =
  [C.cexp|$int:k|]

compileBasicValue (RealVal x) =
  [C.cexp|$double:(toRational x)|]

compileBasicValue (LogVal b) =
  [C.cexp|$int:b'|]
  where b' :: Int
        b' = if b then 1 else 0

compileBasicValue (CharVal c) =
  [C.cexp|$char:c|]

compileBasicValue Checked =
  [C.cexp|0|]

dimSizeToExp :: DimSize -> C.Exp
dimSizeToExp (ConstSize x) = [C.cexp|$int:x|]
dimSizeToExp (VarSize v)   = C.var v

compileExp :: Exp -> CompilerM op C.Exp

compileExp (Constant val) = return $ compileBasicValue val

compileExp (ScalarVar src) =
  return [C.cexp|$id:src|]

compileExp (Index src iexp restype space) = do
  iexp' <- compileExp iexp
  ty <- pointerType restype space
  return [C.cexp|*(($ty:ty)&($id:src[$exp:iexp']))|]

compileExp (UnOp Negate x) = do
  x' <- compileExp x
  return [C.cexp|-$exp:x'|]

compileExp (UnOp Complement x) = do
  x' <- compileExp x
  return [C.cexp|~$exp:x'|]

compileExp (UnOp Not x) = do
  x' <- compileExp x
  return [C.cexp|!$exp:x'|]

compileExp (BinOp bop x y) = do
  x' <- compileExp x
  y' <- compileExp y
  return $ case bop of
             Plus -> [C.cexp|$exp:x' + $exp:y'|]
             Minus -> [C.cexp|$exp:x' - $exp:y'|]
             Times -> [C.cexp|$exp:x' * $exp:y'|]
             Divide -> [C.cexp|$exp:x' / $exp:y'|]
             Mod -> [C.cexp|$exp:x' % $exp:y'|]
             Pow -> [C.cexp|powl($exp:x',$exp:y')|]
             ShiftR -> [C.cexp|$exp:x' >> $exp:y'|]
             ShiftL -> [C.cexp|$exp:x' << $exp:y'|]
             Band -> [C.cexp|$exp:x' & $exp:y'|]
             Xor -> [C.cexp|$exp:x' ^ $exp:y'|]
             Bor -> [C.cexp|$exp:x' | $exp:y'|]
             LogAnd -> [C.cexp|$exp:x' && $exp:y'|]
             LogOr -> [C.cexp|$exp:x' || $exp:y'|]
             Equal -> [C.cexp|$exp:x' == $exp:y'|]
             Less -> [C.cexp|$exp:x' < $exp:y'|]
             Leq -> [C.cexp|$exp:x' <= $exp:y'|]

compileExp (SizeOf t) =
  return [C.cexp|(sizeof($ty:t'))|]
  where t' = scalarTypeToCType t

compileCode :: Code op -> CompilerM op ()

compileCode (Op op) = do
  opc <- asks envOpCompiler
  res <- opc op
  case res of Done             -> return ()
              CompileCode code -> compileCode code

compileCode Skip = return ()

compileCode (c1 :>>: c2) = compileCode c1 >> compileCode c2

compileCode (Assert e loc) = do
  e' <- compileExp e
  stm [C.cstm|{
            if (!$exp:e') {
                   fprintf(stderr, "Assertion %s at %s failed.\n",
                                   $string:(prettyPrint e), $string:(locStr loc));
                   abort();
                 }
          }|]

compileCode (Allocate name e) = do
  size' <- compileExp e
  stm [C.cstm|$id:name = malloc($exp:size');|]

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
  stm [C.cstm|if ($exp:cond') { $items:tbranch' } else { $items:fbranch' }|]

compileCode (Copy dest destoffset src srcoffset size) = do
  destoffset' <- compileExp destoffset
  srcoffset' <- compileExp srcoffset
  size' <- compileExp size
  stm [C.cstm|memmove($id:dest + $exp:destoffset',
                      $id:src + $exp:srcoffset',
                      $exp:size');|]

compileCode (Write dest idx elemtype space elemexp) = do
  idx' <- compileExp idx
  elemexp' <- compileExp elemexp
  ty <- pointerType elemtype space
  stm [C.cstm|*(($ty:ty)&($id:dest[$exp:idx'])) = $exp:elemexp';|]

compileCode (DeclareMem name space) = do
  ty <- memToCType space
  decl [C.cdecl|$ty:ty $id:name;|]

compileCode (DeclareScalar name t) = do
  let ct = scalarTypeToCType t
  decl [C.cdecl|$ty:ct $id:name;|]

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

compileFunBody :: [Param] -> Code op -> CompilerM op ()
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

prettyPrint :: Pretty a => a -> String
prettyPrint x = displayS (renderCompact $ ppr x) ""

ppArrayType :: BasicType -> Int -> String
ppArrayType t 0 = prettyPrint t
ppArrayType t n = "[" ++ ppArrayType t (n-1) ++ "]"
