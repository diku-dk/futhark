{-# LANGUAGE QuasiQuotes, GeneralizedNewtypeDeriving #-}
-- | C code generator framework.
module Futhark.CodeGen.Backends.GenericC
  ( compileProg
  -- * Pluggable compiler
  , OpCompiler
  , OpCompilerResult(..)
  -- * Monadic compiler interface
  , CompilerM
  , lookupVar
  , compileExp
  , item
  , stm
  , stms
  , decl
  -- * General utilities
  , typeShape
  ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS
import qualified Data.Array as A
import qualified Data.HashMap.Lazy as HM
import Data.List

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Text.PrettyPrint.Mainland

import Futhark.CodeGen.ImpCode
import Futhark.MonadFreshNames
import Futhark.CodeGen.Backends.SimpleRepresentation
import Futhark.CodeGen.Backends.GenericCReading
import qualified Futhark.CodeGen.Backends.CUtils as C

data CompilerState = CompilerState {
    compTypeStructs :: [([Type], (C.Type, C.Definition))]
  , compVarDefinitions :: [C.Definition]
  , compInit :: [C.Stm]
  , compNameSrc :: VNameSource
  , compVtable :: HM.HashMap VName ValueType
  }

newCompilerState :: VNameSource -> CompilerState
newCompilerState src = CompilerState {
                         compTypeStructs = []
                       , compVarDefinitions = []
                       , compInit = []
                       , compNameSrc = src
                       , compVtable = HM.empty
                       }


-- | A substitute expression compiler, tried before the main
-- compilation function.
type OpCompiler op = op -> CompilerM op (OpCompilerResult op)

-- | The result of the substitute expression compiler.
data OpCompilerResult op = CompileCode (Code op) -- ^ Equivalent to this code.
                         | Done -- ^ Code added via monadic interface.

data CompilerEnv op = CompilerEnv {
    envOpCompiler :: OpCompiler op
  }

newCompilerEnv :: OpCompiler op -> CompilerEnv op
newCompilerEnv ec = CompilerEnv {
                      envOpCompiler = ec
                    }

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

runCompilerM :: OpCompiler op -> VNameSource -> CompilerM op a -> (a, CompilerState)
runCompilerM ec src (CompilerM m) =
  let (x, s, _) = runRWS m (newCompilerEnv ec) (newCompilerState src)
  in (x, s)

collect :: MonadWriter w m => m () -> m w
collect m = pass $ do
  ((), w) <- listen m
  return (w, const mempty)

item :: C.BlockItem -> CompilerM op ()
item x = tell [x]

stm :: C.Stm -> CompilerM op ()
stm (C.Block items _) = mapM_ item items
stm (C.Default s _) = stm s
stm s = item [C.citem|$stm:s|]

stms :: [C.Stm] -> CompilerM op ()
stms = mapM_ stm

decl :: C.InitGroup -> CompilerM op ()
decl x = item [C.citem|$decl:x;|]

newVar :: VName -> ValueType -> CompilerM op ()
newVar k t = modify $ \s -> s { compVtable = HM.insert k t $ compVtable s }

lookupVar :: VName -> CompilerM op ValueType
lookupVar k = do t <- gets $ HM.lookup k . compVtable
                 case t of
                   Nothing -> fail $ "Uknown variable " ++ textual k ++ " in code generator."
                   Just t' -> return t'

lookupShape :: VName -> CompilerM op [C.Exp]
lookupShape = liftM typeShape . lookupVar

typeShape :: ValueType -> [C.Exp]
typeShape (Type _ shape) = map asExp shape
  where asExp (ConstSize x) = [C.cexp|$int:x|]
        asExp (VarSize v)   = [C.cexp|$id:(textual v)|]

-- | 'new s' returns a fresh variable name, with 's' prepended to it.
new :: String -> CompilerM op String
new = liftM textual . newVName

valueTypeName :: ValueType -> String
valueTypeName (Type Int  []) = "int"
valueTypeName (Type Bool []) = "bool"
valueTypeName (Type Char []) = "char"
valueTypeName (Type Real []) = "real"
valueTypeName (Type Cert []) = "cert"
valueTypeName (Type bt   shape) =
  valueTypeName (Type bt []) ++ show (length shape) ++ "d"

typeName :: [Type] -> String
typeName [Value vt] = valueTypeName vt
typeName [Mem _]    = "mem"
typeName ts  = "tuple_" ++ intercalate "_" (map (typeName . pure) ts)

valueTypeToCType :: ValueType -> C.Type
valueTypeToCType (Type Int  []) = [C.cty|int|]
valueTypeToCType (Type Bool []) = [C.cty|int|]
valueTypeToCType (Type Char []) = [C.cty|char|]
valueTypeToCType (Type Real []) = [C.cty|double|]
valueTypeToCType (Type Cert []) = [C.cty|int|]
valueTypeToCType (Type bt _) =
  let ct = valueTypeToCType $ Type bt []
  in [C.cty|$ty:ct*|]

typeToCType :: [Type] -> CompilerM op C.Type
typeToCType [Value vt] = return $ valueTypeToCType vt
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

-- | Return a statement printing the given value.
printStm :: C.Exp -> [Type] -> CompilerM op C.Stm
printStm place [Value (Type Int [])] =
  return [C.cstm|printf("%d", $exp:place);|]
printStm place [Value (Type Char [])] =
 return [C.cstm|printf("%c", $exp:place);|]
printStm place [Value (Type Bool [])] =
  return [C.cstm|printf($exp:place ? "True" : "False");|]
printStm place [Value (Type Real [])] =
 return [C.cstm|printf("%.6f", $exp:place);|]
printStm _     [Value (Type Cert [])] =
 return [C.cstm|printf("Checked");|]
printStm place [Value (Type Char [_])] =
  return [C.cstm|printf("%s", $exp:place.data);|]
printStm place [(Value t@(Type bt (_:rest)))] = do
  i <- new "print_i"
  v <- new "print_elem"
  let et' = valueTypeToCType $ Type bt rest
  pstm <- printStm (C.var v) [Value (Type bt rest)]
  let placeshape = [ [C.cexp|$exp:place.shape[$int:j]|] | j <- [0..typeRank t-1] ]
      indexi = indexArrayElemStms (C.var v) place placeshape [C.var i]
  return [C.cstm|{
               int $id:i;
               $ty:et' $id:v;
               if ($exp:place.shape[0] == 0) {
                   printf("empty(%s)", $exp:(ppType $ Value $ Type bt rest));
               } else {
                   putchar('[');
                   for ($id:i = 0; $id:i < $exp:place.shape[0]; $id:i++) {
                           $stms:indexi;
                           $stm:pstm
                           if ($id:i != $exp:place.shape[0]-1) {
                                  putchar(',');
                                  putchar(' ');
                           }
                   }
               putchar(']');
               }
             }|]
printStm place ets = do
  prints <- forM (zip [(0::Int)..] ets) $ \(i, et) ->
              printStm [C.cexp|$exp:place.$id:(tupleField i)|] [et]
  let prints' = intercalate [[C.cstm|{putchar(','); putchar(' ');}|]] $ map (:[]) prints
  return [C.cstm|{
               putchar('{');
               $stms:prints'
               putchar('}');
             }|]

readFun :: BasicType -> Maybe String
readFun Int  = Just "read_int"
readFun Char = Just "read_char"
readFun Real = Just "read_double"
readFun _    = Nothing

readStm :: C.Exp -> ValueType -> C.Stm
readStm place (Type Cert []) =
  [C.cstm|$exp:place = 1;|]
readStm place t@(Type et [])
  | Just f <- readFun et =
    [C.cstm|if ($id:f(&$exp:place) != 0) {
          fprintf(stderr, "Syntax error when reading %s.\n", $string:(ppType $ Value t));
                 exit(1);
        }|]
readStm place t@(Type et shape)
  | Just f <- readFun et =
    [C.cstm|if (read_array(sizeof(*$exp:place.data),
                           $id:f,
                           (void**)&$exp:place.data,
                           $exp:place.shape,
                           $int:rank)
                != 0) {
       fprintf(stderr, "Syntax error when reading %s.\n", $string:(ppType (Value t)));
       exit(1);
     }|]
  where rank = length shape
readStm _ t =
  [C.cstm|{
        fprintf(stderr, "Cannot read %s yet.\n", $string:(ppType (Value t)));
        exit(1);
      }|]

mainCall :: Name -> Function op -> CompilerM op C.Stm
mainCall fname (Function outputs inputs _) = do
  crettype <- typeToCType $ map paramType outputs
  ret <- new "main_ret"
  printRes <- printStm (C.var ret) $ map paramType outputs
  let mkParam (args, decls, rstms, shapeargs) (Value paramtype) = do
        name <- new "main_arg"
        let cparamtype = valueTypeToCType paramtype
            rstm = readStm (C.var name) paramtype
            argshape = [ [C.cexp|$id:name.shape[$int:i]|]
                         | i <- [0..typeRank paramtype-1] ]
        return (args ++ [C.var name],
                decls ++ [[C.cdecl|$ty:cparamtype $id:name;|]],
                rstms ++ [rstm],
                shapeargs ++ argshape)
  (valargs, decls, rstms, shapeargs) <-
    foldM mkParam ([], [], [], []) paramtypes
  return [C.cstm|{
               $decls:decls
               $ty:crettype $id:ret;
               $stms:rstms
               $id:ret = $id:(funName fname)($args:shapeargs, $args:valargs);
               $stm:printRes
               printf("\n");
             }|]
  where paramtypes = entryPointInput inputs

-- | Compile imperative program to a C program.  Always uses the
-- function named "main" as entry point, so make sure it is defined.
compileProg :: OpCompiler op -> Program op -> String
compileProg ec (Program prog) =
  let ((prototypes, definitions, main), endstate) =
        runCompilerM ec blankNameSource compileProg'
      funName' = funName . nameFromString
  in pretty 80 $ ppr [C.cunit|
$esc:("#include <stdio.h>")
$esc:("#include <stdlib.h>")
$esc:("#include <string.h>")
$esc:("#include <math.h>")

$edecls:(typeDefinitions endstate)

$edecls:(compVarDefinitions endstate)

$edecls:prototypes

double $id:(funName' "toReal")(int x) {
  return x;
}

int $id:(funName' "trunc")(double x) {
  return x;
}

double $id:(funName' "log")(double x) {
  return log(x);
}

double $id:(funName' "sqrt")(double x) {
  return sqrt(x);
}

double $id:(funName' "exp")(double x) {
return exp(x);
}

$edecls:readerFunctions

$edecls:(map funcToDef definitions)

int main() {
  $stms:(compInit endstate)
  $stm:main;
  return 0;
}

|]
  where compileProg' = do
          (prototypes, definitions) <- unzip <$> mapM compileFun prog
          let mainname = nameFromString "main"
          main <- case lookup mainname prog of
                    Nothing   -> fail "GenericC.compileProg: No main function"
                    Just func -> mainCall mainname func
          return (prototypes, definitions, main)
        funcToDef func = C.FuncDef func loc
          where loc = case func of
                        C.OldFunc _ _ _ _ _ _ l -> l
                        C.Func _ _ _ _ _ l      -> l

compileFun :: (Name, Function op) -> CompilerM op (C.Definition, C.Func)
compileFun (fname, Function outputs inputs body) = do
  args' <- mapM compileInput inputs
  body' <- collect $ do
             mapM_ compileOutput outputs
             compileFunBody outputs body
  crettype <- typeToCType $ map paramType outputs
  return ([C.cedecl|static $ty:crettype $id:(funName fname)( $params:args' );|],
          [C.cfun|static $ty:crettype $id:(funName fname)( $params:args' ) { $items:body' }|])
  where compileInput (Param name (Value t)) = do
          let ctp = valueTypeToCType t
          newVar name t
          return [C.cparam|$ty:ctp $id:(textual name)|]
        compileOutput (Param name (Value t)) = do
          let ctp = valueTypeToCType t
          newVar name t
          decl [C.cdecl|$ty:ctp $id:(textual name);|]

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

dimSizeToExp :: DimSize -> Exp
dimSizeToExp (ConstSize x) = Constant $ IntVal x
dimSizeToExp (VarSize v) = Read v []

compileExp :: Exp -> CompilerM op C.Exp

compileExp (Constant val) = return $ compileBasicValue val

compileExp (Read src []) =
  return [C.cexp|$id:(textual src)|]

compileExp (Read src idxs) = do
  srctype@(Type srcbt srcshape) <- lookupVar src
  idxs' <- mapM compileExp idxs
  let src' = C.var $ textual src
  case drop (length idxs') srcshape of
    [] -> return $ indexArrayExp src' (typeShape srctype) idxs'
    resshape -> do
      name <- new "dest"
      let restype = Type srcbt resshape
          dest    = C.var name
          crestype = valueTypeToCType restype
      decl [C.cdecl|$ty:crestype $id:name;|]
      stms $ indexArrayElemStms dest src' (typeShape srctype) idxs'
      return dest

compileExp (UnOp Negate x) = do
  x' <- compileExp x
  return [C.cexp|-$exp:x'|]

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
                                   $string:(show e), $string:(locStr loc));
                   abort();
                 }
          }|]

compileCode (DeclareArray name et shape) = do
  let ty = Type et shape
  let ct = valueTypeToCType ty
  newVar name ty
  decl [C.cdecl|$ty:ct $id:(textual name);|]

compileCode (Allocate name) = do
  (Type bt shape) <- lookupVar name
  let ct = valueTypeToCType $ Type bt []
  shape' <- mapM (compileExp . dimSizeToExp) shape
  unless (null shape) $
    stm $ allocArray (C.var $ textual name) shape' ct

compileCode (For i bound body) = do
  let i' = textual i
  bound' <- compileExp bound
  body'  <- collect $ compileCode body
  decl [C.cdecl|int $id:i';|]
  stm [C.cstm|for ($id:i' = 0; $id:i' < $exp:bound'; $id:i'++) {
            $items:body'
          }|]

compileCode (If cond tbranch fbranch) = do
  cond' <- compileExp cond
  tbranch' <- collect $ compileCode tbranch
  fbranch' <- collect $ compileCode fbranch
  stm [C.cstm|if ($exp:cond') { $items:tbranch' } else { $items:fbranch' }|]

compileCode (Copy dest destoffset src srcoffset size) = do
  undefined
  {-
  dest <- new "copy_dst"
  t <- lookupVar src
  ct <- typeToCType [t]
  let src' = C.var $ textual src
      copy = arraySliceCopyStm [C.cexp|$id:dest.data|]
             [C.cexp|$exp:src'.data|] (typeShape t)
             0
      alloc = [C.cstm|$id:dest.data =
                 calloc($exp:(C.product $ typeShape t), sizeof(*$id:dest.data));
               |]
  decl [C.cdecl|$ty:ct $id:dest;|]
  stm [C.cstm|$id:dest = $exp:src';|]
  stm alloc
  stm copy
  return $ C.var dest
-}

compileCode (Write dest idxs src) = do
  src' <- compileExp src
  idxs' <- mapM compileExp idxs
  toshape <- lookupShape dest
  let dest' = C.var $ textual dest
      rank = length toshape
  case idxs' of
    [] -> stm [C.cstm|$exp:dest' = $exp:src';|]
    _ | rank == length idxs' ->
      let to = indexArrayExp dest' toshape idxs'
      in stm [C.cstm|$exp:to = $exp:src';|]
      | otherwise            ->
      let to = [C.cexp|&$exp:(indexArrayExp dest' toshape idxs')|]
          from = [C.cexp|$exp:src'.data|]
      in stm $ arraySliceCopyStm to from toshape (length idxs')

compileCode (Call results fname args) = do
  args' <- mapM compileExp args
  restypes <- mapM lookupVar results
  crestype <- typeToCType $ map Value restypes
  case results of
    [result] ->
      stm [C.cstm|$id:(textual result) = $id:(funName fname)($args:args');|]
    _        -> do
      ret <- new "call_ret"
      decl [C.cdecl|$ty:crestype $id:ret;|]
      stm [C.cstm|$id:ret = $id:(funName fname)($args:args');|]
      forM_ (zip [0..] results) $ \(i,result) ->
        stm [C.cstm|$id:(textual result) = $exp:(tupleFieldExp (C.var ret) i);|]

compileFunBody :: [Param] -> Code op -> CompilerM op ()
compileFunBody outputs code = do
  retval <- new "retval"
  bodytype <- typeToCType $ map paramType outputs
  compileCode code
  decl [C.cdecl|$ty:bodytype $id:retval;|]
  let name = textual . paramName
      setRetVal' i output =
        stm [C.cstm|$exp:(tupleFieldExp (C.var retval) i) = $id:(name output);|]
  case outputs of
    [output] -> stm [C.cstm|$id:retval = $id:(name output);|]
    _        -> zipWithM_ setRetVal' [0..] outputs
  stm [C.cstm|return $id:retval;|]
