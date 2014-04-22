{-# LANGUAGE QuasiQuotes, GeneralizedNewtypeDeriving #-}
-- | C code generator framework.
module Futhark.CodeGen.Backends.GenericC
  ( compileProg
  , ExpCompiler
  , ExpCompilerResult(..)
  , CompilerM
  , lookupVar
--  , compileExp
--  , compileSubExp
--  , compileExpNewVar
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

data CompilerState = CompilerState {
    compTypeStructs :: [([Type], (C.Type, C.Definition))]
  , compVarDefinitions :: [C.Definition]
  , compInit :: [C.Stm]
  , compNameSrc :: VNameSource
  , compVtable :: HM.HashMap VName Type
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
-- expression compilation function.
type ExpCompiler = C.Exp -> Exp -> CompilerM ExpCompilerResult

-- | The result of the substitute expression compiler.
data ExpCompilerResult = CompileBody Code
                       -- ^ New bindings.  Note that the bound
                       -- expressions will themselves be compiled
                       -- using the expression compiler.
                       | CompileExp Exp -- ^ A new expression (or
                                        -- possibly the same as the
                                        -- input).
                       | CCode [C.BlockItem] -- ^ Compiled C code.

data CompilerEnv = CompilerEnv {
    envCompileExp :: ExpCompiler
  }

newCompilerEnv :: ExpCompiler -> CompilerEnv
newCompilerEnv ec = CompilerEnv {
                      envCompileExp = ec
                    }

-- | Return a list of struct definitions for the tuples and arrays
-- seen during compilation.  The list is sorted according to
-- dependencies, such that a struct at index N only depends on structs
-- at positions >N.
typeDefinitions :: CompilerState -> [C.Definition]
typeDefinitions = reverse . map (snd . snd) . compTypeStructs

newtype CompilerM a = CompilerM (RWS CompilerEnv [C.BlockItem] CompilerState a)
  deriving (Functor, Applicative, Monad,
            MonadState CompilerState,
            MonadReader CompilerEnv,
            MonadWriter [C.BlockItem])

instance MonadFreshNames CompilerM where
  getNameSource = gets compNameSrc
  putNameSource src = modify $ \s -> s { compNameSrc = src }

runCompilerM :: ExpCompiler -> VNameSource -> CompilerM a -> (a, CompilerState)
runCompilerM ec src (CompilerM m) =
  let (x, s, _) = runRWS m (newCompilerEnv ec) (newCompilerState src)
  in (x, s)

collect :: MonadWriter w m => m () -> m w
collect m = pass $ do
  ((), w) <- listen m
  return (w, const mempty)

item :: C.BlockItem -> CompilerM ()
item x = tell [x]

stm :: C.Stm -> CompilerM ()
stm (C.Block items _) = mapM_ item items
stm (C.Default s _) = stm s
stm s = item [C.citem|$stm:s|]

stms :: [C.Stm] -> CompilerM ()
stms = mapM_ stm

decl :: C.InitGroup -> CompilerM ()
decl x = item [C.citem|$decl:x;|]

newVar :: VName -> Type -> CompilerM ()
newVar k t = modify $ \s -> s { compVtable = HM.insert k t $ compVtable s }

lookupVar :: VName -> CompilerM Type
lookupVar k = do t <- gets $ HM.lookup k . compVtable
                 case t of
                   Nothing -> fail $ "Uknown variable " ++ textual k ++ " in code generator."
                   Just t' -> return t'

-- | 'new s' returns a fresh variable name, with 's' prepended to it.
new :: String -> CompilerM String
new = liftM textual . newVName

typeName' :: Type -> String
typeName' (Type Int  []) = "int"
typeName' (Type Bool []) = "bool"
typeName' (Type Char []) = "char"
typeName' (Type Real []) = "real"
typeName' (Type Cert []) = "cert"
typeName' (Type bt   shape) =
  typeName' (Type bt []) ++ show (length shape) ++ "d"

typeName :: [Type] -> String
typeName [t] = typeName' t
typeName ts  = "tuple_" ++ intercalate "_" (map typeName' ts)

typeToCType :: [Type] -> CompilerM C.Type
typeToCType [Type Int  []] = return [C.cty|int|]
typeToCType [Type Bool []] = return [C.cty|int|]
typeToCType [Type Char []] = return [C.cty|char|]
typeToCType [Type Real []] = return [C.cty|double|]
typeToCType [Type Cert []] = return [C.cty|int|]
typeToCType t@[Type bt shape] = do
  ty <- gets $ find (sameRepresentation t . fst) . compTypeStructs
  case ty of
    Just (_, (cty, _)) -> return cty
    Nothing -> do
      ct <- typeToCType [Type bt []]
      let name = typeName t
          rank = length shape
          ctp = [C.cty|$ty:ct*|]
          struct = [C.cedecl|struct $id:name { typename int64_t shape[$int:rank]; $ty:ctp data; };|]
          stype  = [C.cty|struct $id:name|]
      modify $ \s -> s { compTypeStructs = (t, (stype, struct)) : compTypeStructs s }
      return stype
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
printStm :: C.Exp -> [Type] -> CompilerM C.Stm
printStm place [Type Int []]  = return [C.cstm|printf("%d", $exp:place);|]
printStm place [Type Char []] = return [C.cstm|printf("%c", $exp:place);|]
printStm place [Type Bool []] =
  return [C.cstm|printf($exp:place ? "True" : "False");|]
printStm place [Type Real []] = return [C.cstm|printf("%.6f", $exp:place);|]
printStm _     [Type Cert []] = return [C.cstm|printf("Checked");|]
printStm place [Type Char [_]] =
  return [C.cstm|printf("%s", $exp:place.data);|]
printStm place [t@(Type bt (_:rest))] = do
  i <- new "print_i"
  v <- new "print_elem"
  et' <- typeToCType [Type bt rest]
  pstm <- printStm (varExp v) [Type bt rest]
  let indexi = indexArrayElemStms (varExp v) place t [varExp i]
  return [C.cstm|{
               int $id:i;
               $ty:et' $id:v;
               if ($exp:place.shape[0] == 0) {
                   printf("empty(%s)", $exp:(ppType $ Type bt rest));
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

readStm :: C.Exp -> Type -> C.Stm
readStm place (Type Cert []) =
  [C.cstm|$exp:place = 1;|]
readStm place t@(Type et [])
  | Just f <- readFun et =
    [C.cstm|if ($id:f(&$exp:place) != 0) {
          fprintf(stderr, "Syntax error when reading %s.\n", $string:(ppType t));
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
       fprintf(stderr, "Syntax error when reading %s.\n", $string:(ppType t));
       exit(1);
     }|]
  where rank = length shape
readStm _ t =
  [C.cstm|{
        fprintf(stderr, "Cannot read %s yet.\n", $string:(ppType t));
        exit(1);
      }|]

mainCall :: Name -> Function -> CompilerM C.Stm
mainCall fname (Function outputs inputs _) = do
  crettype <- typeToCType $ map paramType outputs
  ret <- new "main_ret"
  printRes <- printStm (varExp ret) $ map paramType outputs
  let mkParam (args, decls, rstms, []) paramtype = do
        name <- new "main_arg"
        cparamtype <- typeToCType [paramtype]
        let rstm = readStm (varExp name) paramtype
            argshape = [ [C.cexp|$id:name.shape[$int:i]|]
                         | i <- [0..typeRank paramtype-1] ]
        return (args ++ [varExp name],
                decls ++ [[C.cdecl|$ty:cparamtype $id:name;|]],
                rstms ++ [rstm],
                argshape)
      mkParam (args, decls, rstms, shape : shapes) _ =
        return (args ++ [shape], decls, rstms, shapes)
  (args, decls, rstms, _) <- foldM mkParam ([], [], [], []) paramtypes
  return [C.cstm|{
               $decls:decls
               $ty:crettype $id:ret;
               $stms:rstms
               $id:ret = $id:(funName fname)($args:args);
               $stm:printRes
               printf("\n");
             }|]
  where paramtypes = map paramType inputs

-- | Compile Futhark program to a C program.  Always uses the function
-- named "main" as entry point, so make sure it is defined.
compileProg :: ExpCompiler -> Program -> String
compileProg ec prog =
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

compileFun :: (Name, Function) -> CompilerM (C.Definition, C.Func)
compileFun (fname, Function outputs inputs body) = do
  args' <- mapM compileInput inputs
  body' <- collect $ do
             mapM_ compileOutput outputs
             compileFunBody outputs body
  crettype <- typeToCType $ map paramType outputs
  return ([C.cedecl|static $ty:crettype $id:(funName fname)( $params:args' );|],
          [C.cfun|static $ty:crettype $id:(funName fname)( $params:args' ) { $items:body' }|])
  where compileInput (Param name t) = do
          ctp <- typeToCType [t]
          newVar name t
          return [C.cparam|$ty:ctp $id:(textual name)|]
        compileOutput (Param name t) = do
          ctp <- typeToCType [t]
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

compileValue :: Value -> CompilerM C.Exp

compileValue (BasicVal bv) =
  return $ compileBasicValue bv

compileValue (ArrayVal shape arr bt) = do
  val <- new "ArrayVal"
  dt <- new "ArrayData"
  ct <- typeToCType [Type bt $ map ConstSize shape]
  cbt <-typeToCType [Type bt []]
  let asint n = [C.cinit|$int:n|]
      cshape = map asint shape
      arrdef = [C.cedecl|$ty:ct $id:val = { $inits:cshape, NULL };|]
  case concatMap basicElemInit $ A.elems arr of
    [] -> modify $ \s -> s { compVarDefinitions = arrdef : compVarDefinitions s }
    elems -> let elemdef = [C.cedecl|$ty:cbt $id:dt[] = { $inits:elems };|]
                 initstm = [C.cstm|$id:val.data = $id:dt;|]
             in modify $ \s -> s {
                                 compInit = initstm : compInit s
                               , compVarDefinitions = arrdef : elemdef :
                                                      compVarDefinitions s
                               }
  return [C.cexp|$id:val|]
  where basicElemInit (IntVal x) = [[C.cinit|$int:x|]]
        basicElemInit (RealVal x) = [[C.cinit|$double:(toRational x)|]]
        basicElemInit (CharVal c) = [[C.cinit|$char:c|]]
        basicElemInit Checked = [[C.cinit|0|]]
        basicElemInit (LogVal True) = [[C.cinit|1|]]
        basicElemInit (LogVal False) = [[C.cinit|0|]]

dimSizeToExp :: DimSize -> Exp
dimSizeToExp (ConstSize x) = Constant $ BasicVal $ IntVal x
dimSizeToExp (VarSize v) = Read v []

compileExp :: Exp -> CompilerM C.Exp

compileExp (Constant val) = compileValue val

compileExp (Read src []) =
  return [C.cexp|$id:(textual src)|]

compileExp (Read src idxs) = do
  srctype@(Type srcbt srcshape) <- lookupVar src
  idxs' <- mapM compileExp idxs
  let src' = varExp $ textual src
  case drop (length idxs') srcshape of
    [] -> return $ indexArrayExp src' (length srcshape) idxs'
    resshape -> do
      name <- new "dest"
      let restype = Type srcbt resshape
          dest    = varExp name
      crestype <- typeToCType [restype]
      decl [C.cdecl|$ty:crestype $id:name;|]
      stms $ indexArrayElemStms dest src' srctype idxs'
      return dest

compileExp (Copy src) = do
  dest <- new "copy_dst"
  t <- lookupVar src
  ct <- typeToCType [t]
  let src' = varExp $ textual src
      copy = arraySliceCopyStm [C.cexp|$id:dest.data|]
             [C.cexp|$exp:src'.data|] [C.cexp|$exp:src'.shape|]
             t 0
      alloc = [C.cstm|$id:dest.data =
                 malloc($exp:(arraySizeExp src' t) * sizeof(*$id:dest.data));
               |]
  decl [C.cdecl|$ty:ct $id:dest;|]
  stm [C.cstm|$id:dest = $exp:src';|]
  stm alloc
  stm copy
  return $ varExp dest

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

compileCode :: Code -> CompilerM ()

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

compileCode (Declare name et shape) = do
  let ty = Type et shape
  ct <- typeToCType [ty]
  newVar name ty
  decl [C.cdecl|$ty:ct $id:(textual name);|]

compileCode (Allocate name) = do
  ty@(Type _ shape) <- lookupVar name
  ct <- typeToCType [ty]
  shape' <- mapM (compileExp . dimSizeToExp) shape
  unless (null shape) $
    stm $ allocArray (varExp $ textual name) shape' ct

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

compileCode (Write dest idxs src) = do
  src' <- compileExp src
  idxs' <- mapM compileExp idxs
  destt@(Type bt shape) <- lookupVar dest
  let dest' = varExp $ textual dest
      rank = typeRank destt
      elty = Type bt $ drop (length idxs') shape
  case idxs' of
    [] -> stm [C.cstm|$exp:dest' = $exp:src';|]
    _ | rank == length idxs' ->
      let to = indexArrayExp dest' rank idxs'
      in stm [C.cstm|$exp:to = $exp:src';|]
      | otherwise            ->
      let to = indexArrayExp dest' (typeRank destt) idxs'
          from = [C.cexp|$exp:src'.data|]
          fromshape = [C.cexp|$exp:src'.shape|]
      in stm $ arraySliceCopyStm to from fromshape elty 0

compileCode (Call results fname args) = do
  args' <- mapM compileExp args
  restypes <- mapM lookupVar results
  crestype <- typeToCType restypes
  case results of
    [result] ->
      stm [C.cstm|$id:(textual result) = $id:(funName fname)($args:args');|]
    _        -> do
      ret <- new "call_ret"
      decl [C.cdecl|$ty:crestype $id:ret;|]
      stm [C.cstm|$id:ret = $id:(funName fname)($args:args');|]
      forM_ (zip [0..] results) $ \(i,result) ->
        stm [C.cstm|$id:(textual result) = $exp:(tupleFieldExp (varExp ret) i);|]

compileFunBody :: [Param] -> Code -> CompilerM ()
compileFunBody outputs code = do
  retval <- new "retval"
  bodytype <- typeToCType $ map paramType outputs
  compileCode code
  decl [C.cdecl|$ty:bodytype $id:retval;|]
  let name = textual . paramName
      setRetVal' i output =
        stm [C.cstm|$exp:(tupleFieldExp (varExp retval) i) = $id:(name output);|]
  case outputs of
    [output] -> stm [C.cstm|$id:retval = $id:(name output);|]
    _        -> zipWithM_ setRetVal' [0..] outputs
  stm [C.cstm|return $id:retval;|]
