{-# LANGUAGE QuasiQuotes, GeneralizedNewtypeDeriving #-}
module L0.CCodeGen (compileProg) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Array as A
import qualified Data.Map as M
import Data.List

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Text.PrettyPrint.Mainland

import L0.AbSyn
import L0.FreshNames

data CompilerState = CompilerState {
    compTypeStructs :: [(Type, (C.Type, C.Definition))]
  , compVarDefinitions :: [C.Definition]
  , compInit :: [C.Stm]
  , compNameSrc :: NameSource
  }

newCompilerState :: Prog Type -> CompilerState
newCompilerState prog = CompilerState {
                          compTypeStructs = []
                        , compVarDefinitions = []
                        , compInit = []
                        , compNameSrc = newNameSourceForProg prog
                        }

data CompilerEnv = CompilerEnv {
    envVarMap :: M.Map String C.Exp
  }

newCompilerEnv :: CompilerEnv
newCompilerEnv = CompilerEnv {
                   envVarMap = M.empty
                 }

-- | Return a list of struct definitions for the tuples and arrays
-- seen during compilation.  The list is sorted according to
-- dependencies, such that a struct at index N only depends on structs
-- at positions >N.
typeDefinitions :: CompilerState -> [C.Definition]
typeDefinitions = reverse . map (snd . snd) . compTypeStructs

newtype CompilerM a = CompilerM (ReaderT CompilerEnv (State CompilerState) a)
  deriving (Functor, Applicative, Monad,
            MonadState CompilerState, MonadReader CompilerEnv)

binding :: [(String, C.Exp)] -> CompilerM a -> CompilerM a
binding kvs = local (flip (foldl add) kvs)
  where add env (k, v) = env { envVarMap = M.insert k v $ envVarMap env }

lookupVar :: String -> CompilerM C.Exp
lookupVar k = do v <- asks $ M.lookup k . envVarMap
                 case v of
                   Nothing -> error $ "Uknown variable " ++ k ++ " in code generator."
                   Just v' -> return v'

-- | 'new s' returns a fresh variable name, with 's' prepended to it.
new :: String -> CompilerM String
new k = do (name, src) <- gets $ newName k . compNameSrc
           modify $ \s -> s { compNameSrc = src }
           return name

-- | Turn a name into a C expression consisting of just that name.
varExp :: String -> C.Exp
varExp k = [C.cexp|$id:k|]

typeToCType :: Type -> CompilerM C.Type
typeToCType (Int _) = return [C.cty|int|]
typeToCType (Bool _) = return [C.cty|int|]
typeToCType (Char _) = return [C.cty|char|]
typeToCType (Real _) = return [C.cty|double|]
typeToCType t@(Tuple ts _) = do
  ty <- gets $ lookup t . compTypeStructs
  case ty of
    Just (cty, _) -> return cty
    Nothing -> do
      members <- zipWithM field ts [(0::Int)..]
      name <- new "tuple_type"
      let struct = [C.cedecl|struct $id:name { $sdecls:members };|]
          stype  = [C.cty|struct $id:name|]
      modify $ \s -> s { compTypeStructs = (t, (stype,struct)) : compTypeStructs s }
      return stype
        where field et i = do
                 ct <- typeToCType et
                 return [C.csdecl|$ty:ct $id:(tupleField i);|]
typeToCType t@(Array _ _ _) = do
  ty <- gets $ lookup t . compTypeStructs
  case ty of
    Just (cty, _) -> return cty
    Nothing -> do
      ct <- typeToCType $ baseType t
      name <- new "array_type"
      let ctp = [C.cty|$ty:ct*|]
          struct = [C.cedecl|struct $id:name { int dims[$int:(arrayDims t)]; $ty:ctp data; };|]
          stype  = [C.cty|struct $id:name|]
      modify $ \s -> s { compTypeStructs = (t, (stype, struct)) : compTypeStructs s }
      return stype

expCType :: Exp Type -> CompilerM C.Type
expCType = typeToCType . expType

valCType :: Value -> CompilerM C.Type
valCType = typeToCType . valueType

tupleField :: Int -> String
tupleField i = "elem_" ++ show i

tupleFieldExp :: C.Exp -> Int -> C.Exp
tupleFieldExp e i = [C.cexp|$exp:e.$id:(tupleField i)|]

-- | @funName f@ is the name of the C function corresponding to
-- the L0 function @f@.
funName :: String -> String
funName = ("l0_"++)

-- | The size is in elements.
allocArray :: C.Exp -> [C.Exp] -> C.Type -> C.Stm
allocArray place shape basetype =
  [C.cstm|{$stms:shapeassign
           $exp:place.data = calloc($exp:sizeexp, sizeof($ty:basetype));}|]
  where sizeexp = foldl mult [C.cexp|1|] shape
        mult x y = [C.cexp|$exp:x * $exp:y|]
        shapeassign = zipWith assign shape [0..]
        assign :: C.Exp -> Int -> C.Stm
        assign n i = [C.cstm|$exp:place.dims[$int:i] = $exp:n;|]

-- | @arraySliceCopyStm to from t slice@ is a @memcpy()@ statement copying
-- a slice of the array @from@ to the memory pointed at by @to@.
arraySliceCopyStm :: C.Exp -> C.Exp -> Type -> Int -> C.Stm
arraySliceCopyStm to from t slice =
  [C.cstm|memcpy($exp:to,
                 $exp:from.data,
                 $exp:(arraySliceSizeExp from t slice)*sizeof($exp:from.data[0]));|]

-- | Return an expression giving the array slice size in elements.
-- The integer argument is the number of dimensions sliced off.
arraySliceSizeExp :: C.Exp -> Type -> Int -> C.Exp
arraySliceSizeExp place t slice =
  foldl comb [C.cexp|1|] [slice..arrayDims t-1]
  where comb y i = [C.cexp|$exp:place.dims[$int:i] * $exp:y|]

-- | Return an list of expressions giving the array shape in elements.
arrayShapeExp :: C.Exp -> Type -> [C.Exp]
arrayShapeExp place t =
  map comb [0..arrayDims t-1]
  where comb i = [C.cexp|$exp:place.dims[$int:i]|]

indexArrayExp :: C.Exp -> Type -> [C.Exp] -> C.Exp
indexArrayExp place t indexes =
  let sizes = map (foldl mult [C.cexp|1|]) $ tails $ map field [1..arrayDims t - 1]
      field :: Int -> C.Exp
      field i = [C.cexp|$exp:place.dims[$int:i]|]
      mult x y = [C.cexp|$exp:x * $exp:y|]
      add x y = [C.cexp|$exp:x + $exp:y|]
      index = foldl add [C.cexp|0|] $ zipWith mult sizes indexes
  in [C.cexp|$exp:place.data[$exp:index]|]

-- | @indexArrayElemStm place from t idxs@ produces a statement that
-- stores the value at @t[idxs]@ in @place@.  In contrast to
-- 'indexArrayExp', this function takes care of creating proper size
-- information if the result is an array itself.
indexArrayElemStm :: C.Exp -> C.Exp -> Type -> [C.Exp] -> C.Stm
indexArrayElemStm place from t idxs =
  case stripArray (length idxs) t of
    Array _ _ _ ->
      [C.cstm|{
            memcpy(&$exp:place.dims,
                   $exp:from.dims+$int:(length idxs),
                   sizeof($exp:from.dims)-$int:(length idxs));
            $exp:place.data = &$exp:index;
          }|]
    _ -> [C.cstm|$exp:place = $exp:index;|]
  where index = indexArrayExp from t idxs

boundsCheckStm :: C.Exp -> [C.Exp] -> C.Stm
boundsCheckStm place idxs = [C.cstm|{$stms:(zipWith check idxs [0..])}|]
  where check :: C.Exp -> Int -> C.Stm
        check var i = [C.cstm|if ($exp:var < 0 || $exp:var >= $exp:place.dims[$int:i]) {
                            error(1, "Array index out of bounds.\n");
                          }|]

-- | Return a statement printing the given value.
printStm :: C.Exp -> Type -> CompilerM C.Stm
printStm place (Int _)  = return [C.cstm|printf("%d", $exp:place);|]
printStm place (Char _) = return [C.cstm|printf("%c", $exp:place);|]
printStm place (Bool _) = return [C.cstm|printf($exp:place && "true" || "false");|]
printStm place (Real _) = return [C.cstm|printf("%lf", $exp:place);|]
printStm place (Tuple ets _) = do
  prints <- forM (zip [(0::Int)..] ets) $ \(i, et) ->
              printStm [C.cexp|$exp:place.$id:(tupleField i)|] et
  let prints' = intercalate [[C.cstm|{putchar(','); putchar(' ');}|]] $ map (:[]) prints
  return [C.cstm|{
               putchar('(');
               $stms:prints'
               putchar(')');
             }|]
printStm place (Array (Char _) _ _) =
  return [C.cstm|printf("%s", $exp:place.data);|]
printStm place t@(Array et _ _) = do
  i <- new "print_i"
  v <- new "print_elem"
  et' <- typeToCType et
  stm <- printStm (varExp v) et
  let indexi = indexArrayElemStm (varExp v) place t [varExp i]
  return [C.cstm|{
               int $id:i;
               $ty:et' $id:v;
               putchar('{');
               for ($id:i = 0; $id:i < $exp:place.dims[0]; $id:i++) {
                 $stm:indexi;
                 $stm:stm
                 if ($id:i != $exp:place.dims[0]-1) {
                   putchar(',');
                   putchar(' ');
                 }
               }
               putchar('}');
             }|]

readStm :: C.Exp -> Type -> CompilerM C.Stm
readStm place (Int _) =
  return [C.cstm|scanf("%d", &$exp:place);|]
readStm place (Char _) =
  return [C.cstm|scanf("%c", &$exp:place);|]
readStm place (Real _) =
  return [C.cstm|scanf("%f", &$exp:place);|]
readStm _ t =
  return [C.cstm|{
               fprintf(stderr, "Cannot read %s yet.\n", $string:(ppType t));
               exit(1);
             }|]

mainCall :: FunDec Type -> CompilerM C.Stm
mainCall (fname,rettype,params,_,_) = do
  crettype <- typeToCType rettype
  ret <- new "main_ret"
  printRes <- printStm (varExp ret) rettype
  (args, decls, stms) <- liftM unzip3 . forM paramtypes $ \paramtype -> do
    name <- new "main_arg"
    cparamtype <- typeToCType paramtype
    stm <- readStm (varExp name) paramtype
    return (varExp name, [C.cdecl|$ty:cparamtype $id:name;|], stm)
  return [C.cstm|{
               $decls:decls
               $ty:crettype $id:ret;
               $stms:stms
               $id:ret = $id:(funName fname)($args:args);
               $stm:printRes
             }|]
  where paramtypes = map identType params

-- | Compile L0 program to a C program.  Always uses the function
-- named "main" as entry point, so make sure it is defined.
compileProg :: Prog Type -> String
compileProg prog =
  let ((prototypes, definitions, main), endstate) = runCompilerM $ compileProg'
  in pretty 0 $ ppr [C.cunit|
$esc:("#include <stdio.h>")
$esc:("#include <stdlib.h>")
$esc:("#include <string.h>")
$esc:("#include <math.h>")

$edecls:(typeDefinitions endstate)

$edecls:(compVarDefinitions endstate)

$edecls:prototypes

double $id:(funName "toReal")(int x) {
  return x;
}

int $id:(funName "trunc")(double x) {
  return x;
}

double $id:(funName "log")(double x) {
  return log(x);
}

double $id:(funName "sqrt")(double x) {
  return sqrt(x);
}

double $id:(funName "exp")(double x) {
  return exp(x);
}

void error(int exitcode, const char *s) {
  fprintf(stderr, "%s", s);
  abort();
  exit(exitcode);
}

$edecls:(map funcToDef definitions)

int main() {
  $stms:(compInit endstate)
  $stm:main;
  return 0;
}

|]
  where compileProg' = do
          (prototypes, definitions) <- unzip <$> mapM compileFun prog
          main <- case funDecByName "main" prog of
                    Nothing -> error "L0.CCodeGen.compileProg: No main function"
                    Just func -> mainCall func
          return (prototypes, definitions, main)
        funcToDef func = C.FuncDef func loc
          where loc = case func of
                        C.OldFunc _ _ _ _ _ _ l -> l
                        C.Func _ _ _ _ _ l      -> l
        runCompilerM (CompilerM m) =
          runState (runReaderT m newCompilerEnv) $ newCompilerState prog

compileFun :: FunDec Type -> CompilerM (C.Definition, C.Func)
compileFun (fname, rettype, args, body, _) = do
  (argexps, args') <- unzip <$> mapM compileArg args
  body' <- binding (zip (map identName args) $ argexps) $ compileBody body
  crettype <- typeToCType rettype
  return ([C.cedecl|static $ty:crettype $id:(funName fname)( $params:args' );|],
          [C.cfun|static $ty:crettype $id:(funName fname)( $params:args' ) { $stm:body' }|])
  where compileArg (Ident name t _) = do
          name' <- new name
          ctp <- typeToCType t
          return (varExp name', [C.cparam|$ty:ctp $id:name'|])

compileValue :: C.Exp -> Value -> CompilerM C.Stm

compileValue place (IntVal k _) =
  return [C.cstm|$exp:place = $int:k;|]

compileValue place (RealVal x _) =
  return [C.cstm|$exp:place = $double:(toRational x);|]

compileValue place (LogVal b _) =
  return [C.cstm|$exp:place = $int:b';|]
  where b' :: Int
        b' = if b then 1 else 0

compileValue place (CharVal c _) =
  return [C.cstm|$exp:place = $char:c;|]

compileValue place (TupVal vs _) = do
  vs' <- forM (zip vs [(0::Int)..]) $ \(v, i) -> do
           var <- new $ "TupVal_" ++ show i
           v' <- compileValue (varExp var) v
           let field = tupleField i
           vt <- valCType v
           return [C.cstm|{$ty:vt $id:var; $stm:v'; $exp:place.$id:field = $id:var;}|]
  return [C.cstm|{$stms:vs'}|]

compileValue place v@(ArrayVal _ et _) = do
  val <- new "ArrayVal"
  ct <- typeToCType $ valueType v
  cbt <- typeToCType $ baseType et
  let asexp n = [C.cexp|$int:n|]
      alloc = allocArray (varExp val) (map asexp $ arrayShape v) cbt
  i <- new "i"
  initstms <- elemInit val i v
  let def = [C.cedecl|$ty:ct $id:val;|]
      initstm = [C.cstm|{int $id:i = 0; $stm:alloc; $stms:initstms}|]
  modify $ \s -> s { compInit = initstm : compInit s
                   , compVarDefinitions = def : compVarDefinitions s }
  return [C.cstm|$exp:place = $id:val;|]
  where elemInit name i (ArrayVal arr _ _) = do
          stms <- concat <$> mapM (elemInit name i) (A.elems arr)
          return [[C.cstm|{$stm:stm}|] | stm <- stms]
        elemInit name i elv = do
          vplace <- new "arrelem"
          vstm <- compileValue (varExp vplace) elv
          cty <- typeToCType $ valueType elv
          return [[C.cstm|{$ty:cty $id:vplace;
                           $stm:vstm;
                           $id:name.data[$id:i++] = $id:vplace;}|]]

compileExp :: C.Exp -> Exp Type -> CompilerM C.Stm

compileExp place (Literal val) =
  compileValue place val

compileExp place (Var (Ident name _ _)) = do
  name' <- lookupVar name
  return [C.cstm|$exp:place = $exp:name';|]

compileExp place (TupLit es _) = do
  es' <- forM (zip es [(0::Int)..]) $ \(e, i) -> do
           var <- new $ "TupVal_" ++ show i
           e' <- compileExp (varExp var) e
           let field = tupleField i
           et <- expCType e
           return [C.cstm|{$ty:et $id:var; $stm:e'; $exp:place.$id:field = $id:var;}|]
  return [C.cstm|{$stms:es'}|]

compileExp place a@(ArrayLit [] _ _) =
  compileValue place $ blankValue (expType a)
compileExp place (ArrayLit (e:es) _ _) = do
  name <- new "ArrayLit_elem"
  et <- typeToCType $ expType e
  bt <- typeToCType $ baseType $ expType e
  e' <- compileExp (varExp name) e
  es' <- mapM (compileExpInPlace $ varExp name) es
  i <- new "ArrayLit_i"
  case expType e of
    Array _ _ _ -> do
      eldims <- new "ArrayLit_eldims"
      elsize <- new "ArrayLit_elsize"
      datap <- new "ArrayLit_datap"
      let numdims = arrayDims $ expType e
          dimexps = [C.cexp|$int:(length es+1)|] :
                    [ [C.cexp|$id:eldims[$int:j]|] | j <- [0..numdims-1] ]
          alloc = allocArray place dimexps bt
          es'' = [ [C.cstm|{
                         $stm:e''
                         if (memcmp($id:eldims, $id:name.dims, sizeof($id:eldims)) != 0) {
                             error(1, "Array elements have different sizes.\n");
                         }
                         memcpy($id:datap, $id:name.data, $id:elsize);
                         $id:datap += $id:elsize;
                       }|] | e'' <- es']
      return [C.cstm|{
                   $ty:et $id:name;
                   int $id:eldims[$int:numdims];
                   int $id:elsize = 1 * sizeof($ty:bt);
                   int $id:i;
                   char *$id:datap;
                   $stm:e'
                   memcpy($id:eldims, $id:name.dims, sizeof($id:eldims));
                   for ($id:i = 0; $id:i < $int:numdims; $id:i++) {
                     $id:elsize *= $id:eldims[$id:i];
                   }
                   $stm:alloc
                   $id:datap = (char*) $exp:place.data;
                   memcpy($id:datap, $id:name.data, $id:elsize);
                   $id:datap += $id:elsize;
                   $stms:es''
                 }|]
    _ -> do -- Element type is not array.
      let alloc = allocArray place [[C.cexp|$int:(length es+1)|]] bt
          es'' = [ [C.cstm|{
                         $stm:e''
                         $exp:place.data[$id:i++] = $id:name;
                       }|] | e'' <- e':es']
      return [C.cstm|{
                  $ty:et $id:name;
                  int $id:i = 0;
                  $stm:alloc
                  $stms:es''
                 }|]

compileExp place (BinOp bop e1 e2 _ _) = do
  e1_dest <- new "binop_x1"
  e2_dest <- new "binop_x2"
  e1' <- compileExp (varExp e1_dest) e1
  e2' <- compileExp (varExp e2_dest) e2
  e1t <- expCType e1
  e2t <- expCType e2
  return [C.cstm|{
               $ty:e1t $id:e1_dest;
               $ty:e2t $id:e2_dest;
               $stm:e1'
               $stm:e2'
               $stm:(compileBinOp e1_dest e2_dest)
             }|]
  where compileBinOp x y =
          case bop of
            Plus -> [C.cstm|$exp:place = $id:x + $id:y;|]
            Minus -> [C.cstm|$exp:place = $id:x - $id:y;|]
            Times -> [C.cstm|$exp:place = $id:x * $id:y;|]
            Divide -> [C.cstm|$exp:place = $id:x / $id:y;|]
            Mod -> [C.cstm|$exp:place = $id:x % $id:y;|]
            Pow -> [C.cstm|$exp:place = powl($id:x,$id:y);|]
            ShiftR -> [C.cstm|$exp:place = $id:x >> $id:y;|]
            ShiftL -> [C.cstm|$exp:place = $id:x << $id:y;|]
            Band -> [C.cstm|$exp:place = $id:x & $id:y;|]
            Xor -> [C.cstm|$exp:place = $id:x ^ $id:y;|]
            Bor -> [C.cstm|$exp:place = $id:x | $id:y;|]
            LogAnd -> [C.cstm|$exp:place = $id:x && $id:y;|]
            LogOr -> [C.cstm|$exp:place = $id:x || $id:y;|]
            Equal -> [C.cstm|$exp:place = $id:x == $id:y;|]
            Less -> [C.cstm|$exp:place = $id:x < $id:y;|]
            Leq -> [C.cstm|$exp:place = $id:x <= $id:y;|]

compileExp place (And e1 e2 _) = do
  e1' <- compileExp place e1
  e2' <- compileExp place e2
  return [C.cstm|{$stm:e1' if ($exp:place) { $stm:e2' }}|]

compileExp place (Or e1 e2 _) = do
  e1' <- compileExp place e1
  e2' <- compileExp place e2
  return [C.cstm|{$stm:e1' if (!$exp:place) { $stm:e2' }}|]

compileExp place (Not e1 _) = do
  e1' <- compileExp place e1
  return [C.cstm|{$stm:e1' $exp:place = !$exp:place;}|]

compileExp place (Negate e1 _ _) = do
  e1' <- compileExp place e1
  return [C.cstm|{$stm:e1' $exp:place = -$exp:place;}|]

compileExp place (If cond e1 e2 _ _) = do
  condvar <- new "if_cond"
  cond' <- compileExp (varExp condvar) cond
  e1' <- compileExp place e1
  e2' <- compileExp place e2
  return [C.cstm|{
               int $id:condvar;
               $stm:cond'
               if ($id:condvar) $stm:e1'
               else $stm:e2'
             }|]

compileExp place (Apply fname args _ _) = do
  (vars, args') <- liftM unzip . forM args $ \arg -> do
                     var <- new "apply_arg"
                     arg' <- compileExp (varExp var) arg
                     argtype <- expCType arg
                     return (([C.cdecl|$ty:argtype $id:var;|], varExp var), arg')
  let (vardecls, varexps) = unzip vars
  return [C.cstm|{
               $decls:vardecls
               $stms:args'
               $exp:place = $id:(funName fname)($args:varexps);
             }|]

compileExp place (LetPat pat e body _) = do
  val <- new "let_value"
  e' <- compileExp (varExp val) e
  let bindings = compilePattern pat (varExp val)
  body' <- binding bindings $ compileExp place body
  et <- expCType e
  return [C.cstm|{
               $ty:et $id:val;
               $stm:e'
               $stm:body'
             }|]

compileExp place (Index var idxs _ _ _) = do
  arr <- lookupVar $ identName var
  idxvars <- mapM (new . ("index_"++) . show) [0..length idxs-1]
  idxs' <- zipWithM compileExp (map varExp idxvars) idxs
  let vardecls = [[C.cdecl|int $id:idxvar;|] | idxvar <- idxvars]
      varexps =  map varExp idxvars
      index = indexArrayElemStm place arr (identType var) varexps
  return [C.cstm|{
               $decls:vardecls
               $stms:idxs'
               $stm:(boundsCheckStm arr varexps)
               $stm:index
             }|]

compileExp place (Size e _) = do
  dest <- new "size_value"
  et <- typeToCType $ expType e
  e' <- compileExp (varExp dest) e
  return [C.cstm|{$ty:et $id:dest; $stm:e' $exp:place = $id:dest.dims[0];}|]

compileExp place e@(Iota (Var v) _) = do
  e' <- compileExpInPlace place e
  v' <- lookupVar $ identName v
  let alloc = allocArray place [v'] [C.cty|int|]
  return [C.cstm|{
               $stm:alloc
               $stm:e'
             }|]

compileExp place (Iota ne pos) = do
  size <- new "iota_size"
  let ident = Ident size (Int pos) pos
  compileExp place $ LetPat (Id ident) ne (Iota (Var ident) pos) pos

compileExp place e@(Replicate (Var nv) (Var vv) _) = do
  e' <- compileExpInPlace place e
  bt <- typeToCType $ baseType $ identType vv
  nv' <- lookupVar $ identName nv
  vv' <- lookupVar $ identName vv
  let dims = nv' : arrayShapeExp vv' (identType vv)
      alloc = allocArray place dims bt
  return [C.cstm|{
               $stm:alloc
               $stm:e'
             }|]

compileExp place (Replicate ne ve pos) = do
  nv <- new "replicate_n"
  vv <- new "replicate_v"
  let nident = Ident nv (Int pos) pos
      vident = Ident vv (expType ve) pos
      nlet body = LetPat (Id nident) ne body pos
      vlet body = LetPat (Id vident) ve body pos
  compileExp place $ nlet $ vlet $ Replicate (Var nident) (Var vident) pos

compileExp place (Reshape shapeexps arrexp intype _ _) = do
  shapevars <- mapM (new . ("shape_"++) . show) [0..length shapeexps-1]
  arr <- new "reshape_arr"
  intype' <- typeToCType intype
  shapeexps' <- zipWithM compileExp (map varExp shapevars) shapeexps
  arrexp' <- compileExp (varExp arr) arrexp
  let vardecls = [[C.cdecl|int $id:var;|] | var <- shapevars]
      assignstms = [[C.cstm|$exp:place.dims[$int:i] = $id:var;|] | (var, i) <- zip shapevars [(0::Int)..]]
  return [C.cstm|{
               $ty:intype' $id:arr;
               $decls:vardecls
               $stms:shapeexps'
               $stms:assignstms
               $stm:arrexp'
               $exp:place.data = $id:arr.data;
             }|]

compileExp place e@(Transpose arrexp _ _ _) = do
  arr <- new "transpose_arr"
  i <- new "transpose_i"
  j <- new "transpose_j"
  intype <- typeToCType $ expType arrexp
  basetype <- typeToCType $ baseType $ expType arrexp
  arrexp' <- compileExp (varExp arr) arrexp
  let alloc = case arrayShapeExp (varExp arr) (expType arrexp) of
                rows:cols:more -> allocArray place (cols:rows:more) basetype
                _              -> error "One-dimensional array in transpose; should have been caught by type checker"
      indexfrom = indexArrayExp (varExp arr) (expType arrexp) [varExp i, varExp j]
      indexto   = indexArrayExp place (expType e) [varExp j, varExp i]
      size = arraySliceSizeExp (varExp arr) (expType arrexp) 2
  return [C.cstm|{
               $ty:intype $id:arr;
               int $id:i, $id:j;
               $stm:arrexp'
               $stm:alloc
               for ($id:i = 0; $id:i < $id:arr.dims[0]; $id:i++) {
                 for ($id:j = 0; $id:j < $id:arr.dims[1]; $id:j++) {
                   memcpy(&$exp:indexto, &$exp:indexfrom, $exp:size * sizeof($id:arr.data[0]));
                 }
               }
             }|]

compileExp place (Split posexp arrexp _ _) = do
  arr <- new "split_arr"
  pos <- new "split_pos"
  arrexp' <- compileExp (varExp arr) arrexp
  posexp' <- compileExp (varExp pos) posexp
  arrt <- typeToCType $ expType arrexp
  let splitexp = indexArrayExp (varExp arr) (expType arrexp) [varExp pos]
      place0 = tupleFieldExp place 0
      place1 = tupleFieldExp place 1
  return [C.cstm|{
               $ty:arrt $id:arr;
               int $id:pos;
               $stm:posexp'
               $stm:arrexp'
               if ($id:pos < 0 || $id:pos > $id:arr.dims[0]) {
                 error(1, "Split out of bounds.\n");
               }
               memcpy($exp:place0.dims, $id:arr.dims, sizeof($id:arr.dims));
               memcpy($exp:place1.dims, $id:arr.dims, sizeof($id:arr.dims));
               $exp:place0.data = $id:arr.data;
               $exp:place0.dims[0] = $id:pos;
               $exp:place1.data = &$exp:splitexp;
               $exp:place1.dims[0] -= $id:pos;
             }|]

compileExp place (Concat xarr yarr _ _) = do
  x <- new "concat_x"
  y <- new "concat_y"
  xarr' <- compileExp (varExp x) xarr
  yarr' <- compileExp (varExp y) yarr
  arrt <- typeToCType $ expType xarr
  bt' <- typeToCType $ baseType $ expType xarr
  let alloc = case (arrayShapeExp (varExp x) (expType xarr),
                    arrayShapeExp (varExp y) (expType yarr)) of
                (xrows:rest, yrows:_) ->
                  allocArray place ([C.cexp|$exp:xrows+$exp:yrows|]:rest) bt'
                _ -> error "Zero-dimensional array in concat."
      xsize = arraySliceSizeExp (varExp x) (expType xarr) 0
      copyx = arraySliceCopyStm
              [C.cexp|$exp:place.data|] (varExp x) (expType xarr) 0
      copyy = arraySliceCopyStm
              [C.cexp|$exp:place.data+$exp:xsize|] (varExp y) (expType yarr) 0
  return [C.cstm|{
               $ty:arrt $id:x, $id:y;
               $stm:xarr'
               $stm:yarr'
               if ($exp:(arraySliceSizeExp (varExp x) (expType xarr) 1) !=
                   $exp:(arraySliceSizeExp (varExp y) (expType yarr) 1)) {
                 error(1, "Arguments to concat differ in size.");
               }
               $stm:alloc
               $stm:copyx
               $stm:copyy
             }|]

compileExp place (LetWith name src idxs ve body _) = do
  name' <- new $ identName name
  src' <- lookupVar $ identName src
  etype <- typeToCType $ identType src
  basetype <- typeToCType $ baseType $ identType src
  idxvars <- mapM (const $ new "letwith_index") [1..length idxs]
  let idxexps = map varExp idxvars
  idxs' <- zipWithM compileExp idxexps idxs
  el <- new "letwith_el"
  elty <- typeToCType $ expType ve
  ve' <- compileExpInPlace (varExp el) ve
  let idxdecls = [[C.cdecl|int $id:idxvar;|] | idxvar <- idxvars]
      (alloc, copy) =
        case identType src of
          Array _ _ _ ->
            ([C.cstm|{
                   $stm:(allocArray (varExp name')
                         (arrayShapeExp src' (identType src)) basetype)
                   memcpy($id:name'.dims, $exp:src'.dims, sizeof($exp:src'.dims));
                   }|],
             arraySliceCopyStm [C.cexp|$id:name'.data|] src' (identType src) 0)
          _ -> ([C.cstm|;|], [C.cstm|$id:name' = $exp:src';|])
      (elempre, elempost) =
        case expType ve of
          Array _ _ _ -> (indexArrayElemStm (varExp el) (varExp name') (identType src) idxexps,
                          [C.cstm|;|])
          _ -> ([C.cstm|;|],
                case identType src of
                  Array _ _ _ ->
                    [C.cstm|$exp:(indexArrayExp (varExp name') (identType src) idxexps) = $id:el;|]
                  _ -> [C.cstm|$id:name' = $id:el;|])
  body' <- binding [(identName name, varExp name')] $ compileExp place body
  return [C.cstm|{
               $ty:etype $id:name';
               $ty:elty $id:el;
               $decls:idxdecls
               $stm:alloc
               $stm:copy
               $stms:idxs'
               $stm:(boundsCheckStm (varExp name') idxexps)
               $stm:elempre
               $stm:ve'
               $stm:elempost
               $stm:body'
             }|]

compileExp place (DoLoop mergepat mergeexp loopvar boundexp loopbody letbody _) = do
  loopvar' <- new $ identName loopvar
  bound <- new "loop_bound"
  mergevarR <- new $ "loop_mergevar_read"
  mergevarW <- new $ "loop_mergevar_write"
  let bindings = compilePattern mergepat (varExp mergevarR)
  mergeexp' <- compileExp (varExp mergevarR) mergeexp
  mergetype <- typeToCType $ expType loopbody
  boundexp' <- compileExp (varExp bound) boundexp
  loopbody' <- binding ((identName loopvar, varExp loopvar') : bindings) $
               compileExp (varExp mergevarW) loopbody
  letbody' <- binding bindings $ compileExp place letbody
  return [C.cstm|{
               int $id:bound, $id:loopvar';
               $ty:mergetype $id:mergevarR;
               $ty:mergetype $id:mergevarW;
               $stm:mergeexp'
               $stm:boundexp'
               for ($id:loopvar' = 0; $id:loopvar' < $id:bound; $id:loopvar'++) {
                 $stm:loopbody'
                 $id:mergevarR = $id:mergevarW;
               }
               $stm:letbody'
             }|]

compileExp place (Copy e _) = do
  val <- new "copy_val"
  e' <- compileExp (varExp val) e
  t <- typeToCType $ expType e
  let copy = case expType e of
               Array _ _ _ -> arraySliceCopyStm [C.cexp|$exp:place.data|]
                                (varExp val) (expType e) 0
               _ -> [C.cstm|;|]
  return [C.cstm|{
               $ty:t $id:val;
               $stm:e'
               $exp:place = $id:val;
               $stm:copy
             }|]

compileExp _ (Zip _ _) = error "Zip encountered during code generation."
compileExp _ (Unzip _ _ _) = error "Unzip encountered during code generation."
compileExp _ (Map _ _ _ _ _) = soacError
compileExp _ (Reduce _ _ _ _ _) = soacError
compileExp _ (Scan _ _ _ _ _) = soacError
compileExp _ (Filter _ _ _ _) = soacError
compileExp _ (Mapall _ _ _ _ _) = soacError
compileExp _ (Redomap _ _ _ _ _ _ _) = soacError

compileExpInPlace :: C.Exp -> Exp Type -> CompilerM C.Stm

compileExpInPlace place (Iota ne _) = do
  size <- new "iota_size"
  i <- new "iota_i"
  ne' <- compileExp (varExp size) ne
  return [C.cstm|{
           int $id:size, $id:i;
           $stm:ne'
           if ($exp:place.dims[0] != $id:size) {
             error(1, "Cannot fit iota array in destination.\n");
           }
           for ($id:i = 0; $id:i < $id:size; $id:i++) {
             $exp:place.data[$id:i] = $id:i;
           }}|]

compileExpInPlace place e@(Replicate ne ve _) = do
  size <- new "replicate_size"
  i <- new "replicate_i"
  v <- new "replicate_v"
  ne' <- compileExp (varExp size) ne
  ve' <- compileExpInPlace (varExp v) ve
  vt <- typeToCType $ expType ve
  let (vsetup, vpost) =
        case expType ve of
          Array _ _ _ ->
            ([C.cstm|{
                   memcpy($id:v.dims, &$exp:place.dims[1],
                          sizeof($exp:place.dims)-sizeof($exp:place.dims[0]));
                   $id:v.data = $exp:place.data;
                 }|],
             [C.cstm|{}|])
          _ -> ([C.cstm|{}|],
                [C.cstm|$exp:place.data[0] = $id:v;|])
      index = indexArrayExp place (expType e)
      indexi = index [varExp i]
      index0 = index [[C.cexp|0|]]
      index1 = index [[C.cexp|1|]]
  return [C.cstm|{
               int $id:size, $id:i;
               $ty:vt $id:v;
               $stm:ne'
               if ($exp:place.dims[0] != $id:size) {
                 error(1, "Cannot fit replicate array in destination.\n");
               }
               $stm:vsetup
               $stm:ve'
               $stm:vpost
               for ($id:i = 1; $id:i < $id:size; $id:i++) {
                 memcpy(&$exp:indexi, $exp:place.data, (&$exp:index1-&$exp:index0)*sizeof(*$exp:place.data));
                }}|]

compileExpInPlace place e
  | t@(Array _ _ _) <- expType e = do
    tmpplace <- new "inplace"
    e' <- compileExp (varExp tmpplace) e
    let copy = arraySliceCopyStm [C.cexp|$exp:place.data|] (varExp tmpplace) t 0
    ctype <- typeToCType $ expType e
    return [C.cstm|{
               $ty:ctype $id:tmpplace;
               $stm:e'
               if (memcmp($exp:place.dims, $id:tmpplace.dims, sizeof($id:tmpplace.dims)) == 0) {
                  $stm:copy
               } else {
                   error(1, "Cannot fit array in destination.\n");
               }
             }|]

compileExpInPlace place e = compileExp place e

compilePattern :: TupIdent Type -> C.Exp
               -> [(String, C.Exp)]
compilePattern pat vexp = compilePattern' pat vexp
  where compilePattern' (Id var) vexp' = do
          [(identName var, vexp')]
        compilePattern' (TupId pats _) vexp' = do
          concat $ zipWith prep pats [(0::Int)..]
          where prep pat' i = compilePattern' pat' [C.cexp|$exp:vexp'.$id:field|]
                  where field = tupleField i

compileBody :: Exp Type -> CompilerM C.Stm
compileBody body = do
  retval <- new "retval"
  body' <- compileExp (varExp retval) body
  bodytype <- expCType body
  return [C.cstm|{
               $ty:bodytype $id:retval;
               $stm:body'
               return $id:retval;
             }|]

soacError :: CompilerM a
soacError = error $ "SOAC encountered in code generator; should have been removed by first-order transform."
