{-# LANGUAGE QuasiQuotes, GeneralizedNewtypeDeriving #-}
-- | C code generator framework.
module L0C.Backends.GenericC
  ( compileProg
  , ExpCompiler
  , ExpCompilerResult(..)
  , CompilerM
  , lookupVar
  , compileExp
  , compileSubExp
  , compileExpNewVar
  ) where

import Debug.Trace

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Array as A
import qualified Data.HashMap.Lazy as HM
import Data.List

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Text.PrettyPrint.Mainland

import L0C.InternalRep
import L0C.Backends.SimpleRepresentation
import L0C.Backends.GenericCReading
import L0C.MonadFreshNames

data CompilerState = CompilerState {
    compTypeStructs :: [([DeclType], (C.Type, C.Definition))]
  , compVarDefinitions :: [C.Definition]
  , compInit :: [C.Stm]
  , compNameSrc :: VNameSource
  }

newCompilerState :: Prog -> CompilerState
newCompilerState prog = CompilerState {
                          compTypeStructs = []
                        , compVarDefinitions = []
                        , compInit = []
                        , compNameSrc = newNameSourceForProg prog
                        }

-- | A substitute expression compiler, tried before the main
-- expression compilation function.
type ExpCompiler = C.Exp -> Exp -> CompilerM ExpCompilerResult

-- | The result of the substitute expression compiler.
data ExpCompilerResult = CompileBody Body
                       -- ^ New bindings.  Note that the bound
                       -- expressions will themselves be compiled
                       -- using the expression compiler.
                       | CompileExp Exp -- ^ A new expression (or
                                        -- possibly the same as the
                                        -- input).
                       | CCode [C.BlockItem] -- ^ Compiled C code.

data CompilerEnv = CompilerEnv {
    envVarMap :: HM.HashMap VName C.Exp
  , envCompileExp :: ExpCompiler
  }

newCompilerEnv :: ExpCompiler -> CompilerEnv
newCompilerEnv ec = CompilerEnv {
                      envVarMap = HM.empty
                    , envCompileExp = ec
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

instance MonadFreshNames CompilerM where
  getNameSource = gets compNameSrc
  putNameSource src = modify $ \s -> s { compNameSrc = src }

runCompilerM :: ExpCompiler -> Prog -> CompilerM a -> (a, CompilerState)
runCompilerM ec prog (CompilerM m) =
  runState (runReaderT m $ newCompilerEnv ec) $ newCompilerState prog

binding :: [(VName, C.Exp)] -> CompilerM a -> CompilerM a
binding kvs = local (flip (foldl add) kvs)
  where add env (k, v) = env { envVarMap = HM.insert k v $ envVarMap env }

lookupVar :: VName -> CompilerM C.Exp
lookupVar k = do v <- asks $ HM.lookup k . envVarMap
                 case v of
                   Nothing -> error $ "Uknown variable " ++ textual k ++ " in code generator."
                   Just v' -> return v'

-- | 'new s' returns a fresh variable name, with 's' prepended to it.
new :: String -> CompilerM String
new = liftM textual . newVName

typeName' :: ArrayShape shape => TypeBase als shape -> String
typeName' (Basic Int)  = "int"
typeName' (Basic Bool) = "bool"
typeName' (Basic Char) = "char"
typeName' (Basic Real) = "real"
typeName' (Basic Cert) = "cert"
typeName' t@(Array {}) =
  typeName' (basicDecl $ elemType t) ++ show (arrayRank t) ++ "d"

typeName :: ArrayShape shape => [TypeBase als shape] -> String
typeName [t] = typeName' t
typeName ts  = "tuple_" ++ intercalate "_" (map typeName' ts)

typeToCType :: ArrayShape shape => [TypeBase als shape] -> CompilerM C.Type
typeToCType [Basic Int]  = return [C.cty|int|]
typeToCType [Basic Bool] = return [C.cty|int|]
typeToCType [Basic Char] = return [C.cty|char|]
typeToCType [Basic Real] = return [C.cty|double|]
typeToCType [Basic Cert] = return [C.cty|int|]
typeToCType [t@Array {}] = do
  ty <- gets $ find (sameRepresentation [toDecl t] . fst) . compTypeStructs
  case ty of
    Just (_, (cty, _)) -> return cty
    Nothing -> do
      ct <- typeToCType [basicDecl $ elemType t]
      let name = typeName [t]
          ctp = [C.cty|$ty:ct*|]
          struct = [C.cedecl|struct $id:name { typename int64_t shape[$int:(arrayRank t)]; $ty:ctp data; };|]
          stype  = [C.cty|struct $id:name|]
      modify $ \s -> s { compTypeStructs = ([toDecl t], (stype, struct)) : compTypeStructs s }
      return stype
typeToCType t = do
  ty <- gets $ find (sameRepresentation (map toDecl t) . fst) . compTypeStructs
  case ty of
    Just (_, (cty, _)) -> return cty
    Nothing -> do
      members <- zipWithM field t [(0::Int)..]
      let name = typeName t
          struct = [C.cedecl|struct $id:name { $sdecls:members };|]
          stype  = [C.cty|struct $id:name|]
      modify $ \s -> s { compTypeStructs = (map toDecl t, (stype,struct)) : compTypeStructs s }
      return stype
        where field et i = do
                ct <- typeToCType [et]
                return [C.csdecl|$ty:ct $id:(tupleField i);|]

expCType :: Exp -> CompilerM C.Type
expCType = typeToCType . typeOf

bodyCType :: Body -> CompilerM C.Type
bodyCType = typeToCType . bodyType

-- | Return a statement printing the given value.
printStm :: C.Exp -> [DeclType] -> CompilerM C.Stm
printStm place [Basic Int]  = return [C.cstm|printf("%d", $exp:place);|]
printStm place [Basic Char] = return [C.cstm|printf("%c", $exp:place);|]
printStm place [Basic Bool] =
  return [C.cstm|printf($exp:place ? "True" : "False");|]
printStm place [Basic Real] = return [C.cstm|printf("%.6f", $exp:place);|]
printStm _     [Basic Cert] = return [C.cstm|printf("Checked");|]
printStm place [Array Char _ _ _] =
  return [C.cstm|printf("%s", $exp:place.data);|]
printStm place [t@Array {}] = do
  i <- new "print_i"
  v <- new "print_elem"
  et' <- typeToCType [rowType t]
  pstm <- printStm (varExp v) [rowType t]
  let indexi = indexArrayElemStms (varExp v) place t [varExp i]
  return [C.cstm|{
               int $id:i;
               $ty:et' $id:v;
               if ($exp:place.shape[0] == 0) {
                   printf("empty(%s)", $exp:(ppType $ rowType t));
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

readStm :: C.Exp -> DeclType -> C.Stm
readStm place t@(Basic et)
  | Just f <- readFun et =
    [C.cstm|if ($id:f(&$exp:place) != 0) {
          fprintf(stderr, "Syntax error when reading %s.\n", $string:(ppType t));
                 exit(1);
        }|]
readStm place t@(Array {})
  | Just f <- readFun (elemType t) =
    [C.cstm|if (read_array(sizeof(*$exp:place.data),
                           $id:f,
                           (void**)&$exp:place.data,
                           $exp:place.shape,
                           $int:n)
                != 0) {
       fprintf(stderr, "Syntax error when reading %s.\n", $string:(ppType t));
       exit(1);
     }|]
  where n  = arrayRank t
readStm _ t =
  [C.cstm|{
        fprintf(stderr, "Cannot read %s yet.\n", $string:(ppType t));
        exit(1);
      }|]

mainCall :: FunDec -> CompilerM C.Stm
mainCall (fname,rettype,params,_,_) = do
  crettype <- typeToCType $ map fromDecl rettype
  ret <- new "main_ret"
  printRes <- printStm (varExp ret) rettype
  (args, decls, rstms) <- liftM unzip3 . forM paramtypes $ \paramtype -> do
    name <- new "main_arg"
    cparamtype <- typeToCType [paramtype]
    let rstm = readStm (varExp name) $ toDecl paramtype
    return (varExp name, [C.cdecl|$ty:cparamtype $id:name;|], rstm)
  return [C.cstm|{
               $decls:decls
               $ty:crettype $id:ret;
               $stms:rstms
               $id:ret = $id:(funName fname)($args:args);
               $stm:printRes
               printf("\n");
             }|]
  where paramtypes = map identType params

-- | Compile L0 program to a C program.  Always uses the function
-- named "main" as entry point, so make sure it is defined.
compileProg :: ExpCompiler -> Prog -> String
compileProg ec prog =
  let ((prototypes, definitions, main), endstate) =
        runCompilerM ec prog compileProg'
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
          (prototypes, definitions) <- unzip <$> mapM compileFun (progFunctions prog)
          main <- case funDecByName (nameFromString "main") prog of
                    Nothing -> error "L0.CCodeGen.compileProg: No main function"
                    Just func -> mainCall func
          return (prototypes, definitions, main)
        funcToDef func = C.FuncDef func loc
          where loc = case func of
                        C.OldFunc _ _ _ _ _ _ l -> l
                        C.Func _ _ _ _ _ l      -> l

compileFun :: FunDec -> CompilerM (C.Definition, C.Func)
compileFun (fname, rettype, args, body, _) = do
  (argexps, args') <- unzip <$> mapM compileArg args
  body' <- binding (zip (map identName args) $ argexps) $ compileFunBody body
  crettype <- typeToCType $ map fromDecl rettype
  return ([C.cedecl|static $ty:crettype $id:(funName fname)( $params:args' );|],
          [C.cfun|static $ty:crettype $id:(funName fname)( $params:args' ) { $items:body' }|])
  where compileArg (Ident name t _) = do
          name' <- new $ textual name
          ctp <- typeToCType [t]
          return (varExp name', [C.cparam|$ty:ctp $id:name'|])

compileValue :: C.Exp -> Value -> CompilerM [C.Stm]

compileValue place (BasicVal (IntVal k)) =
  return [[C.cstm|$exp:place = $int:k;|]]

compileValue place (BasicVal (RealVal x)) =
  return [[C.cstm|$exp:place = $double:(toRational x);|]]

compileValue place (BasicVal (LogVal b)) =
  return [[C.cstm|$exp:place = $int:b';|]]
  where b' :: Int
        b' = if b then 1 else 0

compileValue place (BasicVal (CharVal c)) =
  return [[C.cstm|$exp:place = $char:c;|]]

compileValue _ (BasicVal Checked) = return []

compileValue place v@(ArrayVal _ rt) = do
  val <- new "ArrayVal"
  dt <- new "ArrayData"
  ct <- typeToCType [valueType v]
  cbt <- typeToCType [basicDecl $ elemType rt]
  let asinit n = [C.cinit|$int:n|]
      shape = map asinit $ valueShape v
      arrdef = [C.cedecl|$ty:ct $id:val = { $inits:shape, NULL };|]
  case elemInit v of
    [] -> modify $ \s -> s { compVarDefinitions = arrdef : compVarDefinitions s }
    elems -> let elemdef = case arrayString $ flattenArray v of
                      Just s -> [C.cedecl|$ty:cbt $id:dt[] = $string:s;|]
                      Nothing -> [C.cedecl|$ty:cbt $id:dt[] = { $inits:elems };|]
                 initstm = [C.cstm|$id:val.data = $id:dt;|]
             in modify $ \s -> s {
                                 compInit = initstm : compInit s
                               , compVarDefinitions = arrdef : elemdef :
                                                      compVarDefinitions s
                               }
  return [[C.cstm|$exp:place = $id:val;|]]
  where elemInit (ArrayVal arr _) = concatMap elemInit $ A.elems arr
        elemInit (BasicVal bv)  = basicElemInit bv
        basicElemInit (IntVal x) = [[C.cinit|$int:x|]]
        basicElemInit (RealVal x) = [[C.cinit|$double:(toRational x)|]]
        basicElemInit (CharVal c) = [[C.cinit|$char:c|]]
        basicElemInit Checked = [[C.cinit|0|]]
        basicElemInit (LogVal True) = [[C.cinit|1|]]
        basicElemInit (LogVal False) = [[C.cinit|0|]]

compileSubExp :: C.Exp -> SubExp -> CompilerM [C.BlockItem]
compileSubExp place (Constant val _) = do
  stms <$> compileValue place val

compileSubExp place (Var (Ident name _ _)) = do
  name' <- lookupVar name
  return $ stm [C.cstm|$exp:place = $exp:name';|]

compileBody :: C.Exp -> Body -> CompilerM [C.BlockItem]
compileBody place (LetPat pat e body _) = do
  val <- new "let_value"
  e' <- compileExp (varExp val) e
  let bindings = compilePattern pat (varExp val)
  body' <- binding bindings $ compileBody place body
  et <- expCType e
  return $ stm [C.cstm|{
                     $ty:et $id:val;
                     $items:e'
                     $items:body'
                   }|]

compileBody place (LetWith _ name src idxs ve body _) = do
  name' <- new $ textual $ identName name
  src' <- lookupVar $ identName src
  etype <- typeToCType [identType src]
  idxvars <- mapM (const $ new "letwith_index") [1..length idxs]
  let idxexps = map varExp idxvars
  idxs' <- concat <$> zipWithM compileSubExp idxexps idxs
  el <- new "letwith_el"
  elty <- typeToCType [subExpType ve]
  ve' <- compileSubExpInPlace (varExp el) ve
  let idxdecls = [[C.cdecl|int $id:idxvar;|] | idxvar <- idxvars]
      (elempre, elempost) =
        case subExpType ve of
          Array {} -> (indexArrayElemStms (varExp el) (varExp name') (identType src) idxexps,
                       [C.cstm|;|])
          _ -> ([[C.cstm|;|]],
                case identType src of
                  Array {} ->
                    [C.cstm|$exp:(indexArrayExp (varExp name')
                                  (arrayRank $ identType src) idxexps) = $id:el;|]
                  _ -> [C.cstm|$id:name' = $id:el;|])
  body' <- binding [(identName name, varExp name')] $ compileBody place body
  return $ stm [C.cstm|{
                     $ty:etype $id:name';
                     $ty:elty $id:el;
                     $decls:idxdecls
                     $id:name' = $exp:src';
                     $items:idxs'
                     $stms:elempre
                     $items:ve'
                     $stm:elempost
                     $items:body'
                   }|]

compileBody place (DoLoop merge loopvar boundexp loopbody letbody loc) = do
  let (mergepat, mergeexp) = unzip merge
  loopvar' <- new $ textual $ identName loopvar
  bound <- new "loop_bound"
  mergevarR <- new $ "loop_mergevar_read"
  mergevarW <- new $ "loop_mergevar_write"
  let bindings = compilePattern mergepat (varExp mergevarR)
  mergeexp' <- compileExp (varExp mergevarR) $ TupLit mergeexp loc
  mergetype <- bodyCType loopbody
  boundexp' <- compileSubExp (varExp bound) boundexp
  loopbody' <- binding ((identName loopvar, varExp loopvar') : bindings) $
               compileBody (varExp mergevarW) loopbody
  letbody' <- binding bindings $ compileBody place letbody
  return $ stm [C.cstm|{
                     int $id:bound, $id:loopvar';
                     $ty:mergetype $id:mergevarR;
                     $ty:mergetype $id:mergevarW;
                     $items:mergeexp'
                     $items:boundexp'
                     for ($id:loopvar' = 0; $id:loopvar' < $id:bound; $id:loopvar'++) {
                       $items:loopbody'
                       $id:mergevarR = $id:mergevarW;
                     }
                     $items:letbody'
                   }|]

compileBody place (Result _ [e] _)  = compileSubExp place e
compileBody place (Result _ es loc) = compileExp place $ TupLit es loc

compileExp :: C.Exp -> Exp -> CompilerM [C.BlockItem]

compileExp target e = do
  res <- join $ asks envCompileExp <*> pure target <*> pure e
  case res of CompileBody b  -> trace ("body " ++ ppBody b) $ compileBody target b
              CompileExp  e' -> compileExp' target e'
              CCode res'     -> return res'

compileExp' :: C.Exp -> Exp -> CompilerM [C.BlockItem]

compileExp' place (SubExp e) =
  compileSubExp place e

compileExp' place (TupLit [e] _) =
  compileSubExp place e

compileExp' place (TupLit es _) = do
  liftM concat . forM (zip es [(0::Int)..]) $ \(e, i) -> do
    var <- new $ "TupVal_" ++ show i
    e' <- compileSubExp (varExp var) e
    let field = tupleField i
    et <- expCType $ SubExp e
    return $ stm [C.cstm|{
                       $ty:et $id:var;
                       $items:e';
                       $exp:place.$id:field = $id:var;
                     }|]

compileExp' place (ArrayLit [] rt _) = do
  stms <$> compileValue place
           (blankValue $ arrayOf (toDecl rt) (Rank 1) Unique)

compileExp' place (ArrayLit (e:es) _ _) = do
  name <- new "ArrayLit_elem"
  et <- typeToCType [subExpType e]
  bt <- typeToCType [basicDecl $ elemType $ subExpType e]
  e' <- compileSubExp (varExp name) e
  es' <- mapM (compileSubExpInPlace $ varExp name) es
  i <- new "ArrayLit_i"
  case subExpType e of
    Array {} -> do
      eldims <- new "ArrayLit_eldims"
      elsize <- new "ArrayLit_elsize"
      datap <- new "ArrayLit_datap"
      let numdims = arrayRank $ subExpType e
          dimexps = [C.cexp|$int:(length es+1)|] :
                    [ [C.cexp|$id:eldims[$int:j]|] | j <- [0..numdims-1] ]
          alloc = allocArray place dimexps bt
          es'' = [ [C.cstm|{
                         $items:e''
                         if (memcmp($id:eldims, $id:name.shape, sizeof($id:eldims)) != 0) {
                             error(1, "Array elements have different sizes.\n");
                         }
                         memcpy($id:datap, $id:name.data, $id:elsize);
                         $id:datap += $id:elsize;
                       }|] | e'' <- es']
      return $ stm [C.cstm|{
                         $ty:et $id:name;
                         int $id:eldims[$int:numdims];
                         int $id:elsize = 1 * sizeof($ty:bt);
                         int $id:i;
                         char *$id:datap;
                         $items:e'
                         memcpy($id:eldims, $id:name.shape, sizeof($id:eldims));
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
                         $items:e''
                         $exp:place.data[$id:i++] = $id:name;
                       }|] | e'' <- e':es']
      return $ stm [C.cstm|{
                         $ty:et $id:name;
                         int $id:i = 0;
                         $stm:alloc
                         $stms:es''
                       }|]

compileExp' place (BinOp bop e1 e2 _ _) = do
  e1_dest <- new "binop_x1"
  e2_dest <- new "binop_x2"
  e1' <- compileSubExp (varExp e1_dest) e1
  e2' <- compileSubExp (varExp e2_dest) e2
  e1t <- expCType $ SubExp e1
  e2t <- expCType $ SubExp e2
  return $ stm [C.cstm|{
                     $ty:e1t $id:e1_dest;
                     $ty:e2t $id:e2_dest;
                     $items:e1'
                     $items:e2'
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

compileExp' place (Not e1 _) = do
  e1' <- compileSubExp place e1
  return $ stm [C.cstm|{$items:e1' $exp:place = !$exp:place;}|]

compileExp' place (Negate e1 _) = do
  e1' <- compileSubExp place e1
  return $ stm [C.cstm|{$items:e1' $exp:place = -$exp:place;}|]

compileExp' place (If cond e1 e2 _ _) = do
  condvar <- new "if_cond"
  cond' <- compileSubExp (varExp condvar) cond
  e1' <- compileBody place e1
  e2' <- compileBody place e2
  return $ stm [C.cstm|{
                     int $id:condvar;
                     $items:cond'
                     if ($id:condvar) { $items:e1' }
                     else { $items:e2' }
                   }|]

compileExp' place (Apply fname args _ _) = do
  (vars, args') <- liftM unzip . forM args $ \(arg, _) -> do
                     var <- new "apply_arg"
                     arg' <- compileSubExp (varExp var) arg
                     argtype <- expCType $ SubExp arg
                     return (([C.cdecl|$ty:argtype $id:var;|], varExp var), arg')
  let (vardecls, varexps) = unzip vars
  return $ stm [C.cstm|{
                     $decls:vardecls
                     $items:(concat args')
                     $exp:place = $id:(funName fname)($args:varexps);
               }|]

compileExp' place (Index _ var idxs _) = do
  arr <- lookupVar $ identName var
  idxvars <- mapM (new . ("index_"++) . show) [0..length idxs-1]
  idxs' <- concat <$> zipWithM compileSubExp (map varExp idxvars) idxs
  let vardecls = [[C.cdecl|int $id:idxvar;|] | idxvar <- idxvars]
      varexps =  map varExp idxvars
      index = indexArrayElemStms place arr (identType var) varexps
  return $ stm [C.cstm|{
                     $decls:vardecls
                     $items:idxs'
                     $stms:index
                   }|]

compileExp' place e@(Iota se _) = do
  n <- new "iota_n"
  e' <- compileExpInPlace place e
  se' <- compileSubExp (varExp n) se
  let alloc = allocArray place [varExp n] [C.cty|int|]
  return $ stm [C.cstm|{
                     int $id:n;
                     $items:se'
                     $stm:alloc
                     $items:e'
                   }|]

compileExp' place e@(Replicate (Var nv) (Var vv) _) = do
  e' <- compileExpInPlace place e
  bt <- typeToCType [basicDecl $ elemType $ identType vv]
  nv' <- lookupVar $ identName nv
  vv' <- lookupVar $ identName vv
  let dims = nv' : arrayShapeExp vv' (identType vv)
      alloc = allocArray place dims bt
  return $ stm [C.cstm|{
                     $stm:alloc
                     $items:e'
                   }|]

compileExp' place (Replicate ne ve pos) = do
  nv <- newVName "replicate_n"
  vv <- newVName "replicate_v"
  rr <- newVName "replicate_r"
  let nident = Ident nv (Basic Int) pos
      vident = Ident vv (subExpType ve) pos
      rident = Ident rr (arrayOf (subExpType ve) (Shape [Var nident]) Unique) pos
      nlet body = LetPat [nident] (SubExp ne) body pos
      vlet body = LetPat [vident] (SubExp ve) body pos
      rlet body = LetPat [rident] (Replicate (Var nident) (Var vident) pos) body pos
  compileBody place $ nlet $ vlet $ rlet $ Result [] [Var rident] pos

compileExp' place (Reshape _ shapeexps arrexp _) = do
  shapevars <- mapM (new . ("shape_"++) . show) [0..length shapeexps-1]
  arr <- new "reshape_arr"
  intype' <- typeToCType [subExpType arrexp]
  shapeexps' <- concat <$> zipWithM compileSubExp (map varExp shapevars) shapeexps
  arrexp' <- compileSubExp (varExp arr) arrexp
  let vardecls = [[C.cdecl|int $id:var;|] | var <- shapevars]
      assignstms = [[C.cstm|$exp:place.shape[$int:i] = $id:var;|] | (var, i) <- zip shapevars [(0::Int)..]]
  return $ stm [C.cstm|{
                     $ty:intype' $id:arr;
                     $decls:vardecls
                     $items:shapeexps'
                     $stms:assignstms
                     $items:arrexp'
                     $exp:place.data = $id:arr.data;
                   }|]

compileExp' place (Rearrange _ perm arrexp _) = do
  arr <- new "transpose_arr"
  intype <- typeToCType [subExpType arrexp]
  basetype <- typeToCType [basicDecl $ elemType $ subExpType arrexp]
  arrexp' <- compileSubExp (varExp arr) arrexp
  let newshape = permuteShape perm $ arrayShapeExp (varExp arr) (subExpType arrexp)
      alloc =  allocArray place newshape basetype
      rank = arrayRank $ subExpType arrexp
      loop is 0 =
        let iexps = map varExp is
            indexfrom = indexArrayExp (varExp arr) rank iexps
            indexto   = indexArrayExp place rank $ permuteShape perm iexps
        in return ([C.cstm|$exp:indexto = $exp:indexfrom;|], is)
      loop is d = do i <- new "transpose_i"
                     (inner, is') <- loop (i:is) (d-1)
                     let body = [C.cstm|for ($id:i = 0; $id:i < $id:arr.shape[$int:(d-1)]; $id:i++) {
                                      $stm:inner
                                    }|]
                     return (body, is')
  (copy, is) <- loop [] $ arrayRank $ subExpType arrexp
  let idecls = [[C.cdecl|int $id:i;|] | i <- is]
  return $ stm [C.cstm|{
                     $ty:intype $id:arr;
                     $decls:idecls
                     $items:arrexp'
                     $stm:alloc
                     $stm:copy
                   }|]

compileExp' place (Rotate _ n arrexp _) = do
  arr <- new "rotate_arr"
  i <- new "rotate_i"
  intype <- typeToCType [subExpType arrexp]
  basetype <- typeToCType [basicDecl $ elemType $ subExpType arrexp]
  arrexp' <- compileSubExp (varExp arr) arrexp
  let shape = arrayShapeExp (varExp arr) (subExpType arrexp)
      alloc = allocArray place shape basetype
      rank = arrayRank $ subExpType arrexp
      copyStm = arraySliceCopyStm
                [C.cexp|&($exp:place.data[($id:i+$int:n)%$id:arr.shape[0]])|]
                [C.cexp|&($id:arr.data[$id:i])|]
                [C.cexp|$id:arr.shape|]
                (stripArray 1 $ subExpType arrexp)
                (rank-1)
      copy = [C.cstm|{
                   int $id:i;
                   for ($id:i = 0; $id:i < $id:arr.shape[0]; $id:i++) {
                           $stm:copyStm
                         }
                 }|]
  return $ stm [C.cstm|{
                     $ty:intype $id:arr;
                     $items:arrexp'
                     $stm:alloc
                     $stm:copy
                   }|]

compileExp' place (Split _ posexp arrexp _ _) = do
  arr <- new "split_arr"
  pos <- new "split_pos"
  arrexp' <- compileSubExp (varExp arr) arrexp
  posexp' <- compileSubExp (varExp pos) posexp
  arrt <- typeToCType [subExpType arrexp]
  let splitexp = indexArrayExp (varExp arr) (arrayRank $ subExpType arrexp) [varExp pos]
      place0 = tupleFieldExp place 0
      place1 = tupleFieldExp place 1
  return $ stm [C.cstm|{
                     $ty:arrt $id:arr;
                     int $id:pos;
                     $items:posexp'
                     $items:arrexp'
                     if ($id:pos < 0 || $id:pos > $id:arr.shape[0]) {
                       error(1, "Split out of bounds.\n");
                     }
                     memcpy($exp:place0.shape, $id:arr.shape, sizeof($id:arr.shape));
                     memcpy($exp:place1.shape, $id:arr.shape, sizeof($id:arr.shape));
                     $exp:place0.data = $id:arr.data;
                     $exp:place0.shape[0] = $id:pos;
                     $exp:place1.data = &$exp:splitexp;
                     $exp:place1.shape[0] -= $id:pos;
                   }|]

compileExp' place (Concat _ xarr yarr _ _) = do
  x <- new "concat_x"
  y <- new "concat_y"
  xarr' <- compileSubExp (varExp x) xarr
  yarr' <- compileSubExp (varExp y) yarr
  arrt <- typeToCType [subExpType xarr]
  bt' <- typeToCType [basicDecl $ elemType $ subExpType xarr]
  let alloc = case (arrayShapeExp (varExp x) (subExpType xarr),
                    arrayShapeExp (varExp y) (subExpType yarr)) of
                (xrows:rest, yrows:_) ->
                  allocArray place ([C.cexp|$exp:xrows+$exp:yrows|]:rest) bt'
                _ -> error "Zero-dimensional array in concat."
      xsize = arraySliceSizeExp (varExp x) (subExpType xarr) 0
      copyx = arraySliceCopyStm
              [C.cexp|$exp:place.data|] [C.cexp|$id:x.data|]
              [C.cexp|$id:x.shape|] (subExpType xarr) 0
      copyy = arraySliceCopyStm
              [C.cexp|$exp:place.data+$exp:xsize|] [C.cexp|$id:y.data|]
              [C.cexp|$id:y.shape|] (subExpType yarr) 0
  return $ stm [C.cstm|{
                     $ty:arrt $id:x, $id:y;
                     $items:xarr'
                     $items:yarr'
                     if ($exp:(arraySliceSizeExp (varExp x) (subExpType xarr) 1) !=
                         $exp:(arraySliceSizeExp (varExp y) (subExpType yarr) 1)) {
                       error(1, "Arguments to concat differ in size.");
                     }
                     $stm:alloc
                     $stm:copyx
                     $stm:copyy
             }|]

compileExp' place (Copy e _) = do
  val <- new "copy_val"
  e' <- compileSubExp (varExp val) e
  t <- typeToCType [subExpType e]
  let copy = case subExpType e of
               Array {} -> arraySliceCopyStm [C.cexp|$exp:place.data|]
                           [C.cexp|$id:val.data|] [C.cexp|$id:val.shape|]
                           (subExpType e) 0
               _ -> [C.cstm|;|]
  return $ stm [C.cstm|{
                     $ty:t $id:val;
                     $items:e'
                     $exp:place = $id:val;
                     $stm:copy
                   }|]

compileExp' place (Assert e loc) = do
  e' <- compileSubExp place e
  return $ stm [C.cstm|{
                     $items:e'
                     if (!$exp:place) {
                            fprintf(stderr, "Assertion %s at %s failed.\n",
                                    $string:(ppSubExp e), $string:(locStr loc));
                            exit(1);
                          }
                   }|]

compileExp' _ (Conjoin _ _) = return []

compileExp' _ (Map {}) = soacError
compileExp' _ (Reduce {}) = soacError
compileExp' _ (Scan {}) = soacError
compileExp' _ (Filter {}) = soacError
compileExp' _ (Redomap {}) = soacError

compileSubExpInPlace :: C.Exp -> SubExp -> CompilerM [C.BlockItem]
compileSubExpInPlace place e
  | t@Array {} <- subExpType e = do
  tmpplace <- new "inplace"
  e' <- compileSubExp (varExp tmpplace) e
  let copy = arraySliceCopyStm [C.cexp|$exp:place.data|]
             [C.cexp|$id:tmpplace.data|] [C.cexp|$id:tmpplace.shape|]
             t 0
  ctype <- typeToCType [subExpType e]
  return $ stm [C.cstm|{
                     $ty:ctype $id:tmpplace;
                     $items:e'
                     if (memcmp($exp:place.shape, $id:tmpplace.shape, sizeof($id:tmpplace.shape)) == 0) {
                        $stm:copy
                     } else {
                         error(1, "Cannot fit array in destination.\n");
                   }
                   }|]

  | otherwise = compileSubExp place e

compileExpInPlace :: C.Exp -> Exp -> CompilerM [C.BlockItem]

compileExpInPlace place (Iota ne _) = do
  size <- new "iota_size"
  i <- new "iota_i"
  ne' <- compileSubExp (varExp size) ne
  return $ stm [C.cstm|{
                     int $id:size, $id:i;
                     $items:ne'
                     if ($exp:place.shape[0] != $id:size) {
                            error(1, "Cannot fit iota array in destination.\n");
                     }
                     for ($id:i = 0; $id:i < $id:size; $id:i++) {
                             $exp:place.data[$id:i] = $id:i;
                     }}|]

compileExpInPlace place (Replicate ne ve _) = do
  size <- new "replicate_size"
  i <- new "replicate_i"
  v <- new "replicate_v"
  ne' <- compileSubExp (varExp size) ne
  ve' <- compileSubExpInPlace (varExp v) ve
  vt <- typeToCType [subExpType ve]
  let (vsetup, vpost) =
        case subExpType ve of
          Array {} ->
            ([C.cstm|{
                   memcpy($id:v.shape, &$exp:place.shape[1],
                          sizeof($exp:place.shape)-sizeof($exp:place.shape[0]));
                   $id:v.data = $exp:place.data;
                 }|],
             [C.cstm|{}|])
          _ -> ([C.cstm|{}|],
                [C.cstm|$exp:place.data[0] = $id:v;|])
      index = indexArrayExp place (arrayRank (subExpType ve) + 1)
      indexi = index [varExp i]
      index0 = index [[C.cexp|0|]]
      index1 = index [[C.cexp|1|]]
  return $ stm [C.cstm|{
                     int $id:size, $id:i;
                     $ty:vt $id:v;
                     $items:ne'
                     if ($exp:place.shape[0] != $id:size) {
                       error(1, "Cannot fit replicate array in destination.\n");
                     }
                     $stm:vsetup
                     $items:ve'
                     $stm:vpost
                     for ($id:i = 1; $id:i < $id:size; $id:i++) {
                       memcpy(&$exp:indexi, $exp:place.data, (&$exp:index1-&$exp:index0)*sizeof(*$exp:place.data));
                   }}|]

compileExpInPlace place e
  | [t@Array {}] <- typeOf e = do
    tmpplace <- new "inplace"
    e' <- compileExp (varExp tmpplace) e
    let copy = arraySliceCopyStm [C.cexp|$exp:place.data|]
               [C.cexp|$id:tmpplace.data|] [C.cexp|$id:tmpplace.shape|]
               t 0
    ctype <- typeToCType $ typeOf e
    return $ stm [C.cstm|{
                       $ty:ctype $id:tmpplace;
                       $items:e'
                       if (memcmp($exp:place.shape, $id:tmpplace.shape, sizeof($id:tmpplace.shape)) == 0) {
                          $stm:copy
                       } else {
                           error(1, "Cannot fit array in destination.\n");
                     }
                     }|]

compileExpInPlace place e = compileExp place e

compileExpNewVar :: Exp -> CompilerM (String, [C.BlockItem])
compileExpNewVar e = do
  v <- new "val"
  ctype <- typeToCType $ typeOf e
  e' <- compileExp (varExp v) e
  return (v, [C.citem|$ty:ctype $id:v;|] : e')

compilePattern :: [Ident] -> C.Exp -> [(VName, C.Exp)]
compilePattern [v] vexp =
  [(identName v, [C.cexp|$exp:vexp|])]
compilePattern pat vexp =
  zipWith prep pat [(0::Int)..]
  where prep v i =
          (identName v, [C.cexp|$exp:vexp.$id:field|])
            where field = tupleField i

compileFunBody :: Body -> CompilerM [C.BlockItem]
compileFunBody body = do
  retval <- new "retval"
  body' <- compileBody (varExp retval) body
  bodytype <- bodyCType body
  return $ stm [C.cstm|{
                     $ty:bodytype $id:retval;
                     $items:body'
                     return $id:retval;
                   }|]

soacError :: CompilerM a
soacError = error $ "SOAC encountered in code generator; should have been removed by first-order transform."
