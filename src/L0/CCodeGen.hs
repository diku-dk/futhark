{-# LANGUAGE QuasiQuotes, GeneralizedNewtypeDeriving #-}
module L0.CCodeGen where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Array as A
import Data.List

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Text.PrettyPrint.Mainland

import L0.AbSyn

data CompilerState = CompilerState {
    compTypeStructs :: [(Type, (C.Type, C.Definition))]
  , compVarDefinitions :: [C.Definition]
  , compInit :: [C.Stm]
  , compVarCounter :: Int
  }

newCompilerState :: CompilerState
newCompilerState = CompilerState {
                     compTypeStructs = []
                   , compVarDefinitions = []
                   , compInit = []
                   , compVarCounter = 0
                   }

-- | Return a list of struct definitions for the tuples and arrays
-- seen during compilation.  The list is sorted according to
-- dependencies, such that a struct at index N only depends on structs
-- at positions >N.
typeDefinitions :: CompilerState -> [C.Definition]
typeDefinitions = reverse . map (snd . snd) . compTypeStructs

newtype CompilerM a = CompilerM (State CompilerState a)
  deriving (Functor, Applicative, Monad, MonadState CompilerState)

runCompilerM :: CompilerM a -> (a, CompilerState)
runCompilerM (CompilerM m) = runState m newCompilerState

-- | 'new s' returns a fresh variable name, with 's' prepended to it.
new :: String -> CompilerM String
new str = do i <- gets compVarCounter
             modify $ \s -> s { compVarCounter = i+1 }
             return $ str ++ "_" ++ show i

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
      name <- new "tuple_type"
      members <- zipWithM field ts [(0::Int)..]
      let struct = [C.cedecl|struct $id:name { $sdecls:members };|]
          stype  = [C.cty|struct $id:name|]
      modify $ \s -> s { compTypeStructs = (t, (stype,struct)) : compTypeStructs s }
      return stype
        where field et i = do
                 ct <- typeToCType et
                 return [C.csdecl|$ty:ct $id:("elem_" ++ show i);|]
typeToCType t@(Array _ _ _) = do
  ty <- gets $ lookup t . compTypeStructs
  case ty of
    Just (cty, _) -> return cty
    Nothing -> do
      name <- new "array_type"
      ct <- typeToCType $ baseType t
      let ctp = [C.cty|$ty:ct*|]
          struct = [C.cedecl|struct $id:name { int dims[$int:(arrayDims t)]; $ty:ctp data; };|]
          stype  = [C.cty|struct $id:name|]
      modify $ \s -> s { compTypeStructs = (t, (stype, struct)) : compTypeStructs s }
      return stype

expCType :: Exp Type -> CompilerM C.Type
expCType = typeToCType . expType

valCType :: Value -> CompilerM C.Type
valCType = typeToCType . valueType

allocArray :: String -> [C.Exp] -> C.Type -> C.Stm
allocArray place shape basetype =
  [C.cstm|{$stms:shapeassign
           $id:place.data = calloc($exp:sizeexp, sizeof($ty:basetype));}|]
  where sizeexp = foldl mult [C.cexp|1|] shape
        mult x y = [C.cexp|$exp:x * $exp:y|]
        shapeassign = zipWith assign shape [0..]
        assign :: C.Exp -> Int -> C.Stm
        assign n i = [C.cstm|$id:place.dims[$int:i] = $exp:n;|]

indexArray :: String -> [C.Exp] -> C.Exp
indexArray place indexes =
  let sizes = map (foldl mult [C.cexp|1|]) $ tails $ map field $ take (length indexes-1) [1..]
      field :: Int -> C.Exp
      field i = [C.cexp|$id:place.dims[$int:i]|]
      mult x y = [C.cexp|$exp:x * $exp:y|]
      add x y = [C.cexp|$exp:x + $exp:y|]
      index = foldl add [C.cexp|0|] $ zipWith mult sizes indexes
  in [C.cexp|$id:place.data[$exp:index]|]

compileProg :: Prog Type -> String
compileProg prog =
  let (funs, endstate) = runCompilerM $ mapM compileFun prog
  in pretty 0 $ ppr [C.cunit|
$esc:("#include <stdio.h>")
$esc:("#include <stdlib.h>")

$edecls:(typeDefinitions endstate)

$edecls:(compVarDefinitions endstate)

$edecls:(map funcToDef funs)

int main() {
  $stms:(compInit endstate)
  l0_main();
  return 0;
}
|]
  where funcToDef func = C.FuncDef func loc
          where loc = case func of
                        C.OldFunc _ _ _ _ _ _ l -> l
                        C.Func _ _ _ _ _ l      -> l

compileFun :: FunDec Type -> CompilerM C.Func
compileFun (fname, rettype, args, body, _) = do
  body' <- compileBody body
  args' <- mapM compileArg args
  crettype <- typeToCType rettype
  return [C.cfun|static $ty:crettype $id:fname' ( $params:args' ) { $stm:body' }|]
  where fname' = "l0_" ++ fname
        compileArg (Ident name t _) = do
          ctp <- typeToCType t
          return [C.cparam|$ty:ctp $id:name|]

compileValue :: String -> Value -> CompilerM C.Stm
compileValue place (IntVal k _) = return [C.cstm|$id:place = $int:k;|]
compileValue place (RealVal x _) = return [C.cstm|$id:place = $double:(toRational x);|]
compileValue place (LogVal b _) = return [C.cstm|$id:place = $int:b';|]
  where b' :: Int
        b' = if b then 1 else 0
compileValue place (CharVal c _) = return [C.cstm|$id:place = $char:c;|]
compileValue place (TupVal vs _) = do
  vs' <- forM (zip vs [(0::Int)..]) $ \(v, i) -> do
           var <- new $ "TupVal_" ++ show i
           v' <- compileValue var v
           let field = "elem_" ++ show i
           vt <- valCType v
           return [C.cstm|{$ty:vt $id:var; $stm:v'; $id:place.$id:field = $id:var;}|]
  return [C.cstm|{$stms:vs'}|]
compileValue place v@(ArrayVal _ et _) = do
  name <- new "ArrayVal"
  ct <- typeToCType $ valueType v
  cbt <- typeToCType $ baseType et
  let asexp n = [C.cexp|$int:n|]
      alloc = allocArray name (map asexp $ arrayShape v) cbt
  i <- new "i"
  initstms <- elemInit name i v
  let def = [C.cedecl|$ty:ct $id:name;|]
      initstm = [C.cstm|{int $id:i = 0; $stm:alloc; $stms:initstms}|]
  modify $ \s -> s { compInit = initstm : compInit s
                   , compVarDefinitions = def : compVarDefinitions s }
  return [C.cstm|$id:place = $id:name;|]
  where elemInit name i (ArrayVal arr _ _) = do
          stms <- concat <$> mapM (elemInit name i) (A.elems arr)
          return [[C.cstm|{$stm:stm}|] | stm <- stms]
        elemInit name i elv = do
          vplace <- new "arrelem"
          vstm <- compileValue vplace elv
          cty <- typeToCType $ valueType elv
          return [[C.cstm|{$ty:cty $id:vplace;
                           $stm:vstm;
                           $id:name.data[$id:i++] = $id:vplace;}|]]

compileExp :: String -> Exp Type -> CompilerM C.Stm
compileExp place (Literal val) = compileValue place val
compileExp place (Var (Ident name _ _)) = return [C.cstm|$id:place = $id:name;|]
compileExp place (TupLit es _) = do
  es' <- forM (zip es [(0::Int)..]) $ \(e, i) -> do
           var <- new $ "TupVal_" ++ show i
           e' <- compileExp var e
           let field = "elem_" ++ show i
           et <- expCType e
           return [C.cstm|{$ty:et $id:var; $stm:e'; $id:place.$id:field = $id:var;}|]
  return [C.cstm|{$stms:es'}|]
compileExp place (BinOp bop e1 e2 _ _) = do
  e1_dest <- new "binop_x1"
  e2_dest <- new "binop_x2"
  e1' <- compileExp e1_dest e1
  e2' <- compileExp e2_dest e2
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
            Plus -> [C.cstm|$id:place = $id:x + $id:y;|]
            Minus -> [C.cstm|$id:place = $id:x - $id:y;|]
            Times -> [C.cstm|$id:place = $id:x * $id:y;|]
            Divide -> [C.cstm|$id:place = $id:x / $id:y;|]
            Pow -> [C.cstm|$id:place = powl($id:x,$id:y);|]
            ShiftR -> [C.cstm|$id:place = $id:x >> $id:y;|]
            ShiftL -> [C.cstm|$id:place = $id:x << $id:y;|]
            Band -> [C.cstm|$id:place = $id:x & $id:y;|]
            Xor -> [C.cstm|$id:place = $id:x ^ $id:y;|]
            Bor -> [C.cstm|$id:place = $id:x | $id:y;|]
            LogAnd -> [C.cstm|$id:place = $id:x && $id:y;|]
            LogOr -> [C.cstm|$id:place = $id:x || $id:y;|]
            Equal -> [C.cstm|$id:place = $id:x == $id:y;|]
            Less -> [C.cstm|$id:place = $id:x < $id:y;|]
            Leq -> [C.cstm|$id:place = $id:x <= $id:y;|]
compileExp place (And e1 e2 _) = do
  e1' <- compileExp place e1
  e2' <- compileExp place e2
  return [C.cstm|{$stm:e1' if ($id:place) { $stm:e2' }}|]
compileExp place (Or e1 e2 _) = do
  e1' <- compileExp place e1
  e2' <- compileExp place e2
  return [C.cstm|{$stm:e1' if (!$id:place) { $stm:e2' }}|]
compileExp place (Not e1 _) = do
  e1' <- compileExp place e1
  return [C.cstm|{$stm:e1' $id:place = !$id:place;}|]
compileExp place (Negate e1 _ _) = do
  e1' <- compileExp place e1
  return [C.cstm|{$stm:e1' $id:place = -$id:place;}|]
compileExp place (If cond e1 e2 _ _) = do
  condvar <- new "if_cond"
  cond' <- compileExp condvar cond
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
                     arg' <- compileExp var arg
                     argtype <- expCType arg
                     return (([C.cdecl|$ty:argtype $id:var;|],
                              [C.cexp|$id:var|]),
                             arg')
  let (vardecls, varexps) = unzip vars
  return [C.cstm|{
               $decls:vardecls
               $stms:args'
               $id:place = $id:fname'($args:varexps);
             }|]
  where fname' = "l0_" ++ fname
compileExp place (LetPat pat e body _) = do
  val <- new "let_value"
  e' <- compileExp val e
  (assignments,decls) <- compilePattern pat [C.cexp|$id:val|]
  body' <- compileExp place body
  et <- expCType e
  return [C.cstm|{
               $decls:decls
               $ty:et $id:val;
               $stm:e'
               $stm:assignments
               $stm:body'
             }|]
compileExp place (Index vname idxs _ _ _) = do
  idxvars <- mapM (new . ("index_"++) . show) [0..length idxs-1]
  idxs' <- zipWithM compileExp idxvars idxs
  let vardecls = [[C.cdecl|int $id:var;|] | var <- idxvars]
      index = indexArray (identName vname) [[C.cexp|$id:var|] | var <- idxvars]
  return [C.cstm|{
               $decls:vardecls
               $stms:idxs'
               $id:place = $exp:index;
             }|]
compileExp place (Iota ne _) = do
  size <- new "iota_size"
  i <- new "iota_i"
  ne' <- compileExp size ne
  let alloc = allocArray place [[C.cexp|$id:size|]] [C.cty|int|]
  return [C.cstm|{
           int $id:size, $id:i;
           $stm:ne'
           $stm:alloc
           $id:place.dims[0] = $id:size;
           for ($id:i = 0; $id:i < $id:size; $id:i++) {
             $id:place.data[$id:i] = $id:i;
           }}|]
compileExp place (Size e _) = do
  dest <- new "size_value"
  et <- typeToCType $ expType e
  e' <- compileExp dest e
  return [C.cstm|{$ty:et $id:dest; $stm:e' $id:place = $id:dest.dims[0];}|]
compileExp place (Replicate ne ve _ _) = do
  size <- new "replicate_size"
  i <- new "replicate_i"
  v <- new "replicate_v"
  ne' <- compileExp size ne
  ve' <- compileExp v ve
  vt <- typeToCType $ expType ve
  let alloc = allocArray place [[C.cexp|$id:size|]] vt
  return [C.cstm|{
           int $id:size, $id:i;
           $ty:vt $id:v;
           $stm:ne'
           $stm:ve'
           $stm:alloc
           $id:place.dims[0] = $id:size;
           for ($id:i = 0; $id:i < $id:size; $id:i++) {
             $id:place.data[$id:i] = $id:v;
           }}|]
compileExp place (Write e t _) = do
  e' <- compileExp place e
  let pr = case t of Int  _ -> [C.cstm|printf("%d", $id:place);|]
                     Char _ -> [C.cstm|printf("%c", $id:place);|]
                     Bool _ -> [C.cstm|printf($id:place && "true" || "false");|]
                     Real _ -> [C.cstm|printf("%lf", $id:place);|]
                     _      -> [C.cstm|printf("Can't print this yet.");|]
  return [C.cstm|{$stm:e' $stm:pr}|]
compileExp place (Read t _) = do
  case t of Int _  -> return [C.cstm|scanf("%d", &$id:place);|]
            Char _ -> return [C.cstm|scanf("%c", &$id:place);|]
            Real _ -> return [C.cstm|scanf("%f", &$id:place);|]
            _      -> return [C.cstm|{fprintf(stderr, "Cannot read that yet.\n"); exit(1);}|]

compilePattern :: TupIdent Type -> C.Exp -> CompilerM (C.Stm, [C.InitGroup])
compilePattern pat vexp = runWriterT $ compilePattern' pat vexp
  where compilePattern' (Id (Ident name t _)) vexp' = do
          ct <- lift $ typeToCType t
          tell [[C.cdecl|$ty:ct $id:name;|]]
          return [C.cstm|$id:name = $exp:vexp';|]
        compilePattern' (TupId pats _) vexp' = do
          pats' <- zipWithM prep pats [(0::Int)..]
          return [C.cstm|{$stms:pats'}|]
          where prep pat' i = compilePattern' pat' [C.cexp|$exp:vexp'.$id:field|]
                  where field = "elem_" ++ show i

compileBody :: Exp Type -> CompilerM C.Stm
compileBody body = do
  retval <- new "retval"
  body' <- compileExp retval body
  bodytype <- expCType body
  return [C.cstm|{
               $ty:bodytype $id:retval;
               $stm:body'
               return $id:retval;
             }|]
