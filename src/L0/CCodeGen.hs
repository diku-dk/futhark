{-# LANGUAGE QuasiQuotes, GeneralizedNewtypeDeriving #-}
module L0.CCodeGen where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.Loc
import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Text.PrettyPrint.Mainland

import L0.AbSyn

data CompilerState = CompilerState {
    compTupleStructs :: [([Type], (C.Type, C.Definition))]
  , compVarCounter :: Int
  }

newCompilerState :: CompilerState
newCompilerState = CompilerState {
                     compTupleStructs = []
                   , compVarCounter = 0
                   }

-- | Return a list of struct definitions for the tuples seen during
-- compilation.  The list is sorted according to dependencies, such
-- that a struct at index N only depends on structs at positions >N.
tupleDefinitions :: CompilerState -> [C.Definition]
tupleDefinitions = reverse . map (snd . snd ) . compTupleStructs

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
typeToCType (Array t _ _) = do
  ct <- typeToCType t
  return [C.cty|$ty:ct*|]
typeToCType (Tuple ts _) = do
  ty <- gets $ lookup ts . compTupleStructs
  case ty of
    Just (cty, _) -> return cty
    Nothing -> do
      name <- new "tuple_type"
      members <- zipWithM field ts [(0::Int)..]
      let struct = [C.cedecl|struct $id:name { $sdecls:members };|]
          stype  = [C.cty|struct $id:name|]
      modify $ \s -> s { compTupleStructs = (ts, (stype,struct)) : compTupleStructs s }
      return stype
        where field t i = do
                 ct <- typeToCType t
                 return [C.csdecl|$ty:ct $id:("elem_" ++ show i);|]

expCType :: Exp Identity -> CompilerM C.Type
expCType = typeToCType . expType

valCType :: Value -> CompilerM C.Type
valCType = typeToCType . valueType

compileProg :: Prog Identity -> String
compileProg prog =
  let (funs, endstate) = runCompilerM $ mapM compileFun prog
  in pretty 0 $ ppr [C.cunit|
$esc:("#include <stdio.h>")

$edecls:(tupleDefinitions endstate)

$edecls:(map funcToDef funs)

int main() {
  l0_main();
}
|]
  where funcToDef func = C.FuncDef func $ fromLoc $ locOf func

compileFun :: FunDec Identity -> CompilerM C.Func
compileFun (fname, rettype, args, body, _) = do
  body' <- compileBody body
  args' <- mapM compileArg args
  crettype <- typeToCType rettype
  return [C.cfun|$ty:crettype $id:fname' ( $params:args' ) { $stm:body' }|]
  where fname' = "l0_" ++ fname
        compileArg (name, tp) = do
          ctp <- typeToCType tp
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

compileExp :: String -> Exp Identity -> CompilerM C.Stm
compileExp place (Literal val) = compileValue place val
compileExp place (Var name _ _) = return [C.cstm|$id:place = $id:name;|]
compileExp place (TupLit es _ _) = do
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
compileExp place (Write e (Identity t) _) = do
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
compileExp place e =
  return $ case expType e of
             (Int _)  -> [C.cstm|$id:place = 0;|]
             (Bool _) -> [C.cstm|$id:place = 0;|]
             (Char _) -> [C.cstm|$id:place = 0;|]
             (Real _) -> [C.cstm|$id:place = 0.0;|]
             (Tuple _ _) -> [C.cstm|;|]
             (Array _ _ _) -> [C.cstm|id:place = 0;|]

compilePattern :: TupIdent Identity -> C.Exp -> CompilerM (C.Stm, [C.InitGroup])
compilePattern pat vexp = runWriterT $ compilePattern' pat vexp
  where compilePattern' (Id name (Identity t) _) vexp' = do
          ct <- lift $ typeToCType t
          tell [[C.cdecl|$ty:ct $id:name;|]]
          return [C.cstm|$id:name = $exp:vexp';|]
        compilePattern' (TupId pats _) vexp' = do
          pats' <- zipWithM prep pats [(0::Int)..]
          return [C.cstm|{$stms:pats'}|]
          where prep pat' i = compilePattern' pat' [C.cexp|$exp:vexp'.$id:field|]
                  where field = "elem_" ++ show i

compileBody :: Exp Identity -> CompilerM C.Stm
compileBody body = do
  retval <- new "retval"
  body' <- compileExp retval body
  bodytype <- expCType body
  return [C.cstm|{
               $ty:bodytype $id:retval;
               $stm:body'
               return $id:retval;
             }|]
