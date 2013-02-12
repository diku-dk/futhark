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

newtype CompilerM a = CompilerM (State Int a)
  deriving (Functor, Applicative, Monad, MonadState Int)

runCompilerM :: CompilerM a -> a
runCompilerM (CompilerM m) = evalState m 0

-- | 'new s' returns a fresh variable name, with 's' prepended to it.
new :: String -> CompilerM String
new s = do i <- get
           modify (+1)
           return $ s ++ "_" ++ show i

compileProg :: Prog Identity -> String
compileProg prog =
  let funs = runCompilerM $ mapM compileFun prog
  in pretty 0 $ ppr [C.cunit|
$esc:("#include <stdio.h>")

$edecls:(map funcToDef funs)

int main() {
  l0_main();
}
|]
  where funcToDef func = C.FuncDef func $ fromLoc $ locOf func

typeToCType :: Type -> C.Type
typeToCType (Int _) = [C.cty|int|]
typeToCType (Bool _) = [C.cty|int|]
typeToCType (Char _) = [C.cty|char|]
typeToCType (Real _) = [C.cty|double|]
typeToCType (Tuple ts _) = [C.cty|struct { $sdecls:members }|]
  where members = zipWith field ts [(0::Int)..]
        field t i = [C.csdecl|$ty:(typeToCType t) $id:("elem_" ++ show i);|]
typeToCType (Array t _ _) = [C.cty|$ty:(typeToCType t)*|]

expCType :: Exp Identity -> C.Type
expCType = typeToCType . expType

valCType :: Value -> C.Type
valCType = typeToCType . valueType

compileFun :: FunDec Identity -> CompilerM C.Func
compileFun (fname, rettype, args, body, _) = do
  body' <- compileBody body
  return [C.cfun|$ty:(typeToCType rettype) $id:fname' ( $params:args' ) { $stm:body' }|]
  where args' = map compileArg args
        fname' = "l0_" ++ fname
        compileArg (name, tp) = [C.cparam|$ty:(typeToCType tp) $id:name|]

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
           return [C.cstm|{$ty:(valCType v) $id:var; $stm:v'; $id:place.$id:field = $id:var;}|]
  return [C.cstm|{$stms:vs'}|]

compileExp :: String -> Exp Identity -> CompilerM C.Stm
compileExp place (Literal val) = compileValue place val
compileExp place (Var name _ _) = return [C.cstm|$id:place = $id:name;|]
compileExp place (TupLit es _ _) = do
  es' <- forM (zip es [(0::Int)..]) $ \(e, i) -> do
           var <- new $ "TupVal_" ++ show i
           e' <- compileExp var e
           let field = "elem_" ++ show i
           return [C.cstm|{$ty:(expCType e) $id:var; $stm:e'; $id:place.$id:field = $id:var;}|]
  return [C.cstm|{$stms:es'}|]
compileExp place (BinOp bop e1 e2 _ _) = do
  e1_dest <- new "binop_x1"
  e2_dest <- new "binop_x2"
  e1' <- compileExp e1_dest e1
  e2' <- compileExp e2_dest e2
  return [C.cstm|{
               $ty:(expCType e1) $id:e1_dest;
               $ty:(expCType e2) $id:e2_dest;
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
compileExp place (LetPat pat e body _) = do
  val <- new "let_value"
  e' <- compileExp val e
  let (assignments,decls) = compilePattern pat [C.cexp|$id:val|]
  body' <- compileExp place body
  return [C.cstm|{
               $decls:decls
               $ty:(expCType e) $id:val;
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
compileExp place e =
  return $ case expType e of
             (Int _)  -> [C.cstm|$id:place = 0;|]
             (Bool _) -> [C.cstm|$id:place = 0;|]
             (Char _) -> [C.cstm|$id:place = 0;|]
             (Real _) -> [C.cstm|$id:place = 0.0;|]
             (Tuple _ _) -> [C.cstm|;|]
             (Array _ _ _) -> [C.cstm|id:place = 0;|]

compilePattern :: TupIdent Identity -> C.Exp -> (C.Stm, [C.InitGroup])
compilePattern pat vexp = runWriter $ compilePattern' pat vexp
  where compilePattern' (Id name (Identity t) _) vexp' = do 
          tell [[C.cdecl|$ty:(typeToCType t) $id:name;|]]
          return [C.cstm|$id:name = $exp:vexp';|]
        compilePattern' (TupId pats _) vexp' = do
          pats' <- zipWithM prep pats [(0::Int)..]
          return [C.cstm|{$stms:pats'}|]
          where prep pat' i = compilePattern' pat' [C.cexp|$exp:vexp'.$id:field|]
                  where field = "elem_" ++ show i

compileBody :: Exp Identity -> CompilerM C.Stm
compileBody body = do
  body' <- compileExp "retval" body
  return [C.cstm|{
               $ty:bodytype retval;
               $stm:body'
             }|]
  where bodytype = expCType body
