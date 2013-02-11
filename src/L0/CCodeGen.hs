{-# LANGUAGE QuasiQuotes, GeneralizedNewtypeDeriving #-}
module L0.CCodeGen where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State

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
        field t i = [C.csdecl|$ty:(typeToCType t) $id:("elem" ++ show i);|]
typeToCType (Array t _ _) = [C.cty|$ty:(typeToCType t)*|]

expCType :: Exp Identity -> C.Type
expCType = typeToCType . expType

compileFun :: FunDec Identity -> CompilerM C.Func
compileFun (fname, rettype, args, body, _) = do
  body' <- compileBody body
  return [C.cfun|$ty:(typeToCType rettype) $id:fname' ( $params:args' ) { $items:body' }|]
  where args' = map compileArg args
        fname' = "l0_" ++ fname
        compileArg (name, tp) = [C.cparam|$ty:(typeToCType tp) $id:name|]

compileValue :: String -> Value -> CompilerM [C.BlockItem]
compileValue place (IntVal k _) = return [C.BlockStm [C.cstm|$id:place = $int:k;|]]
compileValue place (RealVal x _) = return [C.BlockStm [C.cstm|$id:place = $double:(toRational x);|]]
compileValue place (LogVal b _) = return [C.BlockStm [C.cstm|$id:place = $int:b';|]]
  where b' :: Int
        b' = if b then 1 else 0
compileValue place (CharVal c _) = return [C.BlockStm [C.cstm|$id:place = $char:c;|]]

compileExp :: String -> Exp Identity -> CompilerM [C.BlockItem]
compileExp place (Literal val) = compileValue place val
compileExp place (Var name _ _) = return [C.BlockStm [C.cstm|$id:place = $id:name;|]]
compileExp place (BinOp bop e1 e2 _ _) = do
  e1_dest <- new "binop_x1"
  e2_dest <- new "binop_x2"
  e1' <- compileExp e1_dest e1
  e2' <- compileExp e2_dest e2
  let decls = [C.BlockDecl [C.cdecl|$ty:(expCType e1) $id:e1_dest;|]
              ,C.BlockDecl [C.cdecl|$ty:(expCType e2) $id:e2_dest;|]]
      bop' = C.BlockStm $ compileBinOp e1_dest e2_dest
  return $ decls ++ e1' ++ e2' ++ [bop']
  where compileBinOp x y =
          case bop of
            Plus -> [C.cstm|$id:place = $id:x + $id:y;|]
            Minus -> [C.cstm|$id:place = $id:x - $id:y;|]
            Times -> [C.cstm|$id:place = $id:x * $id:y;|]
            Divide -> [C.cstm|$id:place = $id:x / $id:y;|]
            Pow -> [C.cstm|$id:place = powl($id:x,$id:y);|]
compileExp place (LetPat (Id name _) e body _) = do
  e' <- compileExp name e
  body' <- compileExp place body
  return $ C.BlockDecl [C.cdecl|$ty:(expCType e) $id:name;|]
           : e' ++ body'
compileExp place (Write e (Identity t) _) = do
  e' <- compileExp place e
  let pr = case t of Int  _ -> C.BlockStm [C.cstm|printf("%d", $id:place);|]
                     Char _ -> C.BlockStm [C.cstm|printf("%c", $id:place);|]
                     Bool _ -> C.BlockStm [C.cstm|printf($id:place && "true" || "false");|]
                     Real _ -> C.BlockStm [C.cstm|printf("%lf", $id:place);|]
                     _      -> C.BlockStm [C.cstm|printf("Can't print this yet.");|]
  return $ e' ++ [pr]
compileExp place e =
  return $ case expType e of
             (Int _)  -> [C.BlockStm [C.cstm|$id:place = 0;|]]
             (Bool _) -> [C.BlockStm [C.cstm|$id:place = 0;|]]
             (Char _) -> [C.BlockStm [C.cstm|$id:place = 0;|]]
             (Real _) -> [C.BlockStm [C.cstm|$id:place = 0.0;|]]
             (Tuple _ _) -> [C.BlockStm [C.cstm|;|]]
             (Array _ _ _) -> [C.BlockStm [C.cstm|id:place = 0;|]]

compileBody :: Exp Identity -> CompilerM [C.BlockItem]
compileBody body = do
  body' <- compileExp "retval" body
  return $ C.BlockDecl [C.cdecl|$ty:bodytype retval;|] : body'
  where bodytype = expCType body
