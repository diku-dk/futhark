module L0.Untrace
  ( untraceProg )
  where

import L0.AbSyn
import L0.Traversals

-- | Remove all special debugging function calls from the program.
-- This is necessary for most optimisation modules to work properly,
-- as the debugging functions are special-cased only in the type
-- checker and interpreter.
untraceProg :: Prog ty -> Prog ty
untraceProg = map untraceFun

untraceFun :: FunDec ty -> FunDec ty
untraceFun (fname, ret, params, body, pos) =
  (fname, ret, params, untraceExp body, pos)

untraceExp :: Exp ty -> Exp ty
untraceExp (Apply fname [e] _ _)
  | "trace" <- nameToString fname = e
untraceExp e = mapExp untrace e
  where untrace = identityMapper {
                    mapOnExp = return . untraceExp
                  , mapOnLambda = return . untraceLambda
                  }

untraceLambda :: Lambda ty -> Lambda ty
untraceLambda (AnonymFun params body ret pos) =
  AnonymFun params (untraceExp body) ret pos
untraceLambda (CurryFun fname curryargexps rettype pos) =
  CurryFun fname (map untraceExp curryargexps) rettype pos
