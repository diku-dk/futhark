module L0C.Untrace
  ( untraceProg )
  where

import L0C.L0

-- | Remove all special debugging function calls from the program.
-- This is necessary for most optimisation modules to work properly,
-- as the debugging functions are special-cased only in the type
-- checker and interpreter.
untraceProg :: ProgBase ty vn -> ProgBase ty vn
untraceProg = Prog . map untraceFun . progFunctions

untraceFun :: FunDecBase ty vn -> FunDecBase ty vn
untraceFun (fname, ret, params, body, pos) =
  (fname, ret, params, untraceExp body, pos)

untraceExp :: ExpBase ty vn -> ExpBase ty vn
untraceExp (Apply fname [e] _ _)
  | "trace" <- nameToString fname = e
untraceExp e = mapExp untrace e
  where untrace = identityMapper {
                    mapOnExp = return . untraceExp
                  , mapOnLambda = return . untraceLambda
                  }

untraceLambda :: LambdaBase ty vn -> LambdaBase ty vn
untraceLambda (AnonymFun params body ret pos) =
  AnonymFun params (untraceExp body) ret pos
untraceLambda (CurryFun fname curryargexps rettype pos) =
  CurryFun fname (map untraceExp curryargexps) rettype pos
