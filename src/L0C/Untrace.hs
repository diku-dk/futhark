-- | Debugging L0 programs is done by inserting calls to special
-- functions (currently only @trace@).  These are specially handled in
-- the type checker and interpreter, but not necessarily by other
-- transformations, so it may be necessary to remove them before
-- running optimisation passes.
module L0C.Untrace
  ( untraceProg )
  where

import Control.Arrow

import L0C.L0

-- | Remove all special debugging function calls from the program.
untraceProg :: ProgBase ty vn -> ProgBase ty vn
untraceProg = Prog . map untraceFun . progFunctions

untraceFun :: FunDecBase ty vn -> FunDecBase ty vn
untraceFun (fname, ret, params, body, pos) =
  (fname, ret, params, untraceExp body, pos)

untraceExp :: ExpBase ty vn -> ExpBase ty vn
untraceExp (Apply fname [(e,_)] _ _)
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
  CurryFun fname (map (first untraceExp) curryargexps) rettype pos
