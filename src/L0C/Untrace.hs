-- | Debugging L0 programs is done by inserting calls to special
-- functions (currently only @trace@).  These are specially handled in
-- the type checker and interpreter, but not necessarily by other
-- transformations, so it may be necessary to remove them before
-- running optimisation passes.
module L0C.Untrace
  ( untraceProg )
  where

import L0C.InternalRep

-- | Remove all special debugging function calls from the program.
untraceProg :: Prog -> Prog
untraceProg = Prog . map untraceFun . progFunctions

untraceFun :: FunDec -> FunDec
untraceFun (fname, ret, params, body, pos) =
  (fname, ret, params, untraceExp body, pos)

untraceExp :: Exp -> Exp
untraceExp (Apply fname [(e,_)] _ _)
  | "trace" <- nameToString fname = SubExp e
untraceExp e = mapExp untrace e
  where untrace = identityMapper {
                    mapOnExp = return . untraceExp
                  , mapOnLambda = return . untraceLambda
                  }

untraceLambda :: Lambda -> Lambda
untraceLambda (Lambda params body ret pos) =
  Lambda params (untraceExp body) ret pos
