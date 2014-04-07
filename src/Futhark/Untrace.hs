-- | Debugging Futhark programs is done by inserting calls to special
-- functions (currently only @trace@).  These are specially handled in
-- the type checker and interpreter, but not necessarily by other
-- transformations, so it may be necessary to remove them before
-- running optimisation passes.
module Futhark.Untrace
  ( untraceProg )
  where

import Futhark.InternalRep

-- | Remove all special debugging function calls from the program.
untraceProg :: Prog -> Prog
untraceProg = Prog . map untraceFun . progFunctions

untraceFun :: FunDec -> FunDec
untraceFun (fname, ret, params, body, pos) =
  (fname, ret, params, untraceBody body, pos)

untraceBody :: Body -> Body
untraceBody = mapBody untrace
  where untrace = identityMapper {
                    mapOnExp = return . untraceExp
                  , mapOnBody = return .untraceBody
                  , mapOnLambda = return . untraceLambda
                  }
        untraceExp (Apply fname [(e,_)] _ _)
          | "trace" <- nameToString fname = subExp e
        untraceExp e = mapExp untrace e


untraceLambda :: Lambda -> Lambda
untraceLambda (Lambda params body ret pos) =
  Lambda params (untraceBody body) ret pos
