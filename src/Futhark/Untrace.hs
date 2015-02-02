-- | Debugging Futhark programs is done by inserting calls to special
-- functions (currently only @trace@).  These are specially handled in
-- the type checker and interpreter, but not necessarily by other
-- transformations, so it may be necessary to remove them before
-- running optimisation passes.
module Futhark.Untrace
  ( untraceProg )
  where

import Futhark.Representation.Basic
import Futhark.Binder (mkLet)

-- | Remove all special debugging function calls from the program.
untraceProg :: Prog -> Prog
untraceProg = Prog . map untraceFun . progFunctions

untraceFun :: FunDec -> FunDec
untraceFun (FunDec fname ret params body) =
  FunDec fname ret params (untraceBody body)

untraceBody :: Body -> Body
untraceBody = mapBody untrace
  where untrace = identityMapper {
                    mapOnBinding = return . untraceBinding
                  , mapOnBody = return .untraceBody
                  , mapOnLambda = return . untraceLambda
                  }
        untraceBinding bnd@(Let _ _ (PrimOp _)) = bnd
        untraceBinding (Let pat _ e) =
          mkLet (patternIdents pat) $ untraceExp e
        untraceExp (Apply fname [(e,_)] _)
          | "trace" <- nameToString fname = PrimOp $ SubExp e
        untraceExp e = mapExp untrace e


untraceLambda :: Lambda -> Lambda
untraceLambda (Lambda params body ret) =
  Lambda params (untraceBody body) ret
