module L0.Untrace
  ( untraceProg )
  where

import Data.Data
import Data.Generics

import L0.AbSyn

-- | Remove all special debugging function calls from the program.
-- This is necessary for most optimisation modules to work properly,
-- as the debugging functions are special-cased only in the type
-- checker and interpreter.
untraceProg :: Prog Type -> Prog Type
untraceProg = map untraceFun

untraceFun :: FunDec Type -> FunDec Type
untraceFun (fname, ret, params, body, pos) =
  (fname, ret, params, untraceExp body, pos)

untraceExp :: Exp Type -> Exp Type
untraceExp (Apply "trace" [e] _ _) = e
untraceExp e = gmapT (mkT untraceExp
                     `extT` untraceLambda
                     `extT` map untraceExp
                     `extT` map untraceExpPair) e

untraceExpPair :: (Exp Type, Type) -> (Exp Type, Type)
untraceExpPair (e,t) = (untraceExp e,t)

untraceLambda :: Lambda Type -> Lambda Type
untraceLambda (AnonymFun params body ret pos) =
  AnonymFun params (untraceExp body) ret pos
untraceLambda (CurryFun fname curryargexps rettype pos) =
  CurryFun fname (map untraceExp curryargexps) rettype pos
