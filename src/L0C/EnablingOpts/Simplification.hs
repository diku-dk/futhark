-- | This module implements simple simplification rules for let-bound
-- expressions.  The intent is that you pass a symbol-lookup function
-- and a binding (consisting of the output pattern and the
-- expression), and are given back a sequence of bindings, that are
-- more efficient than the original binding, yet compute the same
-- result.
--
-- These rewrite rules are "local", in that they do not maintain any
-- state or look at the program as a whole.  Compare this to the
-- fusion algorithm in @L0C.HOTrans.Fusion@, which must be implemented
-- as its own pass.
module L0C.EnablingOpts.Simplification
  ( simplifyBinding
  , VarLookup
  )

where

import Control.Applicative

import Data.Maybe

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

import L0C.InternalRep

-- | A function that, given a variable name, returns its definition.
type VarLookup = VName -> Maybe Exp

-- | @simplifyBinding lookup pat exp@ performs simplification of the
-- expression @exp@, which is bound to @pat@ in the original program.
-- If simplification is possible, a replacement set of let-bindings is
-- returned, that bind at least the names in @pat@ (and possibly more,
-- for intermediate results).
simplifyBinding :: VarLookup -> [Ident] -> Exp -> Maybe [([Ident], Exp)]

simplifyBinding = applyRules simplificationRules

applyRules :: [SimplificationRule]
           -> VarLookup -> [Ident] -> Exp -> Maybe [([Ident], Exp)]
applyRules [] _ _ _ = Nothing
applyRules (rule:rules) look pat e =
  (concatMap subApply <$> rule look pat e) <|>
  applyRules rules look pat e
  where subApply (pat', e') =
          fromMaybe [(pat', e')] $ applyRules (rule:rules) look pat' e'

type SimplificationRule = VarLookup -> [Ident] -> Exp -> Maybe [([Ident], Exp)]

simplificationRules :: [SimplificationRule]
simplificationRules = [ liftIdentityMapping
                      , removeReplicateMapping
                      ]

liftIdentityMapping :: SimplificationRule
liftIdentityMapping _ pat (Map cs fun arrs loc) =
  case foldr checkInvariance ([], [], []) $ zip3 pat resultSubExps rettype of
    ([], _, _) -> Nothing
    (invariant, mapresult, rettype') ->
      let (pat', resultSubExps') = unzip mapresult
          lambdaRes = Result rescs resultSubExps' resloc
          fun' = fun { lambdaBody = lambdaRes `setBodyResult` lambdaBody fun
                     , lambdaReturnType = rettype'
                     }
      in Just $ (pat', Map cs fun' arrs loc) : invariant
  where inputMap = HM.fromList $ zip (map identName $ lambdaParams fun) arrs
        free = freeInBody $ lambdaBody fun
        rettype = lambdaReturnType fun
        (rescs, resultSubExps, resloc) = bodyResult $ lambdaBody fun
        outersize = arraysSize 0 $ map subExpType arrs

        freeOrConst (Var v)       = v `HS.member` free
        freeOrConst (Constant {}) = True

        checkInvariance (outId, Var v, _) (invariant, mapresult, rettype')
          | Just inp <- HM.lookup (identName v) inputMap =
            (([outId], SubExp inp) : invariant,
             mapresult,
             rettype')
        checkInvariance (outId, e, t) (invariant, mapresult, rettype')
          | freeOrConst e = (([outId], Replicate outersize e loc) : invariant,
                             mapresult,
                             rettype')
          | otherwise = (invariant,
                         (outId, e) : mapresult,
                         t : rettype')
liftIdentityMapping _ _ _ = Nothing

removeReplicateMapping :: SimplificationRule
removeReplicateMapping look pat (Map _ fun arrs _) =
  case mapM isReplicate arrs of
    Just arrs'@((n,_):_) ->
      -- 'n' is the size of the destination array.
      let parameterBnds = [ ([fromParam par], SubExp e)
                            | (par, (_, e)) <- zip (lambdaParams fun) arrs' ]
          (_, resultSubExps, resloc) = bodyResult $ lambdaBody fun
          resultBnds = [ ([v], Replicate n e resloc) | (v, e) <- zip pat resultSubExps ]
      in -- XXX: Throwing away certificates.
         Just $ parameterBnds ++
                bodyLetBindings (lambdaBody fun) ++
                resultBnds
    _ -> Nothing
  where isReplicate (Var v)
          | Just (Replicate n e _) <- look $ identName v = Just (n,e)
        isReplicate _                                    = Nothing
removeReplicateMapping _ _ _ = Nothing

-- A bit hacky for now - get the let-bindings of a body, and fail for
-- any loops and let-withs.  This will be fixed once I rationalise the
-- Body representation.
bodyLetBindings :: Body -> [([Ident], Exp)]
bodyLetBindings (LetPat pat e body _) = (pat,e) : bodyLetBindings body
bodyLetBindings (Result {})           = []
bodyLetBindings _                     = error "bodyLetBindings can only handle let-bindings."