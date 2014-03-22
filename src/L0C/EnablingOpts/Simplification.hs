-- | This module implements simple simplification rules for bindings.
-- The intent is that you pass a symbol-lookup function and a binding,
-- and is given back a sequence of bindings, that are more efficient
-- than the original binding, yet compute the same result.
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
import L0C.Tools

-- | A function that, given a variable name, returns its definition.
type VarLookup = VName -> Maybe Exp

-- | @simplifyBinding lookup bnd@ performs simplification of the
-- binding @bnd@.  If simplification is possible, a replacement list
-- of bindings is returned, that bind at least the same banes as the
-- original binding (and possibly more, for intermediate results).
simplifyBinding :: VarLookup -> Binding -> Maybe [Binding]

simplifyBinding = applyRules simplificationRules

applyRules :: [SimplificationRule]
           -> VarLookup -> Binding -> Maybe [Binding]
applyRules []           _    _   = Nothing
applyRules (rule:rules) look bnd =
  (concatMap subApply <$> rule look bnd) <|>
  applyRules rules look bnd
  where subApply bnd' =
          fromMaybe [bnd'] $ applyRules (rule:rules) look bnd'

type SimplificationRule = VarLookup -> Binding -> Maybe [Binding]

simplificationRules :: [SimplificationRule]
simplificationRules = [ liftIdentityMapping
                      , removeReplicateMapping
                      ]

liftIdentityMapping :: SimplificationRule
liftIdentityMapping _ (LetBind pat (Map cs fun arrs loc)) =
  case foldr checkInvariance ([], [], []) $ zip3 pat resultSubExps rettype of
    ([], _, _) -> Nothing
    (invariant, mapresult, rettype') ->
      let (pat', resultSubExps') = unzip mapresult
          lambdaRes = Result rescs resultSubExps' resloc
          fun' = fun { lambdaBody = lambdaRes `setBodyResult` lambdaBody fun
                     , lambdaReturnType = rettype'
                     }
      in Just $ LetBind pat' (Map cs fun' arrs loc) : invariant
  where inputMap = HM.fromList $ zip (map identName $ lambdaParams fun) arrs
        free = freeInBody $ lambdaBody fun
        rettype = lambdaReturnType fun
        (rescs, resultSubExps, resloc) = bodyResult $ lambdaBody fun
        outersize = arraysSize 0 $ map subExpType arrs

        freeOrConst (Var v)       = v `HS.member` free
        freeOrConst (Constant {}) = True

        checkInvariance (outId, Var v, _) (invariant, mapresult, rettype')
          | Just inp <- HM.lookup (identName v) inputMap =
            (LetBind [outId] (SubExp inp) : invariant,
             mapresult,
             rettype')
        checkInvariance (outId, e, t) (invariant, mapresult, rettype')
          | freeOrConst e = (LetBind [outId] (Replicate outersize e loc) : invariant,
                             mapresult,
                             rettype')
          | otherwise = (invariant,
                         (outId, e) : mapresult,
                         t : rettype')
liftIdentityMapping _ _ = Nothing

removeReplicateMapping :: SimplificationRule
removeReplicateMapping look (LetBind pat (Map _ fun arrs _)) =
  case mapM isReplicate arrs of
    Just arrs'@((n,_):_) ->
      -- 'n' is the size of the destination array.
      let parameterBnds = [ LetBind [fromParam par] $ SubExp e
                            | (par, (_, e)) <- zip (lambdaParams fun) arrs' ]
          (_, resultSubExps, resloc) = bodyResult $ lambdaBody fun
          resultBnds = [ LetBind [v] $ Replicate n e resloc
                           | (v, e) <- zip pat resultSubExps ]
      in -- XXX: Throwing away certificates.
         Just $ parameterBnds ++
                bodyBindings (lambdaBody fun) ++
                resultBnds
    _ -> Nothing
  where isReplicate (Var v)
          | Just (Replicate n e _) <- look $ identName v = Just (n,e)
        isReplicate _                                    = Nothing
removeReplicateMapping _ _ = Nothing
