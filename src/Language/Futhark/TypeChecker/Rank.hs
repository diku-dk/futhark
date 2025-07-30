module Language.Futhark.TypeChecker.Rank
  ( rankAnalysis,
    rankAnalysis1,
  )
where

import Control.Monad (void)
import Data.Bifunctor
import Data.Map qualified as M
import Futhark.IR.Pretty ()
import Language.Futhark hiding (ScalarType)
import Language.Futhark.TypeChecker.Constraints

rankAnalysis1 ::
  (Monad m) =>
  SrcLoc ->
  ([CtTy SComp], [CtAM]) ->
  TyVars SComp ->
  M.Map TyVar (CtType SComp) ->
  [Pat ParamType] ->
  Exp ->
  Maybe (TypeExp Exp VName) ->
  m
    ( ([CtTy ()], M.Map TyVar (CtType ()), TyVars ()),
      [Pat ParamType],
      Exp,
      Maybe (TypeExp Exp VName)
    )
rankAnalysis1 _loc (cs, _cs_am) tyVars artificial params body retdecl =
  pure
    ( ( map void cs,
        M.map (first (const ())) artificial,
        fmap (second void) tyVars
      ),
      params,
      body,
      retdecl
    )

rankAnalysis ::
  (Monad m) =>
  SrcLoc ->
  ([CtTy SComp], [CtAM]) ->
  TyVars SComp ->
  M.Map TyVar (CtType SComp) ->
  [Pat ParamType] ->
  Exp ->
  Maybe (TypeExp Exp VName) ->
  m
    [ ( ([CtTy ()], M.Map TyVar (CtType ()), TyVars ()),
        [Pat ParamType],
        Exp,
        Maybe (TypeExp Exp VName)
      )
    ]
rankAnalysis _loc (cs, _cs_am) tyVars artificial params body retdecl = do
  pure
    [ ( ( map void cs,
          M.map (first (const ())) artificial,
          fmap (second void) tyVars
        ),
        params,
        body,
        retdecl
      )
    ]
