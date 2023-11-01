{-# LANGUAGE UndecidableInstances #-}

-- | This module implements common-subexpression elimination.  This
-- module does not actually remove the duplicate, but only replaces
-- one with a diference to the other.  E.g:
--
-- @
--   let a = x + y
--   let b = x + y
-- @
--
-- becomes:
--
-- @
--   let a = x + y
--   let b = a
-- @
--
-- After which copy propagation in the simplifier will actually remove
-- the definition of @b@.
--
-- Our CSE is still rather stupid.  No normalisation is performed, so
-- the expressions @x+y@ and @y+x@ will be considered distinct.
-- Furthermore, no expression with its own binding will be considered
-- equal to any other, since the variable names will be distinct.
-- This affects SOACs in particular.
module Futhark.Optimise.CSE
  ( performCSE,
    performCSEOnFunDef,
    performCSEOnStms,
    CSEInOp,
  )
where

import Control.Monad.Reader
import Data.Map.Strict qualified as M
import Futhark.Analysis.Alias
import Futhark.IR
import Futhark.IR.Aliases
  ( Aliases,
    consumedInStms,
    mkStmsAliases,
    removeFunDefAliases,
    removeProgAliases,
    removeStmAliases,
  )
import Futhark.IR.GPU qualified as GPU
import Futhark.IR.MC qualified as MC
import Futhark.IR.Mem qualified as Memory
import Futhark.IR.Prop.Aliases
import Futhark.IR.SOACS.SOAC qualified as SOAC
import Futhark.Pass
import Futhark.Transform.Substitute

-- | Perform CSE on every function in a program.
--
-- If the boolean argument is false, the pass will not perform CSE on
-- expressions producing arrays. This should be disabled when the rep has
-- memory information, since at that point arrays have identity beyond their
-- value.
performCSE ::
  (AliasableRep rep, CSEInOp (Op (Aliases rep))) =>
  Bool ->
  Pass rep rep
performCSE cse_arrays =
  Pass "CSE" "Combine common subexpressions." $ \prog ->
    fmap removeProgAliases
      . intraproceduralTransformationWithConsts
        (onConsts (freeIn (progFuns prog)))
        onFun
      . aliasAnalysis
      $ prog
  where
    onConsts free_in_funs stms = do
      let free_list = namesToList free_in_funs
          (res_als, stms_cons) = mkStmsAliases stms $ varsRes free_list
      pure . fst $
        runReader
          ( cseInStms
              (mconcat res_als <> stms_cons)
              (stmsToList stms)
              (pure ())
          )
          (newCSEState cse_arrays)
    onFun _ = pure . cseInFunDef cse_arrays

-- | Perform CSE on a single function.
--
-- If the boolean argument is false, the pass will not perform CSE on
-- expressions producing arrays. This should be disabled when the rep has
-- memory information, since at that point arrays have identity beyond their
-- value.
performCSEOnFunDef ::
  (AliasableRep rep, CSEInOp (Op (Aliases rep))) =>
  Bool ->
  FunDef rep ->
  FunDef rep
performCSEOnFunDef cse_arrays =
  removeFunDefAliases . cseInFunDef cse_arrays . analyseFun

-- | Perform CSE on some statements.
performCSEOnStms ::
  (AliasableRep rep, CSEInOp (Op (Aliases rep))) =>
  Stms rep ->
  Stms rep
performCSEOnStms =
  fmap removeStmAliases . f . fst . analyseStms mempty
  where
    f stms =
      fst $
        runReader
          (cseInStms (consumedInStms stms) (stmsToList stms) (pure ()))
          -- It is never safe to CSE arrays in stms in isolation,
          -- because we might introduce additional aliasing.
          (newCSEState False)

cseInFunDef ::
  (Aliased rep, CSEInOp (Op rep)) =>
  Bool ->
  FunDef rep ->
  FunDef rep
cseInFunDef cse_arrays fundec =
  fundec
    { funDefBody =
        runReader (cseInBody ds $ funDefBody fundec) $ newCSEState cse_arrays
    }
  where
    -- XXX: we treat every array result as a consumption here, because
    -- it is otherwise complicated to ensure we do not introduce more
    -- aliasing than specified by the return type. This is not a
    -- practical problem while we still perform such aggressive
    -- inlining.
    ds = map (retDiet . fst) $ funDefRetType fundec
    retDiet t
      | primType $ declExtTypeOf t = Observe
      | otherwise = Consume

type CSEM rep = Reader (CSEState rep)

cseInBody ::
  (Aliased rep, CSEInOp (Op rep)) =>
  [Diet] ->
  Body rep ->
  CSEM rep (Body rep)
cseInBody ds (Body bodydec stms res) = do
  (stms', res') <-
    cseInStms (res_cons <> stms_cons) (stmsToList stms) $ do
      CSEState (_, nsubsts) _ <- ask
      pure $ substituteNames nsubsts res
  pure $ Body bodydec stms' res'
  where
    (res_als, stms_cons) = mkStmsAliases stms res
    res_cons = mconcat $ zipWith consumeResult ds res_als
    consumeResult Consume als = als
    consumeResult _ _ = mempty

cseInLambda ::
  (Aliased rep, CSEInOp (Op rep)) =>
  Lambda rep ->
  CSEM rep (Lambda rep)
cseInLambda lam = do
  body' <- cseInBody (map (const Observe) $ lambdaReturnType lam) $ lambdaBody lam
  pure lam {lambdaBody = body'}

cseInStms ::
  forall rep a.
  (Aliased rep, CSEInOp (Op rep)) =>
  Names ->
  [Stm rep] ->
  CSEM rep a ->
  CSEM rep (Stms rep, a)
cseInStms _ [] m = do
  a <- m
  pure (mempty, a)
cseInStms consumed (stm : stms) m =
  cseInStm consumed stm $ \stm' -> do
    (stms', a) <- cseInStms consumed stms m
    stm'' <- mapM nestedCSE stm'
    pure (stmsFromList stm'' <> stms', a)
  where
    nestedCSE stm' = do
      let ds =
            case stmExp stm' of
              Loop merge _ _ -> map (diet . declTypeOf . fst) merge
              _ -> map patElemDiet $ patElems $ stmPat stm'
      e <- mapExpM (cse ds) $ stmExp stm'
      pure stm' {stmExp = e}

    cse ds =
      (identityMapper @rep)
        { mapOnBody = const $ cseInBody ds,
          mapOnOp = cseInOp
        }

    patElemDiet pe
      | patElemName pe `nameIn` consumed = Consume
      | otherwise = Observe

-- A small amount of normalisation of expressions that otherwise would
-- be different for pointless reasons.
normExp :: Exp lore -> Exp lore
normExp (Apply fname args ret (safety, _, _)) =
  Apply fname args ret (safety, mempty, mempty)
normExp e = e

cseInStm ::
  (ASTRep rep) =>
  Names ->
  Stm rep ->
  ([Stm rep] -> CSEM rep a) ->
  CSEM rep a
cseInStm consumed (Let pat (StmAux cs attrs edec) e) m = do
  CSEState (esubsts, nsubsts) cse_arrays <- ask
  let e' = normExp $ substituteNames nsubsts e
      pat' = substituteNames nsubsts pat
  if not (alreadyAliases e) && any (bad cse_arrays) (patElems pat)
    then m [Let pat' (StmAux cs attrs edec) e']
    else case M.lookup (edec, e') esubsts of
      Just (subcs, subpat) -> do
        let subsumes = all (`elem` unCerts subcs) (unCerts cs)
        -- We can only do a plain name substitution if it doesn't
        -- violate any certificate dependencies.
        local (if subsumes then addNameSubst pat' subpat else id) $ do
          let lets =
                [ Let (Pat [patElem']) (StmAux cs attrs edec) $
                    BasicOp (SubExp $ Var $ patElemName patElem)
                  | (name, patElem) <- zip (patNames pat') $ patElems subpat,
                    let patElem' = patElem {patElemName = name}
                ]
          m lets
      _ ->
        local (addExpSubst pat' edec cs e') $
          m [Let pat' (StmAux cs attrs edec) e']
  where
    alreadyAliases (BasicOp Index {}) = True
    alreadyAliases (BasicOp Reshape {}) = True
    alreadyAliases _ = False
    bad cse_arrays pe
      | Mem {} <- patElemType pe = True
      | Array {} <- patElemType pe, not cse_arrays = True
      | patElemName pe `nameIn` consumed = True
      | otherwise = False

type ExpressionSubstitutions rep =
  M.Map
    (ExpDec rep, Exp rep)
    (Certs, Pat (LetDec rep))

type NameSubstitutions = M.Map VName VName

data CSEState rep = CSEState
  { _cseSubstitutions :: (ExpressionSubstitutions rep, NameSubstitutions),
    _cseArrays :: Bool
  }

newCSEState :: Bool -> CSEState rep
newCSEState = CSEState (M.empty, M.empty)

mkSubsts :: Pat dec -> Pat dec -> M.Map VName VName
mkSubsts pat vs = M.fromList $ zip (patNames pat) (patNames vs)

addNameSubst :: Pat dec -> Pat dec -> CSEState rep -> CSEState rep
addNameSubst pat subpat (CSEState (esubsts, nsubsts) cse_arrays) =
  CSEState (esubsts, mkSubsts pat subpat `M.union` nsubsts) cse_arrays

addExpSubst ::
  (ASTRep rep) =>
  Pat (LetDec rep) ->
  ExpDec rep ->
  Certs ->
  Exp rep ->
  CSEState rep ->
  CSEState rep
addExpSubst pat edec cs e (CSEState (esubsts, nsubsts) cse_arrays) =
  CSEState (M.insert (edec, e) (cs, pat) esubsts, nsubsts) cse_arrays

-- | The operations that permit CSE.
class CSEInOp op where
  -- | Perform CSE within any nested expressions.
  cseInOp :: op -> CSEM rep op

instance CSEInOp (NoOp rep) where
  cseInOp NoOp = pure NoOp

subCSE :: CSEM rep r -> CSEM otherrep r
subCSE m = do
  CSEState _ cse_arrays <- ask
  pure $ runReader m $ newCSEState cse_arrays

instance
  ( Aliased rep,
    CSEInOp (Op rep),
    CSEInOp (op rep)
  ) =>
  CSEInOp (GPU.HostOp op rep)
  where
  cseInOp (GPU.SegOp op) = GPU.SegOp <$> cseInOp op
  cseInOp (GPU.OtherOp op) = GPU.OtherOp <$> cseInOp op
  cseInOp (GPU.GPUBody types body) =
    subCSE $ GPU.GPUBody types <$> cseInBody (map (const Observe) types) body
  cseInOp x = pure x

instance
  ( Aliased rep,
    CSEInOp (Op rep),
    CSEInOp (op rep)
  ) =>
  CSEInOp (MC.MCOp op rep)
  where
  cseInOp (MC.ParOp par_op op) =
    MC.ParOp <$> traverse cseInOp par_op <*> cseInOp op
  cseInOp (MC.OtherOp op) =
    MC.OtherOp <$> cseInOp op

instance
  (Aliased rep, CSEInOp (Op rep)) =>
  CSEInOp (GPU.SegOp lvl rep)
  where
  cseInOp =
    subCSE
      . GPU.mapSegOpM
        (GPU.SegOpMapper pure cseInLambda cseInKernelBody pure pure)

cseInKernelBody ::
  (Aliased rep, CSEInOp (Op rep)) =>
  GPU.KernelBody rep ->
  CSEM rep (GPU.KernelBody rep)
cseInKernelBody (GPU.KernelBody bodydec stms res) = do
  Body _ stms' _ <- cseInBody (map (const Observe) res) $ Body bodydec stms []
  pure $ GPU.KernelBody bodydec stms' res

instance (CSEInOp (op rep)) => CSEInOp (Memory.MemOp op rep) where
  cseInOp o@Memory.Alloc {} = pure o
  cseInOp (Memory.Inner k) = Memory.Inner <$> subCSE (cseInOp k)

instance
  (AliasableRep rep, CSEInOp (Op (Aliases rep))) =>
  CSEInOp (SOAC.SOAC (Aliases rep))
  where
  cseInOp = subCSE . SOAC.mapSOACM (SOAC.SOACMapper pure cseInLambda pure)
