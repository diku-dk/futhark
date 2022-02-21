{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
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
import qualified Futhark.IR.GPU as GPU
import qualified Futhark.IR.MC as MC
import qualified Futhark.IR.Mem as Memory
import Futhark.IR.Prop.Aliases
import qualified Futhark.IR.SOACS.SOAC as SOAC
import Futhark.Pass
import Futhark.Transform.Substitute

-- | Perform CSE on every function in a program.
--
-- If the boolean argument is false, the pass will not perform CSE on
-- expressions producing arrays. This should be disabled when the rep has
-- memory information, since at that point arrays have identity beyond their
-- value.
performCSE ::
  ( ASTRep rep,
    CanBeAliased (Op rep),
    CSEInOp (OpWithAliases (Op rep))
  ) =>
  Bool ->
  Pass rep rep
performCSE cse_arrays =
  Pass "CSE" "Combine common subexpressions." $
    fmap removeProgAliases
      . intraproceduralTransformationWithConsts onConsts onFun
      . aliasAnalysis
  where
    onConsts stms =
      pure $
        fst $
          runReader
            (cseInStms (consumedInStms stms) (stmsToList stms) (return ()))
            (newCSEState cse_arrays)
    onFun _ = pure . cseInFunDef cse_arrays

-- | Perform CSE on a single function.
--
-- If the boolean argument is false, the pass will not perform CSE on
-- expressions producing arrays. This should be disabled when the rep has
-- memory information, since at that point arrays have identity beyond their
-- value.
performCSEOnFunDef ::
  ( ASTRep rep,
    CanBeAliased (Op rep),
    CSEInOp (OpWithAliases (Op rep))
  ) =>
  Bool ->
  FunDef rep ->
  FunDef rep
performCSEOnFunDef cse_arrays =
  removeFunDefAliases . cseInFunDef cse_arrays . analyseFun

-- | Perform CSE on some statements.
--
-- If the boolean argument is false, the pass will not perform CSE on
-- expressions producing arrays. This should be disabled when the rep has
-- memory information, since at that point arrays have identity beyond their
-- value.
performCSEOnStms ::
  ( ASTRep rep,
    CanBeAliased (Op rep),
    CSEInOp (OpWithAliases (Op rep))
  ) =>
  Bool ->
  Stms rep ->
  Stms rep
performCSEOnStms cse_arrays =
  fmap removeStmAliases . f . fst . analyseStms mempty
  where
    f stms =
      fst $
        runReader
          ( cseInStms
              (consumedInStms stms)
              (stmsToList stms)
              (return ())
          )
          (newCSEState cse_arrays)

cseInFunDef ::
  (ASTRep rep, Aliased rep, CSEInOp (Op rep)) =>
  Bool ->
  FunDef rep ->
  FunDef rep
cseInFunDef cse_arrays fundec =
  fundec
    { funDefBody =
        runReader (cseInBody ds $ funDefBody fundec) $ newCSEState cse_arrays
    }
  where
    -- XXX: we treat every non-entry result as a consumption here, because we
    -- our core language is not strong enough to fully capture the
    -- aliases we want, so we are turning some parts off (see #803,
    -- #1241, and the related comment in TypeCheck.hs).  This is not a
    -- practical problem while we still perform such aggressive
    -- inlining.
    ds
      | isJust $ funDefEntryPoint fundec = map (diet . declExtTypeOf) $ funDefRetType fundec
      | otherwise = map retDiet $ funDefRetType fundec
    retDiet t
      | primType $ declExtTypeOf t = Observe
      | otherwise = Consume

type CSEM rep = Reader (CSEState rep)

cseInBody ::
  (ASTRep rep, Aliased rep, CSEInOp (Op rep)) =>
  [Diet] ->
  Body rep ->
  CSEM rep (Body rep)
cseInBody ds (Body bodydec stms res) = do
  (stms', res') <-
    cseInStms (res_cons <> stms_cons) (stmsToList stms) $ do
      CSEState (_, nsubsts) _ <- ask
      return $ substituteNames nsubsts res
  return $ Body bodydec stms' res'
  where
    (res_als, stms_cons) = mkStmsAliases stms res
    res_cons = mconcat $ zipWith consumeResult ds res_als
    consumeResult Consume als = als
    consumeResult _ _ = mempty

cseInLambda ::
  (ASTRep rep, Aliased rep, CSEInOp (Op rep)) =>
  Lambda rep ->
  CSEM rep (Lambda rep)
cseInLambda lam = do
  body' <- cseInBody (map (const Observe) $ lambdaReturnType lam) $ lambdaBody lam
  return lam {lambdaBody = body'}

cseInStms ::
  (ASTRep rep, Aliased rep, CSEInOp (Op rep)) =>
  Names ->
  [Stm rep] ->
  CSEM rep a ->
  CSEM rep (Stms rep, a)
cseInStms _ [] m = do
  a <- m
  return (mempty, a)
cseInStms consumed (stm : stms) m =
  cseInStm consumed stm $ \stm' -> do
    (stms', a) <- cseInStms consumed stms m
    stm'' <- mapM nestedCSE stm'
    return (stmsFromList stm'' <> stms', a)
  where
    nestedCSE stm' = do
      let ds =
            case stmExp stm' of
              DoLoop merge _ _ -> map (diet . declTypeOf . fst) merge
              _ -> map patElemDiet $ patElems $ stmPat stm'
      e <- mapExpM (cse ds) $ stmExp stm'
      return stm' {stmExp = e}

    cse ds =
      identityMapper
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
  ASTRep rep =>
  Names ->
  Stm rep ->
  ([Stm rep] -> CSEM rep a) ->
  CSEM rep a
cseInStm consumed (Let pat (StmAux cs attrs edec) e) m = do
  CSEState (esubsts, nsubsts) cse_arrays <- ask
  let e' = normExp $substituteNames nsubsts e
      pat' = substituteNames nsubsts pat
  if any (bad cse_arrays) $ patElems pat
    then m [Let pat' (StmAux cs attrs edec) e']
    else case M.lookup (edec, e') esubsts of
      Just subpat ->
        local (addNameSubst pat' subpat) $ do
          let lets =
                [ Let (Pat [patElem']) (StmAux cs attrs edec) $
                    BasicOp $ SubExp $ Var $ patElemName patElem
                  | (name, patElem) <- zip (patNames pat') $ patElems subpat,
                    let patElem' = patElem {patElemName = name}
                ]
          m lets
      _ ->
        local (addExpSubst pat' edec e') $
          m [Let pat' (StmAux cs attrs edec) e']
  where
    bad cse_arrays pe
      | Mem {} <- patElemType pe = True
      | Array {} <- patElemType pe, not cse_arrays = True
      | patElemName pe `nameIn` consumed = True
      | otherwise = False

type ExpressionSubstitutions rep =
  M.Map
    (ExpDec rep, Exp rep)
    (Pat (LetDec rep))

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
  ASTRep rep =>
  Pat (LetDec rep) ->
  ExpDec rep ->
  Exp rep ->
  CSEState rep ->
  CSEState rep
addExpSubst pat edec e (CSEState (esubsts, nsubsts) cse_arrays) =
  CSEState (M.insert (edec, e) pat esubsts, nsubsts) cse_arrays

-- | The operations that permit CSE.
class CSEInOp op where
  -- | Perform CSE within any nested expressions.
  cseInOp :: op -> CSEM rep op

instance CSEInOp () where
  cseInOp () = return ()

subCSE :: CSEM rep r -> CSEM otherrep r
subCSE m = do
  CSEState _ cse_arrays <- ask
  return $ runReader m $ newCSEState cse_arrays

instance
  ( ASTRep rep,
    Aliased rep,
    CSEInOp (Op rep),
    CSEInOp op
  ) =>
  CSEInOp (GPU.HostOp rep op)
  where
  cseInOp (GPU.SegOp op) = GPU.SegOp <$> cseInOp op
  cseInOp (GPU.OtherOp op) = GPU.OtherOp <$> cseInOp op
  cseInOp x = return x

instance
  ( ASTRep rep,
    Aliased rep,
    CSEInOp (Op rep),
    CSEInOp op
  ) =>
  CSEInOp (MC.MCOp rep op)
  where
  cseInOp (MC.ParOp par_op op) =
    MC.ParOp <$> traverse cseInOp par_op <*> cseInOp op
  cseInOp (MC.OtherOp op) =
    MC.OtherOp <$> cseInOp op

instance
  (ASTRep rep, Aliased rep, CSEInOp (Op rep)) =>
  CSEInOp (GPU.SegOp lvl rep)
  where
  cseInOp =
    subCSE
      . GPU.mapSegOpM
        (GPU.SegOpMapper return cseInLambda cseInKernelBody return return)

cseInKernelBody ::
  (ASTRep rep, Aliased rep, CSEInOp (Op rep)) =>
  GPU.KernelBody rep ->
  CSEM rep (GPU.KernelBody rep)
cseInKernelBody (GPU.KernelBody bodydec stms res) = do
  Body _ stms' _ <- cseInBody (map (const Observe) res) $ Body bodydec stms []
  return $ GPU.KernelBody bodydec stms' res

instance CSEInOp op => CSEInOp (Memory.MemOp op) where
  cseInOp o@Memory.Alloc {} = return o
  cseInOp (Memory.Inner k) = Memory.Inner <$> subCSE (cseInOp k)

instance
  ( ASTRep rep,
    CanBeAliased (Op rep),
    CSEInOp (OpWithAliases (Op rep))
  ) =>
  CSEInOp (SOAC.SOAC (Aliases rep))
  where
  cseInOp = subCSE . SOAC.mapSOACM (SOAC.SOACMapper return cseInLambda return)
