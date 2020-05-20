{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
       ( performCSE
       , performCSEOnFunDef
       , performCSEOnStms
       , CSEInOp
       )
       where

import Control.Monad.Reader
import qualified Data.Map.Strict as M

import Futhark.Analysis.Alias
import Futhark.IR
import Futhark.IR.Prop.Aliases
import Futhark.IR.Aliases
  (removeProgAliases, removeFunDefAliases, removeStmAliases,
   Aliases, consumedInStms)
import qualified Futhark.IR.Kernels.Kernel as Kernel
import qualified Futhark.IR.SOACS.SOAC as SOAC
import qualified Futhark.IR.Mem as Memory
import Futhark.Transform.Substitute
import Futhark.Pass

-- | Perform CSE on every function in a program.
performCSE :: (ASTLore lore, CanBeAliased (Op lore),
               CSEInOp (OpWithAliases (Op lore))) =>
              Bool -> Pass lore lore
performCSE cse_arrays =
  Pass "CSE" "Combine common subexpressions." $
  fmap removeProgAliases .
  intraproceduralTransformationWithConsts onConsts onFun .
  aliasAnalysis
  where onConsts stms =
          pure $ fst $
          runReader (cseInStms (consumedInStms stms) (stmsToList stms) (return ()))
          (newCSEState cse_arrays)
        onFun _ = pure . cseInFunDef cse_arrays

-- | Perform CSE on a single function.
performCSEOnFunDef :: (ASTLore lore, CanBeAliased (Op lore),
                       CSEInOp (OpWithAliases (Op lore))) =>
                      Bool -> FunDef lore -> FunDef lore
performCSEOnFunDef cse_arrays =
  removeFunDefAliases . cseInFunDef cse_arrays . analyseFun

-- | Perform CSE on some statements.
performCSEOnStms :: (ASTLore lore, CanBeAliased (Op lore),
                     CSEInOp (OpWithAliases (Op lore))) =>
                    Bool -> Stms lore -> Stms lore
performCSEOnStms cse_arrays =
  fmap removeStmAliases . f . fst . analyseStms mempty
  where f stms =
          fst $ runReader (cseInStms (consumedInStms stms)
                           (stmsToList stms) (return ()))
          (newCSEState cse_arrays)

cseInFunDef :: (ASTLore lore, Aliased lore, CSEInOp (Op lore)) =>
               Bool -> FunDef lore -> FunDef lore
cseInFunDef cse_arrays fundec =
  fundec { funDefBody =
              runReader (cseInBody ds $ funDefBody fundec) $ newCSEState cse_arrays
         }
  where ds = map (diet . declExtTypeOf) $ funDefRetType fundec

type CSEM lore = Reader (CSEState lore)

cseInBody :: (ASTLore lore, Aliased lore, CSEInOp (Op lore)) =>
             [Diet] -> Body lore -> CSEM lore (Body lore)
cseInBody ds (Body bodydec bnds res) = do
  (bnds', res') <-
    cseInStms (res_cons <> consumedInStms bnds) (stmsToList bnds) $ do
    CSEState (_, nsubsts) _ <- ask
    return $ substituteNames nsubsts res
  return $ Body bodydec bnds' res'
  where res_cons = mconcat $ zipWith consumeResult ds res
        consumeResult Consume se = freeIn se
        consumeResult _ _ = mempty

cseInLambda :: (ASTLore lore, Aliased lore, CSEInOp (Op lore)) =>
               Lambda lore -> CSEM lore (Lambda lore)
cseInLambda lam = do
  body' <- cseInBody (map (const Observe) $ lambdaReturnType lam) $ lambdaBody lam
  return lam { lambdaBody = body' }

cseInStms :: (ASTLore lore, Aliased lore, CSEInOp (Op lore)) =>
             Names -> [Stm lore]
          -> CSEM lore a
          -> CSEM lore (Stms lore, a)
cseInStms _ [] m = do a <- m
                      return (mempty, a)
cseInStms consumed (bnd:bnds) m =
  cseInStm consumed bnd $ \bnd' -> do
    (bnds', a) <- cseInStms consumed bnds m
    bnd'' <- mapM nestedCSE bnd'
    return (stmsFromList bnd''<>bnds', a)
  where nestedCSE bnd' = do
          let ds = map patElemDiet $ patternValueElements $ stmPattern bnd'
          e <- mapExpM (cse ds) $ stmExp bnd'
          return bnd' { stmExp = e }

        cse ds = identityMapper { mapOnBody = const $ cseInBody ds
                                , mapOnOp = cseInOp
                                }

        patElemDiet pe | patElemName pe `nameIn` consumed = Consume
                       | otherwise                        = Observe

cseInStm :: ASTLore lore =>
            Names -> Stm lore
         -> ([Stm lore] -> CSEM lore a)
         -> CSEM lore a
cseInStm consumed (Let pat (StmAux cs attrs edec) e) m = do
  CSEState (esubsts, nsubsts) cse_arrays <- ask
  let e' = substituteNames nsubsts e
      pat' = substituteNames nsubsts pat
  if any (bad cse_arrays) $ patternValueElements pat then
    m [Let pat' (StmAux cs attrs edec) e']
    else
    case M.lookup (edec, e') esubsts of
      Just subpat ->
        local (addNameSubst pat' subpat) $ do
          let lets =
                [ Let (Pattern [] [patElem']) (StmAux cs attrs edec) $
                    BasicOp $ SubExp $ Var $ patElemName patElem
                | (name,patElem) <- zip (patternNames pat') $ patternElements subpat ,
                  let patElem' = patElem { patElemName = name }
                ]
          m lets
      _ -> local (addExpSubst pat' edec e') $
           m [Let pat' (StmAux cs attrs edec) e']

  where bad cse_arrays pe
          | Mem{} <- patElemType pe = True
          | Array{} <- patElemType pe, not cse_arrays = True
          | patElemName pe `nameIn` consumed = True
          | otherwise = False

type ExpressionSubstitutions lore = M.Map
                                    (ExpDec lore, Exp lore)
                                    (Pattern lore)
type NameSubstitutions = M.Map VName VName

data CSEState lore = CSEState
                     { _cseSubstitutions :: (ExpressionSubstitutions lore, NameSubstitutions)
                     , _cseArrays :: Bool
                     }

newCSEState :: Bool -> CSEState lore
newCSEState = CSEState (M.empty, M.empty)

mkSubsts :: PatternT dec -> PatternT dec -> M.Map VName VName
mkSubsts pat vs = M.fromList $ zip (patternNames pat) (patternNames vs)

addNameSubst :: PatternT dec -> PatternT dec -> CSEState lore -> CSEState lore
addNameSubst pat subpat (CSEState (esubsts, nsubsts) cse_arrays) =
  CSEState (esubsts, mkSubsts pat subpat `M.union` nsubsts) cse_arrays

addExpSubst :: ASTLore lore =>
               Pattern lore -> ExpDec lore -> Exp lore
            -> CSEState lore
            -> CSEState lore
addExpSubst pat edec e (CSEState (esubsts, nsubsts) cse_arrays) =
  CSEState (M.insert (edec,e) pat esubsts, nsubsts) cse_arrays

-- | The operations that permit CSE.
class CSEInOp op where
  -- | Perform CSE within any nested expressions.
  cseInOp :: op -> CSEM lore op

instance CSEInOp () where
  cseInOp () = return ()

subCSE :: CSEM lore r -> CSEM otherlore r
subCSE m = do
  CSEState _ cse_arrays <- ask
  return $ runReader m $ newCSEState cse_arrays

instance (ASTLore lore, Aliased lore,
          CSEInOp (Op lore), CSEInOp op) => CSEInOp (Kernel.HostOp lore op) where
  cseInOp (Kernel.SegOp op) = Kernel.SegOp <$> cseInOp op
  cseInOp (Kernel.OtherOp op) = Kernel.OtherOp <$> cseInOp op
  cseInOp x = return x

instance (ASTLore lore, Aliased lore, CSEInOp (Op lore)) =>
         CSEInOp (Kernel.SegOp lvl lore) where
  cseInOp = subCSE .
            Kernel.mapSegOpM
            (Kernel.SegOpMapper return cseInLambda cseInKernelBody return return)

cseInKernelBody :: (ASTLore lore, Aliased lore, CSEInOp (Op lore)) =>
                   Kernel.KernelBody lore -> CSEM lore (Kernel.KernelBody lore)
cseInKernelBody (Kernel.KernelBody bodydec bnds res) = do
  Body _ bnds' _ <- cseInBody (map (const Observe) res) $ Body bodydec bnds []
  return $ Kernel.KernelBody bodydec bnds' res

instance CSEInOp op => CSEInOp (Memory.MemOp op) where
  cseInOp o@Memory.Alloc{} = return o
  cseInOp (Memory.Inner k) = Memory.Inner <$> subCSE (cseInOp k)

instance (ASTLore lore,
          CanBeAliased (Op lore),
          CSEInOp (OpWithAliases (Op lore))) =>
         CSEInOp (SOAC.SOAC (Aliases lore)) where
  cseInOp = subCSE . SOAC.mapSOACM (SOAC.SOACMapper return cseInLambda return)
