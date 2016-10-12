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
       , CSEInOp
       )
       where

import Control.Applicative
import Control.Monad.Reader
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Lazy as M

import Prelude

import Futhark.Analysis.Alias
import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.Aliases
  (removeFunDefAliases, Aliases, consumedInStms)
import qualified Futhark.Representation.Kernels.Kernel as Kernel
import qualified Futhark.Representation.Kernels.KernelExp as KernelExp
import qualified Futhark.Representation.SOACS.SOAC as SOAC
import qualified Futhark.Representation.ExplicitMemory as ExplicitMemory
import Futhark.Transform.Substitute
import Futhark.Pass
import Futhark.Tools

-- | Perform CSE on every functioon in a program.
performCSE :: (Attributes lore, CanBeAliased (Op lore),
               CSEInOp (OpWithAliases (Op lore))) =>
              Bool -> Pass lore lore
performCSE cse_arrays =
  simplePass
  "CSE"
  "Combine common subexpressions." $
  intraproceduralTransformation $
  return . removeFunDefAliases . cseInFunDef cse_arrays . analyseFun

cseInFunDef :: (Attributes lore, Aliased lore, CSEInOp (Op lore)) =>
               Bool -> FunDef lore -> FunDef lore
cseInFunDef cse_arrays fundec =
  fundec { funDefBody =
              runReader (cseInBody $ funDefBody fundec) $ newCSEState cse_arrays
         }

type CSEM lore = Reader (CSEState lore)

cseInBody :: (Attributes lore, Aliased lore, CSEInOp (Op lore)) =>
             Body lore -> CSEM lore (Body lore)
cseInBody (Body bodyattr bnds res) =
  cseInStms (consumedInStms bnds res) bnds $ do
    CSEState (_, nsubsts) _ <- ask
    return $ Body bodyattr [] $ substituteNames nsubsts res

cseInLambda :: (Attributes lore, Aliased lore, CSEInOp (Op lore)) =>
               Lambda lore -> CSEM lore (Lambda lore)
cseInLambda lam = do
  body' <- cseInBody $ lambdaBody lam
  return lam { lambdaBody = body' }

cseInExtLambda :: (Attributes lore, Aliased lore, CSEInOp (Op lore)) =>
                  ExtLambda lore -> CSEM lore (ExtLambda lore)
cseInExtLambda lam = do
  body' <- cseInBody $ extLambdaBody lam
  return lam { extLambdaBody = body' }

cseInStms :: (Attributes lore, Aliased lore, CSEInOp (Op lore)) =>
             Names -> [Stm lore]
          -> CSEM lore (Body lore)
          -> CSEM lore (Body lore)
cseInStms _ [] m = m
cseInStms consumed (bnd:bnds) m =
  cseInStm consumed bnd $ \bnd' -> do
    Body bodyattr bnds' es <- cseInStms consumed bnds m
    bnd'' <- mapM nestedCSE bnd'
    return $ Body bodyattr (bnd''++bnds') es
  where nestedCSE bnd' = do
          e <- mapExpM cse $ bindingExp bnd'
          return bnd' { bindingExp = e }
        cse = identityMapper { mapOnBody = const cseInBody
                             , mapOnOp = cseInOp
                             }

cseInStm :: Attributes lore =>
            Names -> Stm lore
         -> ([Stm lore] -> CSEM lore a)
         -> CSEM lore a
cseInStm consumed (Let pat eattr e) m = do
  CSEState (esubsts, nsubsts) cse_arrays <- ask
  let e' = substituteNames nsubsts e
      pat' = substituteNames nsubsts pat
  if any (bad cse_arrays) $ patternValueElements pat then
    m [Let pat' eattr e']
    else
    case M.lookup (eattr, e') esubsts of
      Just subpat ->
        local (addNameSubst pat' subpat) $ do
          let lets =
                [ Let (Pattern [] [patElem']) eattr $ BasicOp $ SubExp $ Var $ patElemName patElem
                | (name,patElem) <- zip (patternNames pat') $ patternElements subpat ,
                  let patElem' = patElem { patElemName = name }
                ]
          m lets
      _ -> local (addExpSubst pat' eattr e') $ m [Let pat' eattr e']

  where bad cse_arrays pe
          | Mem{} <- patElemType pe = True
          | Array{} <- patElemType pe, not cse_arrays = True
          | patElemName pe `HS.member` consumed = True
          | BindInPlace{} <- patElemBindage pe = True
          | otherwise = False

type ExpressionSubstitutions lore = M.Map
                                    (ExpAttr lore, Exp lore)
                                    (Pattern lore)
type NameSubstitutions = HM.HashMap VName VName

data CSEState lore = CSEState
                     { _cseSubstitutions :: (ExpressionSubstitutions lore, NameSubstitutions)
                     , _cseArrays :: Bool
                     }

newCSEState :: Bool -> CSEState lore
newCSEState = CSEState (M.empty, HM.empty)

mkSubsts :: PatternT attr -> PatternT attr -> HM.HashMap VName VName
mkSubsts pat vs = HM.fromList $ zip (patternNames pat) (patternNames vs)

addNameSubst :: PatternT attr -> PatternT attr -> CSEState lore -> CSEState lore
addNameSubst pat subpat (CSEState (esubsts, nsubsts) cse_arrays) =
  CSEState (esubsts, mkSubsts pat subpat `HM.union` nsubsts) cse_arrays

addExpSubst :: Attributes lore =>
               Pattern lore -> ExpAttr lore -> Exp lore
            -> CSEState lore
            -> CSEState lore
addExpSubst pat eattr e (CSEState (esubsts, nsubsts) cse_arrays) =
  CSEState (M.insert (eattr,e) pat esubsts, nsubsts) cse_arrays

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

instance (Attributes lore, Aliased lore, CSEInOp (Op lore)) => CSEInOp (Kernel.Kernel lore) where
  cseInOp = subCSE .
            Kernel.mapKernelM
            (Kernel.KernelMapper return cseInLambda cseInBody
             return return return cseInKernelBody)

cseInKernelBody :: (Attributes lore, Aliased lore, CSEInOp (Op lore)) =>
                   Kernel.KernelBody lore -> CSEM lore (Kernel.KernelBody lore)
cseInKernelBody (Kernel.KernelBody bodyattr bnds res) = do
  Body _ bnds' _ <- cseInBody $ Body bodyattr bnds []
  return $ Kernel.KernelBody bodyattr bnds' res

instance (Attributes lore, Aliased lore, CSEInOp (Op lore)) => CSEInOp (KernelExp.KernelExp lore) where
  cseInOp (KernelExp.Combine cspace ts active body) =
    subCSE $ KernelExp.Combine cspace ts active <$> cseInBody body
  cseInOp (KernelExp.GroupReduce w lam input) =
    subCSE $ KernelExp.GroupReduce w <$> cseInLambda lam <*> pure input
  cseInOp (KernelExp.GroupStream w max_chunk lam nes arrs) =
    subCSE $ KernelExp.GroupStream w max_chunk <$> cseInGroupStreamLambda lam <*> pure nes <*> pure arrs
  cseInOp op = return op

cseInGroupStreamLambda :: (Attributes lore, Aliased lore, CSEInOp (Op lore)) =>
                          KernelExp.GroupStreamLambda lore
                       -> CSEM lore (KernelExp.GroupStreamLambda lore)
cseInGroupStreamLambda lam = do
  body' <- cseInBody $ KernelExp.groupStreamLambdaBody lam
  return lam { KernelExp.groupStreamLambdaBody = body' }


instance CSEInOp op => CSEInOp (ExplicitMemory.MemOp op) where
  cseInOp o@ExplicitMemory.Alloc{} = return o
  cseInOp (ExplicitMemory.Inner k) = ExplicitMemory.Inner <$> subCSE (cseInOp k)

instance (Attributes lore,
          CanBeAliased (Op lore),
          CSEInOp (OpWithAliases (Op lore))) =>
         CSEInOp (SOAC.SOAC (Aliases lore)) where
  cseInOp = subCSE . SOAC.mapSOACM
            (SOAC.SOACMapper return cseInLambda cseInExtLambda return return)
