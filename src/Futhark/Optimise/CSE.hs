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
import Data.Monoid

import Prelude

import Futhark.Analysis.Alias
import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.Aliases (removeFunDefAliases, Aliases)
import qualified Futhark.Representation.Kernels.Kernel as Kernel
import qualified Futhark.Representation.SOACS.SOAC as SOAC
import qualified Futhark.Representation.ExplicitMemory as ExplicitMemory
import Futhark.Transform.Substitute
import Futhark.Pass
import Futhark.Tools

-- | Perform CSE on every functioon in a program.
performCSE :: (Attributes lore, CanBeAliased (Op lore),
               CSEInOp (Aliases lore) (OpWithAliases (Op lore))) =>
              Bool -> Pass lore lore
performCSE cse_arrays =
  simplePass
  "CSE"
  "Combine common subexpressions." $
  intraproceduralTransformation $
  return . removeFunDefAliases . cseInFunDef cse_arrays . analyseFun

cseInFunDef :: (Aliased lore, CSEInOp lore (Op lore)) =>
               Bool -> FunDef lore -> FunDef lore
cseInFunDef cse_arrays fundec =
  fundec { funDefBody =
              runReader (cseInBody $ funDefBody fundec) $ newCSEState cse_arrays
         }

type CSEM lore = Reader (CSEState lore)

cseInBody :: (Aliased lore, CSEInOp lore (Op lore)) =>
             Body lore -> CSEM lore (Body lore)
cseInBody (Body bodyattr bnds res) =
  cseInBindings (mconcat $ map consumedInBinding bnds) bnds $ do
    CSEState (_, nsubsts) _ <- ask
    return $ Body bodyattr [] $ substituteNames nsubsts res

cseInLambda :: (Aliased lore, CSEInOp lore (Op lore)) =>
               Lambda lore -> CSEM lore (Lambda lore)
cseInLambda lam = do
  body' <- cseInBody $ lambdaBody lam
  return lam { lambdaBody = body' }

cseInExtLambda :: (Aliased lore, CSEInOp lore (Op lore)) =>
                  ExtLambda lore -> CSEM lore (ExtLambda lore)
cseInExtLambda lam = do
  body' <- cseInBody $ extLambdaBody lam
  return lam { extLambdaBody = body' }

cseInBindings :: (Aliased lore, CSEInOp lore (Op lore)) =>
                 Names -> [Binding lore]
              -> CSEM lore (Body lore)
              -> CSEM lore (Body lore)
cseInBindings _ [] m = m
cseInBindings consumed (bnd:bnds) m =
  cseInBinding consumed bnd $ \bnd' -> do
    Body bodyattr bnds' es <- cseInBindings consumed bnds m
    bnd'' <- mapM nestedCSE bnd'
    return $ Body bodyattr (bnd''++bnds') es
  where nestedCSE bnd' = do
          e <- mapExpM cse $ bindingExp bnd'
          return bnd' { bindingExp = e }
        cse = identityMapper { mapOnBody = cseInBody
                             , mapOnOp = cseInOp
                             }

cseInBinding :: Attributes lore =>
                Names -> Binding lore
             -> ([Binding lore] -> CSEM lore a)
             -> CSEM lore a
cseInBinding consumed (Let pat eattr e) m = do
  CSEState (esubsts, nsubsts) cse_arrays <- ask
  let e' = substituteNames nsubsts e
      pat' = substituteNames nsubsts pat
  if any (bad cse_arrays) $ patternValueElements pat then
    m [Let pat' eattr e']
    else
    case M.lookup (eattr, e') esubsts of
      Nothing -> local (addExpSubst pat' eattr e') $ m [Let pat' eattr e']

      Just subpat ->
        local (addNameSubst pat' subpat) $ do
          let lets =
                [ Let (Pattern [] [patElem']) eattr $ PrimOp $ SubExp $ Var $ patElemName patElem
                | (name,patElem) <- zip (patternNames pat') $ patternElements subpat ,
                  let patElem' = patElem { patElemName = name }
                ]
          m lets
  where bad cse_arrays pat_elem
          | Mem{} <- patElemType pat_elem = True
          | Array{} <- patElemType pat_elem, not cse_arrays = True
          | patElemName pat_elem `HS.member` consumed = True
          | BindInPlace{} <- patElemBindage pat_elem = True
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
class Attributes lore => CSEInOp lore op where
  -- | Perform CSE within any nested expressions.
  cseInOp :: Aliased lore => op -> CSEM lore op

instance Attributes lore => CSEInOp lore () where
  cseInOp () = return ()

instance (Attributes lore, CSEInOp lore (Op lore)) => CSEInOp lore (Kernel.Kernel lore) where
  cseInOp = Kernel.mapKernelM $
            Kernel.KernelMapper return cseInLambda cseInBody
            return return return

instance (Attributes (Aliases lore),
          CanBeAliased (Op lore),
          CSEInOp (Aliases lore) (OpWithAliases (Op lore))) =>
         CSEInOp (Aliases lore) (ExplicitMemory.MemOp (Aliases lore)) where
  cseInOp o@ExplicitMemory.Alloc{} = return o
  cseInOp (ExplicitMemory.Inner k) = ExplicitMemory.Inner <$> cseInOp k

instance (Attributes (Aliases lore),
          CanBeAliased (Op lore),
          CSEInOp (Aliases lore) (OpWithAliases (Op lore))) =>
         CSEInOp (Aliases lore) (SOAC.SOAC (Aliases lore)) where
  cseInOp = SOAC.mapSOACM $
            SOAC.SOACMapper return cseInLambda cseInExtLambda
            return return
