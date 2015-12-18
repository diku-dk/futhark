{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Futhark.Optimise.CSE
       ( performCSE
       , CSEInOp
       )
       where

import Control.Applicative
import Control.Monad.Reader
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Lazy as M

import Prelude

import Futhark.Representation.AST
import qualified Futhark.Representation.Kernels.Kernel as Kernel
import qualified Futhark.Representation.SOACS.SOAC as SOAC
import qualified Futhark.Representation.ExplicitMemory as ExplicitMemory
import Futhark.Transform.Substitute
import Futhark.Pass
import Futhark.Tools

performCSE :: CSEInOp lore (Op lore) =>
              Pass lore lore
performCSE = simplePass
             "CSE"
             "Combine common subexpressions." $
             intraproceduralTransformation $ return . cseInFunDec

cseInFunDec :: CSEInOp lore (Op lore) =>
               FunDec lore -> FunDec lore
cseInFunDec fundec =
  fundec { funDecBody =
              runReader (cseInBody $ funDecBody fundec) newCSEState
         }

type CSEM lore = Reader (CSEState lore)

cseInBody :: CSEInOp lore (Op lore) =>
             Body lore -> CSEM lore (Body lore)
cseInBody (Body bodyattr bnds res) =
  cseInBindings bnds $ do
    CSEState (_, nsubsts) <- ask
    return $ Body bodyattr [] $ substituteNames nsubsts res

cseInLambda :: CSEInOp lore (Op lore) =>
               Lambda lore -> CSEM lore (Lambda lore)
cseInLambda lam = do
  body' <- cseInBody $ lambdaBody lam
  return lam { lambdaBody = body' }

cseInExtLambda :: CSEInOp lore (Op lore) =>
                  ExtLambda lore -> CSEM lore (ExtLambda lore)
cseInExtLambda lam = do
  body' <- cseInBody $ extLambdaBody lam
  return lam { extLambdaBody = body' }

cseInBindings :: CSEInOp lore (Op lore) =>
                 [Binding lore]
              -> CSEM lore (Body lore)
              -> CSEM lore (Body lore)
cseInBindings [] m = m
cseInBindings (bnd:bnds) m =
  cseInBinding bnd $ \bnd' -> do
    Body bodyattr bnds' es <- cseInBindings bnds m
    bnd'' <- mapM nestedCSE bnd'
    return $ Body bodyattr (bnd''++bnds') es
  where nestedCSE bnd' = do
          e <- mapExpM cse $ bindingExp bnd'
          return bnd' { bindingExp = e }
        cse = identityMapper { mapOnBody = cseInBody
                             , mapOnLambda = cseInLambda
                             , mapOnOp = cseInOp
                             }

cseInBinding :: Proper lore =>
                Binding lore
             -> ([Binding lore] -> CSEM lore a)
             -> CSEM lore a
cseInBinding (Let pat eattr e) m = do
  CSEState (esubsts, nsubsts) <- ask
  let e' = substituteNames nsubsts e
      pat' = substituteNames nsubsts pat
  if any bad $ patternTypes pat then
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
  where bad Array{} = True
        bad Mem{}   = True
        bad _       = False

type ExpressionSubstitutions lore = M.Map
                                    (ExpAttr lore, Exp lore)
                                    (Pattern lore)
type NameSubstitutions = HM.HashMap VName VName

newtype CSEState lore = CSEState (ExpressionSubstitutions lore, NameSubstitutions)

newCSEState :: CSEState lore
newCSEState = CSEState (M.empty, HM.empty)

mkSubsts :: PatternT attr -> PatternT attr -> HM.HashMap VName VName
mkSubsts pat vs = HM.fromList $ zip (patternNames pat) (patternNames vs)

addNameSubst :: PatternT attr -> PatternT attr -> CSEState lore -> CSEState lore
addNameSubst pat subpat (CSEState (esubsts, nsubsts)) =
  CSEState (esubsts, mkSubsts pat subpat `HM.union` nsubsts)

addExpSubst :: Proper lore =>
               Pattern lore -> ExpAttr lore -> Exp lore
            -> CSEState lore
            -> CSEState lore
addExpSubst pat eattr e (CSEState (esubsts, nsubsts)) =
  CSEState (M.insert (eattr,e) pat esubsts, nsubsts)

-- | The operations that permit CSE.
class Proper lore => CSEInOp lore op where
  cseInOp :: op -> CSEM lore op

instance Proper lore => CSEInOp lore () where
  cseInOp () = return ()

instance CSEInOp lore (Op lore) => CSEInOp lore (Kernel.Kernel lore) where
  cseInOp = Kernel.mapKernelM $
            Kernel.KernelMapper return cseInLambda cseInBody
            return return return

instance CSEInOp lore (Op lore) => CSEInOp lore (ExplicitMemory.MemOp lore) where
  cseInOp o@ExplicitMemory.Alloc{} = return o
  cseInOp (ExplicitMemory.Inner k) = ExplicitMemory.Inner <$> cseInOp k

instance CSEInOp lore (Op lore) => CSEInOp lore (SOAC.SOAC lore) where
  cseInOp = SOAC.mapSOACM $
            SOAC.SOACMapper return cseInLambda cseInExtLambda
            return return
