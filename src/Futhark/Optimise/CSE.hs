module Futhark.Optimise.CSE
       ( performCSE )
       where

import Control.Applicative
import Control.Monad.Reader
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Lazy as M

import Futhark.Representation.AST
import Futhark.Substitute
import Futhark.Binder (Proper)

performCSE :: Proper lore =>
              Prog lore -> Prog lore
performCSE prog =
  prog { progFunctions = map cseInFunDec $ progFunctions prog }

cseInFunDec :: Proper lore =>
               FunDec lore -> FunDec lore
cseInFunDec fundec =
  fundec { funDecBody =
              runReader (cseInBody $ funDecBody fundec) newCSEState
         }

type CSEM lore = Reader (CSEState lore)

cseInBody :: Proper lore =>
             Body lore -> CSEM lore (Body lore)
cseInBody (Body bodyattr bnds res) = do
  bnds' <- cseInBindings bnds
  return $ Body bodyattr bnds' res

cseInBindings :: Proper lore =>
                 [Binding lore] -> CSEM lore [Binding lore]
cseInBindings [] = return []
cseInBindings (bnd:bnds) =
  cseInBinding bnd $ \bnd' ->
    (bnd'++) <$> cseInBindings bnds

cseInBinding :: Proper lore =>
                Binding lore
             -> ([Binding lore] -> CSEM lore a)
             -> CSEM lore a
cseInBinding bnd m
  -- Only CSE on basic expressions.  Arrays and memory blocks may have
  -- complicated annotations that cannot be handled generally.
  | not $ all basicType $ patternTypes $ bindingPattern bnd = m [bnd]
cseInBinding (Let pat eattr e) m = do
  CSEState (esubsts, nsubsts) <- ask
  let e' = substituteNames nsubsts e
  case M.lookup e' esubsts of
    Nothing -> local (addExpSubst pat e') $ m [Let pat eattr e]

    Just subpat ->
      let lets =
            [ Let (Pattern [patElem]) eattr $ PrimOp $ SubExp $ Var v
            | (patElem,v) <- zip (patternElements pat) $ patternIdents subpat
            ]
      in local (addNameSubst pat subpat) $ m lets

newtype CSEState lore =
  CSEState (M.Map (Exp lore) (Pattern lore), HM.HashMap VName VName)

newCSEState :: CSEState lore
newCSEState = CSEState (M.empty, HM.empty)

mkSubsts :: Pattern lore -> Pattern lore -> HM.HashMap VName VName
mkSubsts pat vs = HM.fromList $ zip (patternNames pat) (patternNames vs)

addNameSubst :: Pattern lore -> Pattern lore -> CSEState lore -> CSEState lore
addNameSubst pat subpat (CSEState (esubsts, nsubsts)) =
  CSEState (esubsts, mkSubsts pat subpat `HM.union` nsubsts)

addExpSubst :: Proper lore =>
               Pattern lore -> Exp lore -> CSEState lore -> CSEState lore
addExpSubst pat e (CSEState (esubsts, nsubsts)) =
  CSEState (M.insert e pat esubsts, nsubsts)
