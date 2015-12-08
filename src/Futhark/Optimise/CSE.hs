module Futhark.Optimise.CSE
       ( performCSE )
       where

import Control.Monad.Reader
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Lazy as M

import Futhark.Representation.AST
import qualified Futhark.Representation.AST.Annotations as Annotations
import Futhark.Transform.Substitute
import Futhark.Pass
import Futhark.Tools

performCSE :: Proper lore =>
              Pass lore lore
performCSE = simplePass
             "CSE"
             "Combine common subexpressions." $
             intraproceduralTransformation $ return . cseInFunDec

cseInFunDec :: Proper lore =>
               FunDec lore -> FunDec lore
cseInFunDec fundec =
  fundec { funDecBody =
              runReader (cseInBody $ funDecBody fundec) newCSEState
         }

type CSEM lore = Reader (CSEState lore)

cseInBody :: Proper lore =>
             Body lore -> CSEM lore (Body lore)
cseInBody (Body bodyattr bnds res) =
  cseInBindings bnds $ do
    CSEState (_, nsubsts) <- ask
    return $ Body bodyattr [] $ substituteNames nsubsts res

cseInLambda :: Proper lore =>
               Lambda lore -> CSEM lore (Lambda lore)
cseInLambda lam = do
  body' <- cseInBody $ lambdaBody lam
  return lam { lambdaBody = body' }

cseInBindings :: Proper lore =>
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

type ExpressionSubstitutions lore = M.Map (Annotations.Exp lore, Exp lore) (Pattern lore)
type NameSubstitutions = HM.HashMap VName VName

newtype CSEState lore = CSEState (ExpressionSubstitutions lore, NameSubstitutions)

newCSEState :: CSEState lore
newCSEState = CSEState (M.empty, HM.empty)

mkSubsts :: Pattern lore -> Pattern lore -> HM.HashMap VName VName
mkSubsts pat vs = HM.fromList $ zip (patternNames pat) (patternNames vs)

addNameSubst :: Pattern lore -> Pattern lore -> CSEState lore -> CSEState lore
addNameSubst pat subpat (CSEState (esubsts, nsubsts)) =
  CSEState (esubsts, mkSubsts pat subpat `HM.union` nsubsts)

addExpSubst :: Proper lore =>
               Pattern lore -> Annotations.Exp lore -> Exp lore
            -> CSEState lore
            -> CSEState lore
addExpSubst pat eattr e (CSEState (esubsts, nsubsts)) =
  CSEState (M.insert (eattr,e) pat esubsts, nsubsts)
