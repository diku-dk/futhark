module Futhark.Optimise.CSE
       ( performCSE )
       where

import Control.Monad.Reader
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Lazy as M

import Futhark.Representation.AST
import qualified Futhark.Representation.AST.Lore as Lore
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
cseInBody (Body bodyattr bnds res) =
  cseInBindings bnds $ do
    CSEState (_, nsubsts) <- ask
    return $ Body bodyattr [] $ substituteNames nsubsts res

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
        cse = identityMapper { mapOnBody = cseInBody }

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
          CSEState (_, nsubsts') <- ask
          let lets =
                [ Let (Pattern [] [patElem']) eattr $ PrimOp $ SubExp $ Var v
                | (patElem,v) <- zip (patternElements pat') $ patternNames subpat,
                  let patElem' = setPatElemName (substituteNames nsubsts' patElem) $
                                 patElemName patElem
                ]
          m lets
  where bad (Array _ _ Unique) = True
        bad (Mem _)            = True
        bad _                  = False
        setPatElemName patElem name =
          patElem { patElemIdent = Ident name $ identType $ patElemIdent patElem }

newtype CSEState lore =
  CSEState (M.Map (Lore.Exp lore, Exp lore) (Pattern lore), HM.HashMap VName VName)

newCSEState :: CSEState lore
newCSEState = CSEState (M.empty, HM.empty)

mkSubsts :: Pattern lore -> Pattern lore -> HM.HashMap VName VName
mkSubsts pat vs = HM.fromList $ zip (patternNames pat) (patternNames vs)

addNameSubst :: Pattern lore -> Pattern lore -> CSEState lore -> CSEState lore
addNameSubst pat subpat (CSEState (esubsts, nsubsts)) =
  CSEState (esubsts, mkSubsts pat subpat `HM.union` nsubsts)

addExpSubst :: Proper lore =>
               Pattern lore -> Lore.Exp lore -> Exp lore
            -> CSEState lore
            -> CSEState lore
addExpSubst pat eattr e (CSEState (esubsts, nsubsts)) =
  CSEState (M.insert (eattr,e) pat esubsts, nsubsts)
