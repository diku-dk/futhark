module Futhark.Pass.InitNames (initNamesPass) where

import Control.Monad
import Control.Monad.Writer
import Data.Semigroup
import Futhark.IR
import Futhark.IR.Traversals
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplify.Rep
import Futhark.Pass
import Language.Futhark (maxIntrinsicTag)

type MaxNames rep = (Informing rep, TraverseOpStms (Wise rep))

type MaxMonad = Writer (Max Int)

-- TODO: run by default in dev?
initNamesPass :: (MaxNames rep) => Pass rep rep
initNamesPass =
  Pass
    "init-names"
    "Initialise name source to avoid name clashes"
    initNames

initNames :: (MaxNames rep) => Prog rep -> PassM (Prog rep)
initNames p = do
  let maxName = (+ 1) $ getMax $ snd $ runWriter $ maxNameProg p
  putNameSource (newNameSource maxName)
  pure p

maxNameProg :: (MaxNames rep) => Prog rep -> MaxMonad ()
maxNameProg (Prog ts consts funs) = do
  tell $ Max $ maxIntrinsicTag
  maxNameStms $ informStms consts
  mapM_ (maxNameFun . informFunDef) funs

maxNameFun :: (TraverseOpStms rep) => FunDef rep -> MaxMonad ()
maxNameFun (FunDef _entry _attrs _name _retType params body) =
  mapM_ maxNameParam params >> maxNameBody body

maxNameBody :: (TraverseOpStms rep) => Body rep -> MaxMonad ()
maxNameBody (Body _ stms res) = maxNameStms stms >> mapM_ maxNameSubExpRes res

maxNameStms :: (TraverseOpStms rep) => Stms rep -> MaxMonad ()
maxNameStms = mapM_ maxNameStm

maxNameStm :: (TraverseOpStms rep) => Stm rep -> MaxMonad ()
maxNameStm (Let pat _ e) = maxNamePat pat >> maxNameExp e

maxNameExp :: (TraverseOpStms rep) => Exp rep -> MaxMonad ()
maxNameExp = walkExpM maxNameWalker

maxNameWalker :: forall rep. (TraverseOpStms rep) => Walker rep MaxMonad
maxNameWalker =
  (identityWalker @rep)
    { walkOnSubExp = maxNameSubExp,
      walkOnBody = const maxNameBody,
      walkOnVName = maxNameVName,
      walkOnFParam = maxNameParam,
      walkOnLParam = maxNameParam,
      walkOnOp = maxNameOp
    }

maxNameOp :: (TraverseOpStms rep) => Op rep -> MaxMonad ()
maxNameOp op = void $ traverseOpStms (maxNameOpStms) op

maxNameOpStms ::
  (TraverseOpStms rep) =>
  Scope rep ->
  Stms rep ->
  MaxMonad (Stms rep)
maxNameOpStms _ stms = maxNameStms stms >> pure stms

maxNameSubExpRes :: SubExpRes -> MaxMonad ()
maxNameSubExpRes (SubExpRes _ subExp) = maxNameSubExp subExp

maxNamePat :: Pat dec -> MaxMonad ()
maxNamePat (Pat patElems) = mapM_ maxNamePatElem patElems

maxNameSubExp :: SubExp -> MaxMonad ()
maxNameSubExp (Var vName) = maxNameVName vName
maxNameSubExp _ = pure ()

maxNamePatElem :: PatElem dec -> MaxMonad ()
maxNamePatElem (PatElem vName _) = maxNameVName vName

maxNameParam :: Param dec -> MaxMonad ()
maxNameParam (Param _ vName _) = maxNameVName vName

maxNameVName :: VName -> MaxMonad ()
maxNameVName (VName _ i) = tell $ Max i
