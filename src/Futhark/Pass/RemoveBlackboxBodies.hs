-- | Replace the bodies of black-box functions with blank results.
module Futhark.Pass.RemoveBlackBoxBodies (removeBlackBoxBodies) where

import Control.Monad ((<=<))
import Futhark.Builder
import Futhark.Construct
import Futhark.IR.SOACS
import Futhark.Pass

removeBlackBoxBodies :: Pass SOACS SOACS
removeBlackBoxBodies =
  Pass
    { passName = "remove black box bodies",
      passDescription =
        "Replace the bodies of black-box functions with blank results.",
      passFunction = \prog -> do
        funs <- mapM onFun $ progFuns prog
        pure prog {progFuns = funs}
    }
  where
    onFun fd
      | any isBlackBox $ unAttrs $ funDefAttrs fd = do
          body <- blankBody fd
          pure fd {funDefBody = body}
      | otherwise = pure fd

    isBlackBox (AttrComp "blackbox" _) = True
    isBlackBox _ = False

blankBody :: FunDef SOACS -> PassM (Body SOACS)
blankBody fd =
  case mapM (hasStaticShape . fromDecl . fst) $ funDefRetType fd of
    Nothing ->
      error $
        "remove-black-box-bodies: function "
          <> nameToString (funDefName fd)
          <> " has an existential return type, so its body cannot be removed."
    Just ts -> do
      (ses, stms) <-
        runBuilderT
          (mapM (letSubExp "blank" <=< eBlank) ts)
          (scopeOfFParams $ funDefParams fd)
      pure $ mkBody stms $ subExpsRes ses
