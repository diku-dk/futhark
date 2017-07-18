{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.Optimise.MemoryBlockMerging.PrimExps
  ( findPrimExpsFunDef
  ) where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Tools

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous


type CurrentTypes = M.Map VName PrimType
type PrimExps = M.Map VName (PrimExp VName)

newtype FindM a = FindM { unFindM :: RWS () PrimExps CurrentTypes a }
  deriving (Monad, Functor, Applicative,
            MonadWriter PrimExps,
            MonadState CurrentTypes)


findPrimExpsFunDef :: FunDef ExplicitMemory -> PrimExps
findPrimExpsFunDef fundef =
  let m = unFindM $
        -- We do not need to look in the function parameters.  That would likely
        -- find more mappings, but none of them would be able to be transformed
        -- to e.g. BinOps of other variables, so it would not be of much use to
        -- the modules that use this module.
        lookInBody $ funDefBody fundef
      res = snd $ evalRWS m () M.empty
  in res

lookInBody :: Body ExplicitMemory -> FindM ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStm bnds

lookInStm :: Stm ExplicitMemory -> FindM ()
lookInStm (Let (Pattern _patctxelems patvalelems) () e) = do
  prim_types <- get
  let varUse :: Exp ExplicitMemory -> Maybe (PrimExp VName)
      varUse (BasicOp (SubExp (Var v))) = do
        pt <- M.lookup v prim_types
        return $ ExpMem.LeafExp v pt
      varUse _ = Nothing

  case patvalelems of
    [PatElem dst _ _] ->
      onJust (primExpFromExp varUse e) $ tell . M.singleton dst
    _ -> return ()

  forM_ patvalelems $ \(PatElem var _ membound) ->
    case typeOf membound of
      Prim pt ->
        modify $ M.insert var pt
      _ -> return ()

  -- RECURSIVE BODY WALK.
  let walker = identityWalker { walkOnBody = lookInBody }
  walkExpM walker e
