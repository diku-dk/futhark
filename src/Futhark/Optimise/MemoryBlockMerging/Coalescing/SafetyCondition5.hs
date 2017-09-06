{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find safety condition 5 for all statements.
module Futhark.Optimise.MemoryBlockMerging.Coalescing.SafetyCondition5
  ( findSafetyCondition5FunDef
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (
  InKernel, ExplicitMemory, ExplicitMemorish)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.Miscellaneous


type DeclarationsSoFar = Names
type VarsInUseBeforeMem = M.Map MName Names

newtype FindM lore a = FindM { unFindM :: RWS FirstUses
                               VarsInUseBeforeMem DeclarationsSoFar a }
  deriving (Monad, Functor, Applicative,
            MonadReader FirstUses,
            MonadWriter VarsInUseBeforeMem,
            MonadState DeclarationsSoFar)

type LoreConstraints lore = (ExplicitMemorish lore,
                             ExtractKernelDefVars lore,
                             FullWalk lore)

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

findSafetyCondition5FunDef :: FunDef ExplicitMemory -> FirstUses
                           -> VarsInUseBeforeMem
findSafetyCondition5FunDef fundef first_uses =
  let m = unFindM $ do
        forM_ (funDefParams fundef) lookInFParam
        lookInBody $ funDefBody fundef
      res = snd $ evalRWS m first_uses S.empty
  in res

lookInFParam :: LoreConstraints lore =>
                FParam lore -> FindM lore ()
lookInFParam (Param x _) =
  modify $ S.insert x

lookInLParam :: LoreConstraints lore =>
                LParam lore -> FindM lore ()
lookInLParam (Param x _) =
  modify $ S.insert x

lookInBody :: LoreConstraints lore =>
              Body lore -> FindM lore ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStm bnds

lookInKernelBody :: LoreConstraints lore =>
                    KernelBody lore -> FindM lore ()
lookInKernelBody (KernelBody _ bnds _res) =
  mapM_ lookInStm bnds

lookInStm :: LoreConstraints lore =>
             Stm lore -> FindM lore ()
lookInStm stm@(Let _ _ e) = do
  let new_decls = newDeclarationsStm stm

  first_uses <- ask
  declarations_so_far <- get
  forM_ (S.toList $ S.unions $ map (`lookupEmptyable` first_uses) new_decls) $ \mem ->
    tell $ M.singleton mem declarations_so_far

  forM_ new_decls $ \x ->
    modify $ S.insert x

  -- Special loop handling: Extract useful variables that are in use.
  case e of
    DoLoop _ _ loopform _ ->
      case loopform of
        ForLoop i _ _ _ -> modify $ S.insert i
        WhileLoop c -> modify $ S.insert c
    _ -> return ()

  modify $ S.union (extractKernelDefVars e)

  -- RECURSIVE BODY WALK.
  fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody
          , walkOnFParam = lookInFParam
          , walkOnLParam = lookInLParam
          }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          , walkOnKernelLambda = coerce . lookInLambda
          , walkOnKernelLParam = lookInLParam
          }

lookInLambda :: LoreConstraints lore =>
                Lambda lore -> FindM lore ()
lookInLambda (Lambda params body _) = do
  forM_ params lookInLParam
  lookInBody body

class ExtractKernelDefVars lore where
  extractKernelDefVars :: Exp lore -> Names

instance ExtractKernelDefVars ExplicitMemory where
  extractKernelDefVars (Op (ExpMem.Inner (Kernel _ _ kernelspace _ _))) =
    S.fromList $ map ($ kernelspace)
    [spaceGlobalId, spaceLocalId, spaceGroupId]
  extractKernelDefVars _ = S.empty

instance ExtractKernelDefVars InKernel where
  extractKernelDefVars _ = S.empty
