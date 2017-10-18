{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Get a mapping from statement patterns to statement expression for all
-- statements.
module Futhark.Optimise.MemoryBlockMerging.Coalescing.Exps
  ( Exp'(..)
  , findExpsFunDef
  ) where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Writer

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemorish)
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous

-- | Describes the nth pattern and the statement expression.
data Exp' = forall lore. Annotations lore => Exp Int (Exp lore)
instance Show Exp' where
  show (Exp _npattern e) = show e

type Exps = M.Map VName Exp'

newtype FindM lore a = FindM { unFindM :: Writer Exps a }
  deriving (Monad, Functor, Applicative,
            MonadWriter Exps)

type LoreConstraints lore = (ExplicitMemorish lore,
                             FullWalk lore)

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

findExpsFunDef :: LoreConstraints lore =>
                  FunDef lore -> Exps
findExpsFunDef fundef =
  let m = unFindM $ lookInBody $ funDefBody fundef
      res = execWriter m
  in res

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
lookInStm (Let (Pattern _patctxelems patvalelems) _ e) = do
  forM_ (zip patvalelems [0..]) $ \(PatElem var _ _, i) ->
    tell $ M.singleton var $ Exp i e

  -- Recursive body walk.
  fullWalkExpM walker walker_kernel e
  where walker = identityWalker
          { walkOnBody = lookInBody }
        walker_kernel = identityKernelWalker
          { walkOnKernelBody = coerce . lookInBody
          , walkOnKernelKernelBody = coerce . lookInKernelBody
          }
