{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
-- | Find all variables in a statement.
module Futhark.Optimise.MemoryBlockMerging.AllExpVars
  ( findAllExpVars
  ) where

import qualified Data.Set as S
import Control.Monad
import Control.Monad.Writer

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemorish)
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous


newtype FindM lore a = FindM { unFindM :: Writer Names a }
  deriving (Monad, Functor, Applicative,
            MonadWriter Names)

type LoreConstraints lore = (ExplicitMemorish lore,
                             FullWalk lore)

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

-- Find all the variables (both free and bound) that occur in a statement and
-- any nested bodies.  We use this to record which extra variables need to have
-- their memory blocks updated when some variable needs updating.  The result
-- might be an empty set, but in the case of If, DoLoop, and kernels, the result
-- might be nonempty.  We cannot just find all variables in the program and look
-- through them every time we need to, since a memory block can (at least in
-- theory) be present in two different places (which also means by two different
-- variable sets) in a program, so we should limit ourselves to looking in the
-- statement declaring a new current use of the memory.
findAllExpVars :: LoreConstraints lore =>
                  Exp lore -> Names
findAllExpVars e =
  let m = unFindM $ lookInExp e
  in execWriter m

lookInExp :: LoreConstraints lore =>
             Exp lore -> FindM lore ()
lookInExp = fullWalkExpM walker walker_kernel
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

lookInFParam :: LoreConstraints lore =>
                FParam lore -> FindM lore ()
lookInFParam (Param x _) =
  tell $ S.singleton x

lookInLParam :: LoreConstraints lore =>
                LParam lore -> FindM lore ()
lookInLParam (Param x _) =
  tell $ S.singleton x

lookInBody :: LoreConstraints lore =>
              Body lore -> FindM lore ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStm bnds

lookInKernelBody :: LoreConstraints lore =>
                    KernelBody lore -> FindM lore ()
lookInKernelBody (KernelBody _ bnds res) = do
  mapM_ lookInStm bnds
  forM_ res $ \case
    ThreadsReturn{} -> return ()
    WriteReturn _ arr _ -> tell $ S.singleton arr
    ConcatReturns{} -> return ()
    KernelInPlaceReturn v -> tell $ S.singleton v

lookInStm :: LoreConstraints lore =>
             Stm lore -> FindM lore ()
lookInStm (Let (Pattern _ patvalelems) _ e) = do
  forM_ patvalelems $ \(PatElem x _) ->
    tell $ S.singleton x
  lookInExp e

lookInLambda :: LoreConstraints lore =>
                Lambda lore -> FindM lore ()
lookInLambda (Lambda params body _) = do
  forM_ params lookInLParam
  lookInBody body
