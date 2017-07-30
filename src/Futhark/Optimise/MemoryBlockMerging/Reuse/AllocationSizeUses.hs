{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Find out where allocation sizes are used.
module Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizeUses
  ( findSizeUsesFunDef
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizes


type SizeVars = Names
type DeclarationsSoFar = Names

-- The final return value.  Describes which size variables are in scope at the
-- creation of the key size variable.
type UsesBefore = M.Map VName Names

newtype FindM a = FindM { unFindM :: RWS SizeVars
                          UsesBefore DeclarationsSoFar a }
  deriving (Monad, Functor, Applicative,
            MonadReader SizeVars,
            MonadWriter UsesBefore,
            MonadState DeclarationsSoFar)

findSizeUsesFunDef :: FunDef ExplicitMemory -> UsesBefore
findSizeUsesFunDef fundef  =
  let size_vars = mapMaybe fromVar $ M.elems $ memBlockSizesFunDef fundef
      m = unFindM $ do
        forM_ (funDefParams fundef) lookInFParam
        lookInBody $ funDefBody fundef
      res = snd $ evalRWS m (S.fromList size_vars) S.empty
  in res

lookInFParam :: FParam ExplicitMemory -> FindM ()
lookInFParam (Param x _) =
  lookAtNewDecls $ S.singleton x

lookInBody :: Body ExplicitMemory -> FindM ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStm bnds

lookInStm :: Stm ExplicitMemory -> FindM ()
lookInStm stm@(Let _ _ e) = do
  let new_decls = S.fromList $ newDeclarationsStm stm
  lookAtNewDecls new_decls

  -- RECURSIVE BODY WALK.
  walkExpM walker e
  where walker = identityWalker
          { walkOnBody = lookInBody
          , walkOnFParam = lookInFParam
          }

lookAtNewDecls :: Names -> FindM ()
lookAtNewDecls new_decls = do
  all_size_vars <- ask
  declarations_so_far <- get
  let new_size_vars = S.intersection all_size_vars new_decls
  forM_ new_size_vars $ \var ->
    tell $ M.singleton var declarations_so_far
  modify $ S.union new_size_vars
