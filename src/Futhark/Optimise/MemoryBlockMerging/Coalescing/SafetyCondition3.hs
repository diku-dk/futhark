{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Safety condition 3 verification.
module Futhark.Optimise.MemoryBlockMerging.Coalescing.SafetyCondition3
  ( getVarUsesBetween
  ) where

import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous


data Context = Context
  { ctxSource :: VName
  , ctxDestination :: VName
  }
  deriving (Show)

data Current = Current
  { curHasReachedSource :: Bool
  , curHasReachedDestination :: Bool
  , curVars :: Names
  }
  deriving (Show)

newtype FindM a = FindM { unFindM :: RWS Context () Current a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadState Current)

modifyCurVars :: (Names -> Names) -> FindM ()
modifyCurVars f = modify $ \c -> c { curVars = f $ curVars c }

-- Find all the variables present between the creations of two variables (not
-- inclusive).
getVarUsesBetween :: FunDef ExplicitMemory
                  -> VName -> VName
                  -> Names
getVarUsesBetween fundef src dst =
  let context = Context src dst
      m = unFindM $ lookInBody $ funDefBody fundef
      res = curVars $ fst $ execRWS m context (Current False False S.empty)
  in res

lookInBody :: Body ExplicitMemory -> FindM ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStm bnds

lookInStm :: Stm ExplicitMemory -> FindM ()
lookInStm stm@(Let _ _ e) = do
  let new_decls = newDeclarationsStm stm

  dst <- asks ctxDestination
  when (dst `L.elem` new_decls)
    $ modify $ \c -> c { curHasReachedDestination = True }

  is_after_source <- gets curHasReachedSource
  is_before_destination <- gets curHasReachedDestination

  unless is_before_destination $ do
    let e_free_vars = freeInExp e -- FIXME: should maybe not include those consumed.
        e_used_vars = S.union e_free_vars (S.fromList new_decls)

    -- If the source has been created, add the newly used variables.
    --
    -- Note that "used after creation" refers both to used in subsequent
    -- statements AND any statements in any sub-bodies (if and loop).
    when is_after_source
      $ modifyCurVars $ S.union e_used_vars

    -- If the source is present in the declarations, state that it has been
    -- created.
    src <- asks ctxSource
    when (src `L.elem` new_decls)
      $ modify $ \c -> c { curHasReachedSource = True  }

    -- RECURSIVE BODY WALK.
    case e of
      If _ body0 body1 _ -> do
        -- This is not very If-specific, but rather specific to expressions with
        -- multiple, independent bodies, where If is just the only such
        -- expression.
        --
        -- We do not want the state (for safety condition 3) after traversing
        -- the first branch to be present when traversing the second branch,
        -- since they really will never both be run, so we compute them
        -- independently and then merge them at the end.
        before <- get
        lookInBody body0
        after0 <- get
        put Current { curHasReachedSource = curHasReachedSource before
                    , curHasReachedDestination = curHasReachedDestination after0
                    , curVars = curVars before
                    }
        lookInBody body1
        after1 <- get
        put Current { curHasReachedSource =
                      curHasReachedSource after0 || curHasReachedSource after1
                    , curHasReachedDestination =
                      curHasReachedDestination after0 || curHasReachedDestination after1
                    , curVars =
                      S.union (curVars after0) (curVars after1)
                    }
      _ -> do
        -- In the general case, just look through any 'Body' you can find.  (This
        -- is the case for loops.)
        let walker = identityWalker { walkOnBody = lookInBody
                                    }
        walkExpM walker e
