{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
-- | This module provides a monadic facility similar (and built on top
-- of) "L0C.FreshNames".  The removes the need for a (small) amount of
-- boilerplate, at the cost of using some GHC extensions.  The idea is
-- that if your compiler pass runs in a monad that is an instance of
-- 'MonadFreshNames', you can automatically use the name generation
-- functions exported by this module.
module L0C.InternalRep.MonadFreshNames
  ( module L0C.MonadFreshNames
  , newIdent
  , newIdent'
  , newIdents
  , module L0C.FreshNames
  ) where

import Data.Loc

import Language.L0.Misc
import L0C.InternalRep
import L0C.MonadFreshNames
import L0C.FreshNames hiding (newName, newID, newVName)

-- | Produce a fresh 'Ident', using the given name as a template.
newIdent :: MonadFreshNames VName m =>
            String -> GenType als -> SrcLoc -> m (IdentBase (als VName))
newIdent s t loc = do
  s' <- newID $ varName s Nothing
  return $ Ident s' t loc

-- | Produce a fresh 'Ident', using the given 'Ident' as a template,
-- but possibly modifying the name.
newIdent' :: MonadFreshNames VName m =>
             (String -> String)
          -> IdentBase (als VName) -> m (IdentBase (als VName))
newIdent' f ident =
  newIdent (f $ nameToString $ baseName $ identName ident)
           (identType ident) $
           srclocOf ident

-- | Produce several 'Ident's, using the given name as a template,
-- based on a list of types.
newIdents :: MonadFreshNames VName m =>
             String -> [GenType als] -> SrcLoc -> m [IdentBase (als VName)]
newIdents s ts loc =
  mapM (\t -> newIdent s t loc) ts
