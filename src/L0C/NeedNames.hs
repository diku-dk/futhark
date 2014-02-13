{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
-- | Simple monad for computations that need access to unique names.
-- In particular, this monad permits a degree of speculation, by which
-- the computation can be run and then inspected as to whether it
-- needs names at all.  A state monad containing a 'NameSource' does
-- not have this property.
module L0C.NeedNames
  ( NeedNames
  , provideNames
  , needsNoNames
  )
  where

import Control.Applicative
import L0C.InternalRep
import L0C.MonadFreshNames

-- | The central monad itself.
data NeedNames a = NeedName (NameSource VName -> (NameSource VName, NeedNames a))
                 | Done     a

instance Monad NeedNames where
  return = Done
  Done m >>= f = f m
  NeedName g >>= f = NeedName $ \src -> let (src', m) = g src
                                        in (src', f =<< m)

instance Functor NeedNames where
  f `fmap` x = do x' <- x
                  return $ f x'

instance Applicative NeedNames where
  pure = Done
  f <*> x = do f' <- f
               x' <- x
               return $ f' x'

instance MonadFreshNames (ID Name) NeedNames where
  getNameSource     = NeedName $ \src -> (src, return src)
  putNameSource src = NeedName $ \_   -> (src, return ())

-- | Provide whichever names are needed, then return the result.
provideNames :: MonadFreshNames VName m => NeedNames a -> m a
provideNames (Done x)       = return x
provideNames (NeedName f) = do
  src <- getNameSource
  let (src', m) = f src
  putNameSource src'
  provideNames m

-- | If the computation can produce a result without needing names,
-- return that result.
needsNoNames :: NeedNames a -> Maybe a
needsNoNames (Done x)     = Just x
needsNoNames (NeedName _) = Nothing
