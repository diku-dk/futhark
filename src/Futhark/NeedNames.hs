-- | Simple monad for computations that need access to unique names.
-- In particular, this monad permits a degree of speculation, by which
-- the computation can be run and then inspected as to whether it
-- needs names at all.  A state monad containing a 'NameSource' does
-- not have this property.
module Futhark.NeedNames
  ( NeedNames
  , provideNames
  , needsNoNames
  )
  where

import Control.Applicative
import Futhark.Representation.Basic
import Futhark.MonadFreshNames

-- | The central monad itself.
data NeedNames a = NeedName (NameSource VName -> (NeedNames a, NameSource VName))
                 | Done     a

instance Monad NeedNames where
  return = Done
  Done m >>= f = f m
  NeedName g >>= f = NeedName $ \src -> let (m, src') = g src
                                        in (f =<< m, src')

instance Functor NeedNames where
  f `fmap` x = do x' <- x
                  return $ f x'

instance Applicative NeedNames where
  pure = Done
  f <*> x = do f' <- f
               x' <- x
               return $ f' x'

instance MonadFreshNames NeedNames where
  getNameSource     = NeedName $ \src -> (return src, src)
  putNameSource src = NeedName $ \_   -> (return (), src)

-- | Provide whichever names are needed, then return the result.
provideNames :: MonadFreshNames m => NeedNames a -> m a
provideNames (Done x)       = return x
provideNames (NeedName f) =
  modifyNameSource f >>= provideNames

-- | If the computation can produce a result without needing names,
-- return that result.
needsNoNames :: NeedNames a -> Maybe a
needsNoNames (Done x)     = Just x
needsNoNames (NeedName _) = Nothing
