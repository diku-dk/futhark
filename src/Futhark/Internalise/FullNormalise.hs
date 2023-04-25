module Futhark.Internalise.FullNormalise (transformProg) where

import Control.Monad.State
import Data.Bifunctor
import Futhark.MonadFreshNames
import Language.Futhark

-- something to bind
data Binding
  = PatBind Pat Exp
  | FunBind -- tbd
  | WithBind -- tbd

-- state is the list of bindings, first to eval last
newtype OrderingM a = OrderingM (State ([Binding], VNameSource) a)
  deriving
    (Functor, Applicative, Monad)

instance MonadFreshNames OrderingM where
  getNameSource = OrderingM $ gets snd
  putNameSource = OrderingM . modify . second . const

instance MonadState [Binding] OrderingM where
  get = OrderingM $ gets fst
  put = OrderingM . modify . first . const

runOrdering :: MonadFreshNames m => OrderingM a -> m (a, [Binding])
runOrdering (OrderingM m) =
  modifyNameSource $ mod_tup . runState m . (mempty,)
  where
    mod_tup (a, (s, src)) = ((a, s), src)

getOrdering :: Exp -> OrderingM Exp
getOrdering = pure

transformBody :: MonadFreshNames m => Exp -> m Exp
transformBody e = do
  (e', pre_eval) <- runOrdering (getOrdering e)
  pure $ foldl f e' pre_eval
  where
    appRes = case e of
      (AppExp _ r) -> r
      _ -> Info $ AppRes (typeOf e) []
    f body (PatBind p expr) =
      AppExp
        ( LetPat
            []
            p
            expr
            body
            mempty
        )
        appRes
    f b FunBind = b -- tbd
    f b WithBind = b -- tbd

transformDec :: MonadFreshNames m => Dec -> m Dec
transformDec (ValDec valbind) = do
  body' <- transformBody $ valBindBody valbind
  pure $ ValDec (valbind {valBindBody = body'})
transformDec d = pure d

transformProg :: MonadFreshNames m => [Dec] -> m [Dec]
transformProg = mapM transformDec
