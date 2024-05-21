{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The Algebraic Environment, which is in principle
--   maintained during program traversal, is used to
--   solve symbolically algebraic inequations.
module Futhark.SoP.Monad
  ( Nameable (..),
    mkNameM,
    RangeEnv,
    EquivEnv,
    UntransEnv (..),
    AlgEnv (..),
    addUntrans,
    transClosInRanges,
    lookupUntransPE,
    lookupUntransSym,
    lookupRange,
    addRange,
    SoPMT,
    SoPM,
    lookupSoP,
    runSoPMT,
    runSoPMT_,
    runSoPM,
    runSoPM_,
    evalSoPMT,
    evalSoPMT_,
    evalSoPM,
    evalSoPM_,
    MonadSoP (..),
    substEquivs,
    addEquiv,
    delFromEnv,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Futhark.Analysis.PrimExp
import Futhark.FreshNames
import Futhark.MonadFreshNames
import Futhark.SoP.Expression
import Futhark.SoP.SoP
import Futhark.Util.Pretty
import Language.Futhark.Syntax hiding (Range)

--------------------------------------------------------------------------------
-- Names; probably will remove in the end.
--------------------------------------------------------------------------------

-- | Types which can use a fresh source to generate
--   unique names.
class Nameable u where
  mkName :: VNameSource -> (u, VNameSource)

instance Nameable String where
  mkName (VNameSource i) = ("x" <> show i, VNameSource $ i + 1)

instance Nameable VName where
  mkName (VNameSource i) = (VName "x" i, VNameSource $ i + 1)

instance Nameable Name where
  mkName = mkName

mkNameM :: (Nameable u, MonadFreshNames m) => m u
mkNameM = modifyNameSource mkName

--------------------------------------------------------------------------------
-- Monad
--------------------------------------------------------------------------------

class
  ( Ord u,
    Ord e,
    Nameable u,
    Show u, -- To be removed
    Pretty u, -- To be removed
    MonadFreshNames m,
    Expression e
  ) =>
  MonadSoP u e m
    | m -> u,
      m -> e
  where
  getUntrans :: m (UntransEnv u e)
  getRanges :: m (RangeEnv u)
  getEquivs :: m (EquivEnv u)
  modifyEnv :: (AlgEnv u e -> AlgEnv u e) -> m ()

-- | The algebraic monad; consists of a an algebraic
--   environment along with a fresh variable source.
newtype SoPMT u e m a = SoPMT (StateT (AlgEnv u e) m a)
  deriving
    ( Functor,
      Applicative,
      Monad
    )

instance MonadTrans (SoPMT u e) where
  lift = SoPMT . lift

instance (MonadFreshNames m) => MonadFreshNames (SoPMT u e m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance (MonadFreshNames m) => MonadFreshNames (StateT (AlgEnv u e) m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance (MonadReader r m) => MonadReader r (SoPMT u e m) where
  ask = SoPMT $ lift ask
  local f (SoPMT m) =
    SoPMT $ do
      env <- get
      (a, env') <- lift $ local f $ runStateT m env
      put env'
      pure a

instance (MonadState s m) => MonadState s (SoPMT u e m) where
  get = SoPMT $ lift get
  put = SoPMT . lift . put

instance (MonadWriter w m) => MonadWriter w (SoPMT u e m) where
  tell = SoPMT . lift . tell
  listen (SoPMT m) = SoPMT $ listen m
  pass (SoPMT m) = SoPMT $ pass m

type SoPM u e = SoPMT u e (State VNameSource)

runSoPMT :: (MonadFreshNames m) => AlgEnv u e -> SoPMT u e m a -> m (a, AlgEnv u e)
runSoPMT env (SoPMT sm) = runStateT sm env

runSoPMT_ :: (Ord u, Ord e, MonadFreshNames m) => SoPMT u e m a -> m (a, AlgEnv u e)
runSoPMT_ = runSoPMT mempty

runSoPM :: (Ord u, Ord e) => AlgEnv u e -> SoPM u e a -> (a, AlgEnv u e)
runSoPM env = flip evalState mempty . runSoPMT env

runSoPM_ :: (Ord u, Ord e) => SoPM u e a -> (a, AlgEnv u e)
runSoPM_ = runSoPM mempty

evalSoPMT :: (MonadFreshNames m) => AlgEnv u e -> SoPMT u e m a -> m a
evalSoPMT env m = fst <$> runSoPMT env m

evalSoPMT_ :: (Ord u, Ord e, MonadFreshNames m) => SoPMT u e m a -> m a
evalSoPMT_ = evalSoPMT mempty

evalSoPM :: (Ord u, Ord e) => AlgEnv u e -> SoPM u e a -> a
evalSoPM env = fst . runSoPM env

evalSoPM_ :: (Ord u, Ord e) => SoPM u e a -> a
evalSoPM_ = evalSoPM mempty

instance
  ( Ord u,
    Ord e,
    Nameable u,
    Show u,
    Pretty u,
    MonadFreshNames m,
    Expression e
  ) =>
  MonadSoP u e (SoPMT u e m)
  where
  getUntrans = SoPMT $ gets untrans

  getRanges = SoPMT $ gets ranges

  getEquivs = SoPMT $ gets equivs

  modifyEnv f = SoPMT $ modify f

-- \| Insert a symbol equal to an untranslatable 'PrimExp'.
addUntrans :: (MonadSoP u e m) => u -> e -> m ()
addUntrans sym pe =
  modifyEnv $ \env ->
    env
      { untrans =
          (untrans env)
            { dir = M.insert sym pe (dir (untrans env)),
              inv = M.insert pe sym (inv (untrans env))
            }
      }

-- \| Look-up the sum-of-products representation of a symbol.
lookupSoP :: (MonadSoP u e m) => u -> m (Maybe (SoP u))
lookupSoP x = (M.!? x) <$> getEquivs

-- \| Look-up the symbol for a 'PrimExp'. If no symbol is bound
--    to the expression, bind a new one.
lookupUntransPE :: (MonadSoP u e m) => e -> m u
lookupUntransPE pe = do
  inv_map <- inv <$> getUntrans
  case inv_map M.!? pe of
    Nothing -> do
      x <- mkNameM
      addUntrans x pe
      pure x
    Just x -> pure x

-- \| Look-up the untranslatable 'PrimExp' bound to the given symbol.
lookupUntransSym :: (MonadSoP u e m) => u -> m (Maybe e)
lookupUntransSym sym = ((M.!? sym) . dir) <$> getUntrans

-- \| Look-up the range of a symbol. If no such range exists,
--    return the empty range (and add it to the environment).
lookupRange :: (MonadSoP u e m) => u -> m (Range u)
lookupRange sym = do
  mr <- (M.!? sym) <$> getRanges
  case mr of
    Nothing -> do
      let r = Range mempty 1 mempty
      addRange sym r
      pure r
    Just r
      | rangeMult r <= 0 -> error "Non-positive constant encountered in range."
      | otherwise -> pure r

-- \| Add range information for a symbol; augments the existing
--   range.
addRange :: (MonadSoP u e m) => u -> Range u -> m ()
addRange sym r =
  modifyEnv $ \env ->
    env {ranges = M.insertWith (<>) sym r (ranges env)}

-- \| Add equivalent information for a symbol; unsafe and
-- should only be used for newly introduced variables.
addEquiv :: (MonadSoP u e m) => u -> SoP u -> m ()
addEquiv sym sop = do
  -- sop' <- substEquivs sop
  modifyEnv $ \env ->
    env {equivs = M.insert sym sop (equivs env)}

--------------------------------------------------------------------------------
-- Environment
--------------------------------------------------------------------------------

-- | The environment of untranslatable 'PrimeExp's.  It maps both
--   ways:
--
--   1. A fresh symbol is generated and mapped to the
--      corresponding 'PrimeExp' @pe@ in 'dir'.
--   2. The target @pe@ is mapped backed to the corresponding symbol in 'inv'.
data UntransEnv u e = Unknowns
  { dir :: Map u e,
    inv :: Map e u
  }
  deriving (Eq, Show, Ord)

instance (Ord u, Ord e) => Semigroup (UntransEnv u e) where
  Unknowns d1 i1 <> Unknowns d2 i2 = Unknowns (d1 <> d2) (i1 <> i2)

instance (Ord u, Ord e) => Monoid (UntransEnv u e) where
  mempty = Unknowns mempty mempty

instance (Pretty u, Pretty e) => Pretty (UntransEnv u e) where
  pretty env =
    "dir:"
      <> line
      <> pretty (M.toList $ dir env)
      <> line
      <> "inv:"
      <> line
      <> pretty (M.toList $ inv env)

-- | The equivalence environment binds a variable name to
--   its equivalent 'SoP' representation.
type EquivEnv u = Map u (SoP u)

-- | The range environment binds a variable name to a range.
type RangeEnv u = Map u (Range u)

type PropEnv u = Map u Properties

data Properties = Monotonic
  deriving (Show, Eq, Ord)

instance (Pretty u) => Pretty (RangeEnv u) where
  pretty = pretty . M.toList

-- | The main algebraic environment.
data AlgEnv u e = AlgEnv
  { -- | Binds untranslatable PrimExps to fresh symbols.
    untrans :: UntransEnv u e,
    -- | Binds symbols to their sum-of-product representation..
    equivs :: EquivEnv u,
    -- | Binds symbols to ranges (in sum-of-product form).
    ranges :: RangeEnv u,
    -- | Symbol table to store properties.
    props :: PropEnv u
  }
  deriving (Ord, Show, Eq)

instance (Ord u, Ord e) => Semigroup (AlgEnv u e) where
  AlgEnv u1 s1 r1 p1 <> AlgEnv u2 s2 r2 p2 =
    AlgEnv (u1 <> u2) (s1 <> s2) (r1 <> r2) (p1 <> p2)

instance (Ord u, Ord e) => Monoid (AlgEnv u e) where
  mempty = AlgEnv mempty mempty mempty mempty

instance (Pretty u, Pretty e) => Pretty (AlgEnv u e) where
  pretty (env) =
    "Untranslatable environment:"
      <> line
      <> pretty (untrans env)
      <> line
      <> "Equivalence environment:"
      <> line
      <> pretty (M.toList $ equivs env)
      <> line
      <> "Ranges:"
      <> line
      <> pretty (M.toList $ ranges env)

transClosInRanges :: (Ord u) => RangeEnv u -> Set u -> Set u
transClosInRanges rs syms =
  transClosHelper rs syms S.empty syms
  where
    transClosHelper rs' clos_syms seen active
      | S.null active = clos_syms
      | (sym, active') <- S.deleteFindMin active,
        seen' <- S.insert sym seen =
          case M.lookup sym rs' of
            Nothing ->
              transClosHelper rs' clos_syms seen' active'
            Just range ->
              let new_syms = free range S.\\ seen
                  clos_syms' = S.union clos_syms new_syms
                  active'' = S.union new_syms active'
               in transClosHelper rs' clos_syms' seen' active''

substEquivs :: (MonadSoP u e m) => SoP u -> m (SoP u)
substEquivs sop = flip substitute sop <$> getEquivs

-- | Removes a symbol from the environment
delFromEnv :: (MonadSoP u e m) => u -> m ()
delFromEnv x =
  modifyEnv $ \env ->
    env
      { untrans = delFromUntrans $ untrans env,
        equivs = M.delete x $ equivs env,
        ranges = M.delete x $ ranges env
      }
  where
    delFromUntrans ut =
      ut
        { dir = M.delete x $ dir ut,
          inv = M.filter (/= x) $ inv ut
        }
