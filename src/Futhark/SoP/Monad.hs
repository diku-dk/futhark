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
    lookupUntransPE,
    lookupUntransSym,
    lookupRange,
    addRange,
    addProperty,
    askProperty,
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
    mkRange,
    findSymLEq0Def,
    mkRangeLB,
    mkRangeUB,
    askPropertyWith,
    RangeRelated (..),
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
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
    RangeRelated u,
    Show u, -- To be removed
    Pretty u, -- To be removed
    MonadFreshNames m,
    Expression e
  ) =>
  MonadSoP u e p m -- u is algebra symbols, e is "source" symbols.
    | m -> u,
      m -> e,
      m -> p
  where
  getUntrans :: m (UntransEnv u e)
  getRanges :: m (RangeEnv u)
  getEquivs :: m (EquivEnv u)
  getProperties :: m (Map u (Set p))
  modifyEnv :: (AlgEnv u e p -> AlgEnv u e p) -> m ()
  findSymLEq0 :: SoP u -> m (SoP u, Maybe (u, Range u))
  findSymLEq0 = findSymLEq0Def

-- | The algebraic monad; consists of a an algebraic
--   environment along with a fresh variable source.
newtype SoPMT u e p m a = SoPMT (StateT (AlgEnv u e p) m a)
  deriving
    ( Functor,
      Applicative,
      Monad
    )

instance MonadTrans (SoPMT u e p) where
  lift = SoPMT . lift

instance (MonadFreshNames m) => MonadFreshNames (SoPMT u e p m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance (MonadFreshNames m) => MonadFreshNames (StateT (AlgEnv u e p) m) where
  getNameSource = lift getNameSource
  putNameSource = lift . putNameSource

instance (MonadReader r m) => MonadReader r (SoPMT u e p m) where
  ask = SoPMT $ lift ask
  local f (SoPMT m) =
    SoPMT $ do
      env <- get
      (a, env') <- lift $ local f $ runStateT m env
      put env'
      pure a

instance (MonadState s m) => MonadState s (SoPMT u e p m) where
  get = SoPMT $ lift get
  put = SoPMT . lift . put

instance (MonadWriter w m) => MonadWriter w (SoPMT u e p m) where
  tell = SoPMT . lift . tell
  listen (SoPMT m) = SoPMT $ listen m
  pass (SoPMT m) = SoPMT $ pass m

type SoPM u e p = SoPMT u e p (State VNameSource)

runSoPMT :: AlgEnv u e p -> SoPMT u e p m a -> m (a, AlgEnv u e p)
runSoPMT env (SoPMT sm) = runStateT sm env

runSoPMT_ :: (Ord u, Ord e) => SoPMT u e p m a -> m (a, AlgEnv u e p)
runSoPMT_ = runSoPMT mempty

runSoPM :: AlgEnv u e p -> SoPM u e p a -> (a, AlgEnv u e p)
runSoPM env = flip evalState mempty . runSoPMT env

runSoPM_ :: (Ord u, Ord e) => SoPM u e p a -> (a, AlgEnv u e p)
runSoPM_ = runSoPM mempty

evalSoPMT :: (MonadFreshNames m) => AlgEnv u e p -> SoPMT u e p m a -> m a
evalSoPMT env m = fst <$> runSoPMT env m

evalSoPMT_ :: (Ord u, Ord e, MonadFreshNames m) => SoPMT u e p m a -> m a
evalSoPMT_ = evalSoPMT mempty

evalSoPM :: AlgEnv u e p -> SoPM u e p a -> a
evalSoPM env = fst . runSoPM env

evalSoPM_ :: (Ord u, Ord e) => SoPM u e p a -> a
evalSoPM_ = evalSoPM mempty

-- | Finds the next symbol to eliminate by choosing the
--     symbol with the most-dependent ranges. This is a
--     default implementation that can be extended according
--     to the particularities of the Symbol language.
--   The result is:
--     (equivalent-sop, Maybe(symbol-to-eliminate, its range))
findSymLEq0Def :: (MonadSoP u e p m) => SoP u -> m (SoP u, Maybe (u, Range u))
findSymLEq0Def sop = do
  rs <- getRanges
  let syms = S.toList $ free sop
      is = map (\s -> (length $ transitiveClosure rs (sym2SoP s), s)) syms
  case is of
    [] -> pure (sop, Nothing)
    _ -> do
      let i = snd $ maximum is
      rg <- lookupRange i
      pure (sop, Just (i, rg))

instance
  ( Ord u,
    Ord e,
    Nameable u,
    RangeRelated u,
    Show u,
    Pretty u,
    MonadFreshNames m,
    Expression e
  ) =>
  MonadSoP u e p (SoPMT u e p m)
  where
  getUntrans = SoPMT $ gets untrans

  getRanges = SoPMT $ gets ranges

  getEquivs = SoPMT $ gets equivs

  getProperties = SoPMT $ gets properties

  modifyEnv f = SoPMT $ modify f

-- \| Insert a symbol equal to an untranslatable 'PrimExp'.
addUntrans :: (MonadSoP u e p m) => u -> e -> m ()
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
lookupSoP :: (MonadSoP u e p m) => u -> m (Maybe (SoP u))
lookupSoP x = (M.!? x) <$> getEquivs

-- \| Look-up the symbol for a 'PrimExp'. If no symbol is bound
--    to the expression, bind a new one.
lookupUntransPE :: (MonadSoP u e p m) => e -> m u
lookupUntransPE pe = do
  inv_map <- inv <$> getUntrans
  case inv_map M.!? pe of
    Nothing -> do
      x <- mkNameM
      addUntrans x pe
      pure x
    Just x -> pure x

-- \| Look-up the untranslatable 'PrimExp' bound to the given symbol.
lookupUntransSym :: (MonadSoP u e p m) => u -> m (Maybe e)
lookupUntransSym sym = (M.!? sym) . dir <$> getUntrans

-- \| Look-up the range of a symbol. If no such range exists,
--    return the empty range.
lookupRange :: (MonadSoP u e p m) => u -> m (Range u)
lookupRange sym = do
  mr <- (M.!? sym) <$> getRanges
  case mr of
    Nothing -> do
      pure $ Range mempty 1 mempty
    Just r
      | rangeMult r <= 0 -> error "Non-positive constant encountered in range."
      | otherwise -> pure r

-- \| Add range information for a symbol; augments the existing
--   range.
addRange :: (MonadSoP u e p m) => u -> Range u -> m ()
addRange sym r =
  modifyEnv $ \env ->
    env {ranges = M.insertWith (<>) sym r (ranges env)}

mkRange :: Ord u => Maybe (SoP u) -> Maybe (SoP u) -> Range u
mkRange lb ub = Range (setFromMaybe lb) 1 (setFromMaybe ub)
  where
    setFromMaybe (Just x) = S.singleton x
    setFromMaybe Nothing = mempty

mkRangeLB :: (Ord u) => SoP u -> Range u
mkRangeLB n = Range (S.singleton n) 1 mempty

mkRangeUB :: (Ord u) => SoP u -> Range u
mkRangeUB n = Range mempty 1 (S.singleton n)

-- \| Add equivalent information for a symbol; unsafe and
-- should only be used for newly introduced variables.
addEquiv :: (MonadSoP u e p m) => u -> SoP u -> m ()
addEquiv sym sop = do
  -- sop' <- substEquivs sop
  modifyEnv $ \env ->
    env {equivs = M.insert sym sop (equivs env)}

addProperty :: (MonadSoP u e p m, Ord p) => u -> p -> m ()
addProperty sym prop = do
  modifyEnv $ \env ->
    env {properties = M.insertWith (<>) sym (S.singleton prop) (properties env)}

askProperty :: (MonadSoP u e p m, Ord p) => u -> p -> m Bool
askProperty sym prop = do
  mp <- (M.!? sym) <$> getProperties
  case mp of
    Nothing -> pure False
    Just props -> pure $ prop `S.member` props

askPropertyWith :: (MonadSoP u e p m) => u -> (Set p -> Maybe a) -> m (Maybe a)
askPropertyWith sym getter = do
  mp <- (M.!? sym) <$> getProperties
  case mp of
    Nothing -> pure Nothing
    Just props -> pure $ getter props

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
  pretty env = pretty (M.toList $ dir env)

-- | The equivalence environment binds a variable name to
--   its equivalent 'SoP' representation.
type EquivEnv u = Map u (SoP u)

instance (Pretty u) => Pretty (EquivEnv u) where
  pretty = stack . map (\(a, b) -> pretty a <> " ≡ " <> pretty b) . M.toList

-- | The range environment binds a variable name to a range.
type RangeEnv u = Map u (Range u)

instance (Pretty u) => Pretty (RangeEnv u) where
  pretty = stack . map pretty . M.toList

instance (Pretty u, Pretty p) => Pretty (Map u (Set p)) where
  pretty = pretty . M.toList

-- | The main algebraic environment.
data AlgEnv u e p = AlgEnv
  { -- | Binds untranslatable PrimExps to fresh symbols.
    untrans :: UntransEnv u e,
    -- | Binds symbols to their sum-of-product representation..
    equivs :: EquivEnv u,
    -- | Binds symbols to ranges (in sum-of-product form).
    ranges :: RangeEnv u,
    -- | Properties of symbols
    properties :: Map u (Set p)
  }
  deriving (Ord, Show, Eq)

instance (Ord u, Ord e) => Semigroup (AlgEnv u e p) where
  AlgEnv u1 s1 r1 p1 <> AlgEnv u2 s2 r2 p2 =
    AlgEnv (u1 <> u2) (s1 <> s2) (r1 <> r2) (p1 <> p2)

instance (Ord u, Ord e) => Monoid (AlgEnv u e p) where
  mempty = AlgEnv mempty mempty mempty mempty

instance (Pretty u, Pretty e, Pretty p) => Pretty (AlgEnv u e p) where
  pretty env =
    hang 4 $
      line
        <> "Untranslatable: "
        <> pretty (untrans env)
        <> line
        <> "Equivalences: "
        <> pretty (equivs env)
        <> line
        <> "Ranges: "
        <> pretty (ranges env)
        <> line
        <> "Properties: "
        <> pretty (M.toList $ properties env)

class (Ord u, Free u u) => RangeRelated u where
  freevar :: SoP u -> S.Set u

  rangeRelatedTo :: u -> S.Set u
  rangeRelatedTo = const S.empty

  isRangeRelatedTo :: u -> u -> Bool
  x `isRangeRelatedTo` y = y `S.member` rangeRelatedTo x

  -- | Get the set of symbols that `sym` is (transitively) related to
  -- in the range environment.
  -- You can redefine `rangeRelatedTo` to extend this set. For example,
  -- if the representation of u includes indexing statements, defining
  --   rangeRelatedTo (Idx x _) = x
  -- will include things bounded by `x` in the closure for `Idx x y`.
  transitiveClosure :: RangeEnv u -> SoP u -> Set u
  transitiveClosure ranges sop =
    tc ranges mempty mempty (free sop)
    where
      tc rs closure seen active
        | S.null active = closure
        | (sym, active') <- S.deleteFindMin active,
          closure' <- closure <> rangeRelatedTo sym,
          seen' <- S.insert sym seen =
            case M.lookup sym rs of
              Nothing ->
                let active'' = active' <> free sym
                 in tc rs closure' seen' active''
              Just range ->
                let new_syms = free range S.\\ seen
                    active'' = active' <> free sym <> new_syms
                 in tc rs closure' seen' active''

substEquivs :: (MonadSoP u e p m) => SoP u -> m (SoP u)
substEquivs sop = flip substitute sop <$> getEquivs

-- | Removes a symbol from the environment
delFromEnv :: (MonadSoP u e p m) => u -> m ()
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
