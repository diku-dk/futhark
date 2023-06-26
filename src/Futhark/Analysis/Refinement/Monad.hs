module Futhark.Analysis.Refinement.Monad where

import Control.Applicative
import Control.Monad.RWS
import Data.Foldable (toList)
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as M
import Futhark.Analysis.Refinement.CNF
import Futhark.Analysis.Refinement.Latex
import Futhark.Analysis.Refinement.Representation
import Futhark.MonadFreshNames
import Futhark.SoP.Monad
import Futhark.SoP.SoP
import Futhark.SoP.Util
import Futhark.Util.Pretty
import Language.Futhark qualified as E
import Language.Futhark.Prop qualified as E
import Language.Futhark.Semantic

data SEnv = SEnv
  { vnamesource :: VNameSource,
    algenv :: AlgEnv Exp E.Exp,
    exps :: Map E.VName E.Exp,
    types :: Map E.VName E.PatType,
    known :: [Prop],
    known_map :: Map E.VName [Prop]
  }
  deriving (Eq)

-- addExps :: SEnv -> Map E.VName E.Exp -> SEnv
-- addExps senv es = senv {exps = exps senv <> es}

-- addKnown :: Monad m => [Prop] -> RefineT m ()
-- addKnown ps =
--  modify $ \senv -> senv {known = known senv ++ ps}

-- data Log
--  = Fail String
--  | Success String
--  | Message String
type Log = LaTeX

type RefineM = RefineT Identity

type CNFM = RefineT CNF

newtype RefineT m a = RefineT {getRWST :: RWST () [Log] SEnv m a}

deriving instance (Monad m) => Functor (RefineT m)

deriving instance (Monad m) => Applicative (RefineT m)

deriving instance (Monad m) => Monad (RefineT m)

deriving instance (Monad m) => MonadState SEnv (RefineT m)

deriving instance (Monad m) => MonadFreshNames (RefineT m)

deriving instance (Monad m) => MonadWriter [Log] (RefineT m)

deriving instance (Monad m, MonadPlus m) => MonadPlus (RefineT m)

deriving instance (Monad m, MonadPlus m, Alternative m) => Alternative (RefineT m)

instance (Monad m, Monoid w) => MonadFreshNames (RWST r w SEnv m) where
  getNameSource = gets vnamesource
  putNameSource vns = modify $ \senv -> senv {vnamesource = vns}

instance (Monad m) => MonadSoP Exp E.Exp (RefineT m) where
  getUntrans = gets (untrans . algenv)
  getRanges = gets (ranges . algenv)
  getEquivs = gets (equivs . algenv)
  modifyEnv f = modify $ \env -> env {algenv = f $ algenv env}

execRefineT :: (Monad m) => RefineT m a -> VNameSource -> m [Log]
execRefineT (RefineT m) vns = snd <$> execRWST m mempty (SEnv vns mempty mempty mempty mempty mempty)

execRefineM :: RefineM a -> VNameSource -> [Log]
execRefineM m = runIdentity . execRefineT m

fromCNFM :: CNFM a -> RefineM (CNF (a, SEnv, [Log]))
fromCNFM (RefineT (RWST m)) =
  RefineT $
    RWST $
      \r -> \s -> pure (m r s, s, mempty) -- fix

toCNFM :: RefineM (CNF (a, SEnv, [Log])) -> CNFM a
toCNFM (RefineT (RWST m)) =
  RefineT $
    RWST $
      \r s -> (\(x, _, _) -> x) $ runIdentity $ m r s

bindCNFM :: (a -> CNFM b) -> CNF (a, SEnv, [Log]) -> RefineM (CNF (b, SEnv, [Log]))
bindCNFM f cnf = fromCNFM $ m >>= f
  where
    m = RefineT $ RWST $ \_ _ -> cnf

asCNFM :: (CNFM a -> CNFM b) -> RefineM (CNF (a, SEnv, [Log])) -> RefineM (CNF (b, SEnv, [Log]))
asCNFM f = fromCNFM . f . toCNFM

withCNF :: (a -> Bool) -> CNFM a -> RefineM Bool
withCNF check (RefineT (RWST m)) =
  RefineT $
    RWST $ \r s ->
      let result = cnfIsValid (check . (\(b, _, _) -> b)) $ m r s
          log = foldMap (\(_, _, w) -> w) $ toList $ m r s
       in pure (result, s, log)

addGoals :: (Pretty a) => [CNFM a] -> CNFM a
addGoals ms =
  RefineT $
    RWST $ \r -> \s ->
      foldr (&&&) cnfTrue (map (\m -> runRWST (getRWST m) r s) ms)

lookupVName :: (Monad m) => E.VName -> RefineT m (Maybe E.Exp)
lookupVName x =
  (M.!? x) <$> gets exps

lookupType :: (Monad m) => E.VName -> RefineT m (Maybe E.PatType)
lookupType x =
  (M.!? x) <$> gets types

insertExp :: (Monad m) => E.VName -> E.Exp -> RefineT m ()
insertExp x e =
  modify $ \env -> env {exps = M.insert x e $ exps env}

insertType :: (Monad m) => E.VName -> E.PatType -> RefineT m ()
insertType x t =
  modify $ \env -> env {types = M.insert x t $ types env}
