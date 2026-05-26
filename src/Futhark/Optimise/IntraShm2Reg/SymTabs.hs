module Futhark.Optimise.IntraShm2Reg.SymTabs
  ( Shm2RegM(..),
    runShm2RegM,
    Shm2RegEnv(..),
    BotEnv(..),
    RegEntry(..),
    AccessKind(..),
    unifyAccK,
    freshBotEnv,
    initEntry1,
    initEntry2,
    TopEnv(..),
    freshTopEnv,
    addIota2Env,
    addBinOp2Env,
    peFromSe,
    removeTpConv,
    updateTopdownEnv,
    Env,
  ) where

import Control.Monad.Reader
import Control.Monad.State hiding (state)
--import Control.Monad (forM)
import Data.Map.Strict qualified as M
--import Data.Set qualified as S
import Data.Maybe
import Futhark.IR.GPU
import Futhark.Tools
import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..))
import Futhark.Util.Pretty

-------------------------
--- Monads for SoP
-------------------------

data Shm2RegEnv = Shm2RegEnv
  { vNameSource :: VNameSource,
    algEnv :: AlgEnv VName (PrimExp VName)
  }

newtype Shm2RegM a = Shm2RegM (ReaderT (Scope GPU) (State Shm2RegEnv) a)
  deriving
    ( Monad,
      Applicative,
      Functor,
      MonadState Shm2RegEnv,
      HasScope GPU,
      LocalScope GPU
    )

instance MonadFreshNames Shm2RegM where
  getNameSource = gets vNameSource
  putNameSource source = modify (\env -> env {vNameSource = source})

instance MonadSoP VName (PrimExp VName) Shm2RegM where
  getUntrans = gets (untrans . algEnv)
  getRanges = gets (ranges . algEnv)
  getEquivs = gets (equivs . algEnv)
  modifyEnv f = modify $ \env -> env {algEnv = f $ algEnv env}

runShm2RegM :: (MonadFreshNames m) => Scope GPU -> Shm2RegEnv -> Shm2RegM a -> m a
runShm2RegM scope env (Shm2RegM a) = modifyNameSource $ \src ->
  let state = runReaderT a scope
      (x, newEnv) = runState state (env {vNameSource = src})
   in (x, vNameSource newEnv)

-------------------------
--- Bottom-Up Context ---
-------------------------

data AccessKind = None | Irreg | Compat
  deriving Eq

instance Pretty AccessKind where
  pretty None  = "None"
  pretty Irreg = "Irreg"
  pretty Compat= "Compat"

-- | conservatively unifies access kinds
unifyAccK :: AccessKind -> AccessKind -> AccessKind
unifyAccK None kind = kind
unifyAccK kind None = kind
unifyAccK Irreg _   = Irreg
unifyAccK _ Irreg   = Irreg
unifyAccK _ _ = Compat      


data RegEntry = RegEntry
  {
    shpdims :: [(SubExp, PrimExp VName)],
    -- ^ the shape of the array as SubExps and PrimExps as well.
    pardims :: [PrimExp VName],
    -- ^ the parallel dimensions of a successful inner kernel;
    --   these should match the outer dimensions of @shape@
    bindings :: [(VName, AccessKind)]
    -- ^ the access kind for each statement denoted by
    --   its first bound name
  }

instance Pretty RegEntry where
  pretty reg_entry =
    "\nRegEntry{\n\tShpDims: " <+> pretty (shpdims  reg_entry) <>
     "\n\tParDims: " <+> pretty (pardims  reg_entry) <>
     "\n\tBindings: "<+> pretty (bindings reg_entry) <>
     "   }"


initEntry1 :: [(SubExp, PrimExp VName)] -> RegEntry
initEntry1 shpdims = RegEntry shpdims [] mempty

initEntry2 :: [(SubExp, PrimExp VName)] ->
              [PrimExp VName] ->
              RegEntry
initEntry2 shp pardims = RegEntry shp pardims mempty

data BotEnv = BotEnv
  {
    parSpace :: [(VName, SubExp, PrimExp VName)],
    -- ^ The parallel space of the block intrakernel
    freeVars :: Names,
    -- ^ the free variables in the current intrakernel
    regArrays :: M.Map VName RegEntry
    -- ^ each array (name) target to being allocated
    --   in register memory is bound to an entry
--    indices :: M.Map VName (Slice (PrimExp VName)) -- (Slice SubExp)
--  , optstms :: Stms GPU
  }

instance Pretty BotEnv where
  pretty benv =
    "BotEnv {\n\tParSpace: " <+> pretty (parSpace benv) <>
     "\n\tFreeVars: " <+> pretty (freeVars benv) <>
     "\n\tregArrays: "<+> (foldl (<>) ("") (map printBnd (M.toList (regArrays benv)))) <>
     "   }"
    where
      printBnd (nm, etry) = ("\n\t" <+> pretty nm) <> pretty etry

freshBotEnv :: [(VName, SubExp, PrimExp VName)] -> Names -> BotEnv
freshBotEnv sp fvs = BotEnv sp fvs mempty

---------------------------
--- TopDown Context
---------------------------

data TopEnv = TopEnv
  { bdy_res     :: [SubExp]
  , scalars     :: M.Map VName (PrimExp VName)
    -- ^ binds the name of a scalar var to its expression
  , iotas       :: M.Map VName (SubExp, PrimExp VName)
    -- ^ binds the name of an iota array to its size
  , inv_iotas   :: M.Map (PrimExp VName) (VName, SubExp)
    -- ^ binds a size pexp to the name of a iota and its SubExp size
  , arrTransf   :: M.Map VName (VName, [Int])
    -- ^ This needs to change!
    --   Models a chain of rearrange transformations:
    --   binds the name of an array to (1) the name of its base array,
    --   and (2) the complete permutation
  } deriving Show

freshTopEnv :: TopEnv
freshTopEnv = TopEnv [] mempty mempty mempty mempty

addIota2Env :: TopEnv -> (VName, SubExp, PrimExp VName) -> TopEnv
addIota2Env env (iotnm, w_se, w_pe) =
  env { iotas     = M.insert iotnm (w_se,  w_pe) (iotas env)
      , inv_iotas = M.insert w_pe  (iotnm, w_se) (inv_iotas env) }

expandSE :: TopEnv -> SubExp -> PrimType -> PrimExp  VName
expandSE _ (Constant pval) _ = ValueExp pval
expandSE env (Var vnm) ptp = fromMaybe (LeafExp vnm ptp) $ M.lookup vnm $ scalars env

addBinOp2Env :: TopEnv -> (VName, Type) -> BinOp -> SubExp -> SubExp -> TopEnv
addBinOp2Env env (nm,tp) bop s1 s2 =
  env { scalars = M.insert nm (BinOpExp bop (expSE s1) (expSE s2)) (scalars env) }
  where expSE se = expandSE env se $ elemType tp

peFromSe :: TopEnv -> PrimType -> SubExp -> PrimExp VName
peFromSe _ _ (Constant pv) = ValueExp pv
peFromSe env ptp (Var vnm) =
  case M.lookup vnm (scalars env) of
    Just pe -> pe
    Nothing -> LeafExp vnm ptp

removeTpConv :: PrimExp VName -> PrimExp VName
removeTpConv (ConvOpExp _convop pe) = removeTpConv pe
removeTpConv pe = pe

type Env = (TopEnv, BotEnv)

-----------------------------------------
--- Updating Environments
-----------------------------------------

-- | The top-down pass records the scalar expansion, iotas,
--     tiling calls and the like.
--   ToDos:
--     1. maybe record also the change-of-layout transformations
--        into an LMAD, i.e., in the @arrTransf@ filed of @TopEnv@
updateTopdownEnv :: TopEnv -> Stm GPU -> TopEnv
updateTopdownEnv env (Let (Pat [PatElem pat_nm pat_tp]) _aux e)
  | BasicOp (Iota w start stride Int64) <- e,
    start == se0 && stride == se1,
    pe_w <- peFromSe env i64ptp w =
    addIota2Env env (pat_nm, w, pe_w)
  | BasicOp (BinOp bop s1 s2) <- e,
    Prim ptp <- pat_tp,
    (p1, p2) <- (peFromSe env ptp s1, peFromSe env ptp s2) =
    env { scalars = M.insert pat_nm (BinOpExp bop p1 p2) (scalars env) }
  | BasicOp (UnOp unop se) <- e,
    Prim ptp <- pat_tp,
    pe <- peFromSe env ptp se =
    env { scalars = M.insert pat_nm (UnOpExp unop pe) (scalars env) }
  | BasicOp (ConvOp convop se) <- e,
    Prim ptp <- pat_tp,
    pe <- peFromSe env ptp se =
    env { scalars = M.insert pat_nm (ConvOpExp convop pe) (scalars env) }
  | otherwise = env
  where
    se0 = Constant $ IntValue $ Int64Value 0
    se1 = Constant $ IntValue $ Int64Value 1
    i64ptp = IntType Int64
updateTopdownEnv env _ = env

-----------------------------------------
--- Email Nikolaj
-----------------------------------------
{--
|’ve cooked up a monad for you in Schedules.hs and changed the existing functions to use this monad.
The type of runSchedules is unchanged, so the new ScheduleM monad stays hidden in Schedules.hs.

I realize, I don’t know how to run/test your code, so I can’t really show a minimal working example.
But to create a SoP and run FM you’d do something like:

import Futhark.SoP.Monad (lookupUntransPE)
import Futhark.SoP.SoP (sym2SoP, (.+.), int2SoP)
import Futhark.SoP.FourierMotzkin (($>$)
...
-- The case of transposition (just for demo)
applySchedOnStm (_, bu_env) stm@(Let (Pat [pat_el]) _aux e)
  | ... = do
    vn <- lookupUntransPE e        -- This binds a fresh VName to primal expression e in AlgEnv.
    let sop_e = sym2SoP vn         -- Your SoP/Algebra symbol type is just VName.
    sop_e .+. int2SoP 1 $>$ sop_e  -- Dummy query that should pass.
    ...
--}
