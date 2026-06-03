module Futhark.Optimise.IntraShm2Reg.SymTabs
  ( Shm2RegM(..),
    runShm2RegM,
    Shm2RegEnv(..),
    BotEnv(..),
    RegEntry(..),
    AccessKind(..),
    freshBotEnv,
    validEntryForBotEnv,
    unifyAccK,
    regMapSucceeds,
    mergeInFstBotEnv,
    initEntry,
    initEntry1,
    TopEnv(..),
    freshTopEnv,
    addIota2Env,
    addBinOp2Env,
    peFromSe,
    removeTpConv,
    updateTopdownEnv,
    Env,
    intOfAttr2RegMem,
    removeAttr2RegMem,
    intOfAttrGlb2RegOnly,
    removeAttrGlb2RegOnly,
    intOfAttrParDimOnly,
    removeAttrParDimOnly,
    intOfAttrIgnore,
    removeAttrIgnore,
  ) where

import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Set qualified as S
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

data RegEntry = RegEntry
  {
    shpdims :: [(SubExp, PrimExp VName)],
    -- ^ the shape of the array as SubExps and PrimExps as well.
    pardims :: [PrimExp VName],
    -- ^ the parallel dimensions of a successful inner kernel;
    --   these should match the outer dimensions of @shape@
    bindings:: [(VName, AccessKind)]
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

initEntry :: [(SubExp, PrimExp VName)] ->
              [PrimExp VName] ->
              RegEntry
initEntry shp pardims = RegEntry shp pardims mempty

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

-- | checks that the two entries have consistent parallel dimensions;
--   if so it returns the first, otherwise reports an error
validEntryForBotEnv :: VName -> BotEnv -> RegEntry -> RegEntry
validEntryForBotEnv nm bu_env etry2
  | etry1 <- fromMaybe etry2 $ M.lookup nm (regArrays bu_env),
    pardims etry1 == pardims etry2 && shpdims etry1 == shpdims etry2 =
  etry1
validEntryForBotEnv _ _ _ =
  error ("In validEntryForBotEnv found incompatible entries")

-- | conservatively unifies access kinds
unifyAccK :: AccessKind -> AccessKind -> AccessKind
unifyAccK None kind = kind
unifyAccK kind None = kind
unifyAccK Irreg _   = Irreg
unifyAccK _ Irreg   = Irreg
unifyAccK _ _ = Compat      

-- | Arguments:
--     @bu_env@ the bottom-up environment that keeps
--              track of the register-mapping safety
--     @nm@ a program variable name
--   Returns a boolean denoting whether the input variable
--           can be safely mapped to register memory.
--   The safety of register mapping comes down to verifying
--     that said variable is either not used in any of the
--     remaining kernel statements or it is used in a
--     compatible (@Compat@) way with the register mapping.
regMapSucceeds :: BotEnv -> VName -> Bool
regMapSucceeds bu_env nm
  | Just etry <- M.lookup nm $ regArrays bu_env,
    kind <- foldl unifyAccK None $ map snd (bindings etry) =
    kind == None || kind == Compat
regMapSucceeds _ _ = False

-- | merges the results of @env2@ whose keys are also in @env1@;
--   the @env2@ results are collapsed and are added as a new
--   @binding@ of @new_nm@ in the corresponding entry of @env1@
mergeInFstBotEnv :: VName -> BotEnv -> BotEnv -> BotEnv
mergeInFstBotEnv new_nm env1 env2 =
  env1 { regArrays = M.mapWithKey collapse (regArrays env1) }
  where
    collapse key_nm entry1
      | Just entry2 <- M.lookup key_nm (regArrays env2) =
      let kind = foldl unifyAccK None $ map snd $ bindings entry2
      in  entry1 { bindings = (new_nm, kind) : bindings entry1 }
    collapse _ entry1 = entry1
{--
collapseEntry2Bnd :: VName -> RegEntry -> (VName, AccessKind)
collapseEntry2Bnd new_nm entry =
  let kind = foldl unifyAccK None $ map snd $ bindings entry
  in  (new_nm, kind)
--}
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
  , rootSlcArr  :: M.Map VName VName
    -- ^ binds the name of a sliced array to the parent
  , userParams  :: Names
    -- ^ the set of user-defined parameters
  } deriving Show

freshTopEnv :: TopEnv
freshTopEnv = TopEnv [] mempty mempty mempty mempty mempty

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
updateTopdownEnv :: TopEnv -> Stm GPU -> TopEnv
updateTopdownEnv env (Let (Pat [PatElem pat_nm pat_tp]) _aux e)
  | BasicOp (SubExp (Var arrnm)) <- e = add2rootArr arrnm
  | BasicOp (Index arrnm _slice) <- e = add2rootArr arrnm
  | BasicOp (FlatIndex arrnm _)  <- e = add2rootArr arrnm
  | BasicOp (Rearrange arrnm _)  <- e = add2rootArr arrnm
  | BasicOp (Reshape   arrnm _)  <- e = add2rootArr arrnm
  | BasicOp (Opaque _ (Var nm))  <- e = add2rootArr nm
  | BasicOp (UserParam _nm _default_se) <- e =
    env { userParams = oneName pat_nm <> userParams env }
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
    add2rootArr arrnm =
      let root_nm = fromMaybe arrnm $ M.lookup arrnm (rootSlcArr env)
      in  env { rootSlcArr = M.insert pat_nm root_nm (rootSlcArr env) }

updateTopdownEnv env _ = env


----------------------------------------------------
--- Utility Functions, e.g., parsing attributes
----------------------------------------------------

hasCompIntAttr :: String -> Attr -> Bool
hasCompIntAttr str (AttrComp attr_nm [AttrInt _]) =
  attr_nm == nameFromString str
hasCompIntAttr _ _ = False

getIntFromAttr :: String -> Attrs -> Maybe Int
getIntFromAttr str (Attrs attrs) =
  let attrs' = S.toList attrs
   in case L.findIndex (hasCompIntAttr str) attrs' of
        Just ind ->
          case attrs' !! ind of
            AttrComp _ [AttrInt kk] ->
              Just $ fromIntegral kk
            _ -> Nothing
        Nothing -> Nothing

removeAttr :: String -> Attrs -> Attrs
removeAttr str (Attrs attrs) =
  Attrs $ S.filter (not . hasCompIntAttr str) attrs

intOfAttr2RegMem :: Attrs -> Maybe Int
intOfAttr2RegMem = getIntFromAttr "toregmem"

removeAttr2RegMem :: Attrs -> Attrs
removeAttr2RegMem = removeAttr "toregmem"

intOfAttrGlb2RegOnly :: Attrs -> Maybe Int
intOfAttrGlb2RegOnly = getIntFromAttr "glb2reg_only"

removeAttrGlb2RegOnly :: Attrs -> Attrs
removeAttrGlb2RegOnly = removeAttr "glb2reg_only"

intOfAttrParDimOnly :: Attrs -> Maybe Int
intOfAttrParDimOnly = getIntFromAttr "inform_pardim_only"

removeAttrParDimOnly :: Attrs -> Attrs
removeAttrParDimOnly = removeAttr "inform_pardim_only"

intOfAttrIgnore :: Attrs -> Maybe Int
intOfAttrIgnore = getIntFromAttr "ignore"

removeAttrIgnore :: Attrs -> Attrs
removeAttrIgnore = removeAttr "ignore"

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
