{-# LANGUAGE Strict #-}

-- | This module contains the definition of the
--   Top-Down Environment of Schedules and related
--   utility functions.
module Futhark.Optimise.Schedules.EnvUtils
  ( SoacKind(..)
  , FwdTileTab
  , InvTileTab
  , AssertEnv (..)
  , TopEnv (..)
  , freshTopEnv
  , addIota2Env
  , addTransf2Env
  , updateTopdownEnv
  , tileFuns
  , eqPEs
  , addPes
  , mulPes
  , minPes
  , invPerm
  , identityPerm
  , findType
  , findPType
  , peFromSe
  )
where

import Data.List qualified as L
--import Data.Set qualified as S 
--import Data.Maybe
import Data.Map.Strict qualified as M
import Futhark.IR.SOACS hiding (SOAC (..))
import Futhark.Tools
import Futhark.SoP.SoP
--import Futhark.SoP.Convert
--import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..))

-- import Debug.Trace

-----------------------------------
--- names of special functions  ---
-----------------------------------

tileFuns :: [String]
tileFuns = ["strip1", "strip2"]

data SoacKind = Map | MapAccum | RedoMapF | Redomap | ScanoMapF | ScanoMap | GenScrema | DoLoop
                deriving (Eq, Show)

-----------------------------------
--- Environment
-----------------------------------

-- | Helper environment used to process the asserted (in)equalities:
--   1. @assert(N > 0)@ can refine the range of @N@ in the FM monad
--   2. @assert( d<{(+) N 2}>_6509 == N+2 ) extends the @scalars@
--        table with the binding @d<{(+) N 2}>_6509 |-> N+2@.
--   In order to do so, when encountering an @assert@ statement, we
--     need to retrieve:
--       (a) the logical conjunctions denoted by a variable name, 
--           e.g., @p = p1 && p2@; these are hold in @conjTrace@
--       (b) the (in)equality associated with a variable name,
--           e.g., @p = N > 0@ ; these are hold in @compTrace@
--       (c) @ineqTab@ needs to be removed: the equalities will
--           be directly encoded in the @scalars@ env, while the
--           inequalities should be added directly to the FM monad.
data AssertEnv = AssertEnv
  { compTrace :: M.Map VName (CmpOp, SubExp, SubExp)
  , conjTrace :: M.Map VName Names
  , ineqTab   :: M.Map VName ([SoP VName], [SoP VName])
  } deriving Show

freshAssertEnv :: AssertEnv
freshAssertEnv = AssertEnv mempty mempty mempty

-- type LMAD = LMAD.LMAD (TPrimExp Int64 VName)

-- | Binds a size expression to its tiled
--     approximation: `e -> [t1,...,tm]`,
--     where `t1 * ... * tm` is an upper
--     bound of `e`, i.e., `e <= t1*...*tm`
--   E.g., `let (m, t1, t2) = strip2 (2*M)`
--     produces `2*M -> [m, t1, t2]`
--   ToDo:  maybe use the Poly IR here?
type FwdTileTab = M.Map (PrimExp VName) (SoP VName, Names)

-- ^ Binds a symbolic tile names to a tuple:
--     1. the original size that was tiled
--     2. the list containing the produced tiles.
--   E.g., `let (m, t1, t2) = strip2 (2*M)`
--     produces `m -> (2*M, [m, t1, t2])`
type InvTileTab = M.Map VName ((PrimExp VName, SoP VName), Names)

data TopEnv = TopEnv
  { bdy_res     :: [SubExp]
  , appTilesFwd :: FwdTileTab
  , appTilesInv :: InvTileTab
  , iotas       :: M.Map VName (SubExp, PrimExp VName, SoP VName) -- , Stms SOACS
    -- ^ binds the name of an iota array to its size
  , inv_iotas   :: M.Map (PrimExp VName) (VName, SubExp, SoP VName)
    -- ^ binds a size pexp to the name of a iota and its SubExp size
  , scalars     :: M.Map VName (PrimExp VName, SoP VName)
    -- ^ binds the name of a scalar var to its expression
  , arrayLits   :: M.Map VName [(PrimExp VName, SoP VName)]
    -- ^ binds the name of an array literal to its expression 
  , arrTransf   :: M.Map VName (VName, [Int])
    -- ^ Models a chain of rearrange transformations:
    --   binds the name of an array to (1) the name of its base array,
    --   and (2) the complete permutation
  , assertHelp  :: AssertEnv
  } deriving Show

freshTopEnv :: TopEnv
freshTopEnv = TopEnv [] mempty mempty mempty mempty mempty mempty mempty freshAssertEnv

addIota2Env :: TopEnv -> (VName, SubExp, (PrimExp VName, SoP VName)) -> TopEnv
addIota2Env env (iotnm, w_se, (w_pe, w_sop)) =
  env { iotas     = M.insert iotnm (w_se,  w_pe, w_sop) (iotas env)
      , inv_iotas = M.insert w_pe  (iotnm, w_se, w_sop) (inv_iotas env) }

addTileBinding :: TopEnv -> (PrimExp VName, SoP VName) -> [VName] -> TopEnv
addTileBinding env (pe, sop) tile_nms =
  let nms = namesFromList tile_nms
      fwdenv = M.insert pe (sop, nms) $ appTilesFwd env
      bwdenv = foldl (foldfun nms) (appTilesInv env) tile_nms
  in  env { appTilesFwd = fwdenv, appTilesInv = bwdenv }
  where
    foldfun nms env_cur nm = M.insert nm ((pe,sop), nms) env_cur

addTransf2Env :: TopEnv -> VName -> VName -> [Int] -> TopEnv
addTransf2Env env nm_new nm_old perm_new =
  let (nm_old', perm') =
       case M.lookup nm_old (arrTransf env) of
         Nothing  -> (nm_old, perm_new)
         Just (nm_old0, perm_old) ->
           (nm_old0, composePerms perm_new perm_old)
  in env { arrTransf = M.insert nm_new (nm_old', perm') (arrTransf env) }
  where
    composePerms :: [Int] -> [Int] -> [Int]
    composePerms prm_new prm_old = 
      map (\ i -> prm_old !! i) prm_new

{--
addBinOp2Env :: TopEnv -> (VName, Type) -> BinOp -> SubExp -> SubExp -> TopEnv
addBinOp2Env env (nm,tp) bop s1 s2 =
  env { scalars = M.insert nm (BinOpExp bop (expSE s1) (expSE s2)) (scalars env) }
  where expSE se = peFromSe env (elemType tp) se
--}

{--
-- @fromSoP@ and @toSoPNum@ require to work under @MonadSoP@
addBinOp2Env :: (MonadSoP VName (PrimExp VName) m) => TopEnv -> (VName, Type) -> BinOp -> SubExp -> SubExp -> m TopEnv
addBinOp2Env env (nm,tp) bop s1 s2 = do
  let ((pex1, sop1), (pex2, sop2)) = (expSE s1, expSE s2)
      fv_sop = S.union (freeInSoP sop1) (freeInSoP sop2)
      -- in order for this to work, it needs to be inside a monad
  pe_sop1 <- fromSoP sop1
  pe_sop2 <- fromSoP sop2
  (_, sop0) <- toSoPNum $ BinOpExp bop pe_sop1 pe_sop2
  let sop  = if null (S.difference (freeInSoP sop0) fv_sop)
             then sop0 -- success, no new variables were introduced
             else sym2SoP nm -- binop result is not in SoP domain
      pe   = BinOpExp bop pex1 pex2
  pure $ env { scalars = M.insert nm (pe, sop) (scalars env) }
  where
    expSE se = peFromSe env (elemType tp) se
    freeInSoP :: SoP VName -> S.Set VName
    freeInSoP = free
--}

-- | The top-down pass records the scalar expansion, iotas,
--     tiling calls and the like.
--   ToDos:
--     1. maybe record also the change-of-layout transformations
--        into an LMAD, i.e., in the @arrTransf@ filed of @TopEnv@
updateTopdownEnv :: TopEnv -> Stm SOACS -> TopEnv
updateTopdownEnv env (Let pat _aux (Apply fnm arg_diets _ _))
  | any (`L.isPrefixOf` (nameToString fnm)) tileFuns,
    [(size_se, _)] <- arg_diets,
    ptp:_ <- map (elemType . patElemDec)  $ patElems pat =
  addTileBinding env (peFromSe env ptp size_se) $
    map patElemName $ patElems pat
updateTopdownEnv env (Let pat@(Pat [PatElem pat_nm pat_tp]) aux e)
  | BasicOp (Iota w start stride Int64) <- e,
    start == se0 && stride == se1,
    pe_sop_w <- peFromSe env i64ptp w =
    addIota2Env env (pat_nm, w, pe_sop_w)
  | BasicOp (BinOp LogAnd s1 s2) <- e,
    nms <- getConj s1 <> getConj s2,
    not (null (namesToList nms)) =
    let conjTrace' = M.insert pat_nm nms $ conjTrace $ assertHelp env
    in  env { assertHelp = (assertHelp env) { conjTrace = conjTrace' } }
  | BasicOp (CmpOp cmpop s1 s2) <- e,
    isIntComp cmpop =
    let compTrace' = M.insert pat_nm (cmpop, s1, s2) $ compTrace $ assertHelp env
    in  env { assertHelp = (assertHelp env) { compTrace = compTrace' } }
  | BasicOp (BinOp bop s1 s2) <- e,
    Prim ptp <- pat_tp =
    let (p1, sop1) = peFromSe env ptp s1
        (p2, sop2) = peFromSe env ptp s2
        pex = BinOpExp bop p1 p2
        sop = applyBinOp2SoP bop sop1 sop2
    in env { scalars = M.insert pat_nm (pex,sop) (scalars env) }
  | BasicOp (UnOp unop se) <- e,
    Prim ptp <- pat_tp,
    (pe, sop) <- peFromSe env ptp se =
    env { scalars = M.insert pat_nm (UnOpExp unop pe, applyUnOp2SoP unop sop) (scalars env) }
  | BasicOp (Assert (Var nm) _ _) <- e,
    Just onecomp <- M.lookup nm (compTrace (assertHelp env)) =
      handleComparison onecomp
  | BasicOp (Assert (Var nm) arg1 arg2) <- e,
    Just nms <- M.lookup nm (conjTrace (assertHelp env)) =
      let mkDummyStmt x = Let pat aux $ BasicOp $ Assert (Var x) arg1 arg2
      in  foldl updateTopdownEnv env $ map mkDummyStmt $ namesToList nms
  | BasicOp (ArrayLit arg_ses (Prim ptp)) <- e =
    env { arrayLits = M.insert pat_nm (map (peFromSe env ptp) arg_ses) (arrayLits env) }
  | BasicOp (Scratch _ptp [se]) <- e,
    se == se0 =
    env { arrayLits = M.insert pat_nm [] (arrayLits env) }
  | BasicOp (Replicate shp se) <- e,
    [d] <- shapeDims shp,
    Constant (IntValue (Int64Value n)) <- d =
    let pe_sop = peFromSe env (elemType pat_tp) se
        arr_pe_sop = replicate (fromIntegral n) pe_sop
    in  env { arrayLits = M.insert pat_nm arr_pe_sop (arrayLits env) }
  | BasicOp (Rearrange vnm sigma) <- e =
    addTransf2Env env pat_nm vnm sigma
  | otherwise = env
  where
    se0 = Constant $ IntValue $ Int64Value 0
    se1 = Constant $ IntValue $ Int64Value 1
    i64ptp = IntType Int64
    --
    applyBinOp2SoP Add{} sop1 sop2 = sop1 .+. sop2
    applyBinOp2SoP Sub{} sop1 sop2 = sop1 .-. sop2
    applyBinOp2SoP Mul{} sop1 sop2 = sop1 .*. sop2
    applyBinOp2SoP _ _ _ = sym2SoP pat_nm
    --
    applyUnOp2SoP (Complement _) sop = int2SoP 0 .-. sop
    applyUnOp2SoP _ _ = sym2SoP pat_nm
    --
    isIntComp (CmpEq (IntType _)) = True
    isIntComp (CmpSlt _) = True
    isIntComp (CmpSle _) = True
    isIntComp (CmpUlt _) = True
    isIntComp (CmpUle _) = True
    isIntComp _ = False
    --
    getConj (Var nm)
      | Just nms <- M.lookup nm (conjTrace (assertHelp env)) = nms
    getConj (Var nm) = oneName nm
    getConj _ = mempty
    --
    handleComparison (CmpEq ptp@(IntType _), s1, s2) =
    -- ^ for now we only treat equality; ToDo: handle inequalities using FM's monoid
      case (peFromSe env ptp s2, peFromSe env ptp s1) of
        ( ((LeafExp nm _), _), (p1,sop1) ) ->
          env { scalars = M.insert nm (p1,sop1) (scalars env) }
        ( (p2,sop2), ((LeafExp nm _), _) ) ->
          env { scalars = M.insert nm (p2,sop2) (scalars env) }
        _ -> env
    handleComparison _ = env
updateTopdownEnv env _ = env

-------------------------------------------------------
--- Simple Utilities
-------------------------------------------------------

identityPerm :: [Int] -> Bool
identityPerm [] = True
identityPerm perm = perm == [0 .. length perm - 1]

invPerm :: [Int] -> [Int]
invPerm xs =
  map f [0..length xs-1]
  where
    f i | Just ind <- L.elemIndex i xs = ind
    f _ = error "Violation of assumed permutation semantics"

eqPEs :: PrimExp VName -> PrimExp VName -> Bool
eqPEs p1 p2 | p1 == p2 = True
eqPEs (BinOpExp bop1 pe11 pe12) ((BinOpExp bop2 pe21 pe22))
  | pe11 == pe22 && pe12 == pe21 && bop1 == bop2 = isCommBop bop1
  where
    isCommBop Add{} = True
    isCommBop Mul{} = True
    isCommBop SMax{}= True
    isCommBop SMin{}= True
    isCommBop UMin{}= True
    isCommBop UMax{}= True
    isCommBop _     = False
eqPEs _ _ = False

pe0 :: PrimExp VName
pe0 = ValueExp $ IntValue $ Int64Value 0

pe1 :: PrimExp VName
pe1 = ValueExp $ IntValue $ Int64Value 1

addPes :: PrimExp VName -> PrimExp VName -> PrimExp VName
addPes e1 e2 | e1 == pe0 = e2
addPes e1 e2 | e2 == pe0 = e1
addPes e1 e2 = BinOpExp (Add Int64 OverflowWrap) e1 e2  -- OverflowUndef

mulPes :: PrimExp VName -> PrimExp VName -> PrimExp VName
mulPes e1 e2 | e1 == pe1 = e2
mulPes e1 e2 | e2 == pe1 = e1
mulPes e1 e2 = BinOpExp (Mul Int64 OverflowWrap) e1 e2  -- OverflowUndef

minPes :: PrimExp VName -> PrimExp VName -> PrimExp VName
minPes e1 e2 | e1 == e2 = e1
minPes e1 e2 = BinOpExp (SMin Int64) e1 e2

findType :: (HasScope SOACS m) => SubExp -> m Type
findType (Constant pval) = pure $ Prim $ primValueType pval
findType (Var vnm) = lookupType vnm

findPType :: (HasScope SOACS m, Monad m) => SubExp -> m PrimType
findPType se = do
  tp <- findType se
  case tp of
    Prim ptp -> pure ptp
    _ -> error ("In findPType subexp: " ++ prettyString se ++
                " has type "++prettyString tp++", which is not PrimType")

peFromSe :: TopEnv -> PrimType -> SubExp -> (PrimExp VName, SoP VName)
peFromSe _ ptp@(IntType _) (Constant pv@(IntValue iv))
  | ptp == primValueType pv = (ValueExp pv, int2SoP $ valueIntegral iv) 
peFromSe _ ptp (Constant pv) =
  error ("In peFromSe: value " ++ prettyString pv ++ " is not of int type " ++ prettyString ptp)
peFromSe env ptp (Var vnm)
  | Nothing <- M.lookup vnm (scalars env) =
  (LeafExp vnm ptp, sym2SoP vnm)
peFromSe env ptp@(IntType _) (Var vnm)
  | Just (pe, sop) <- M.lookup vnm (scalars env),
    primExpType pe == ptp =
  (pe, sop)
peFromSe _env ptp (Var vnm) =
  error ("In peFromSe: the binding of: " ++ prettyString vnm ++ " is not of int type " ++ prettyString ptp)

