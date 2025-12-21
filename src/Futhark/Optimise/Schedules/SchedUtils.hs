{-# LANGUAGE Strict #-}

-- | This module consists of the representation of
--   a schedule and related utility functions
module Futhark.Optimise.Schedules.SchedUtils
  ( AdjustRes(..)
  , BotEnv  (..)
  , freshBotEnv
  , HLSched (..)
  , ParMode (..)
  , splitAtSched
  , parMode
  , mkRegIndStrides
  , parseSchedule
  , headOfSched
  , tailOfSched
  , sortByPerm
  , append2Sched
  , oneFullyConsumedMapRed
  , equivLambdas
  , fromFParam
  , toFParam
  , stmtMatchesSchedule
  )
where

import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.IR.SOACS -- hiding (SOAC (..))
import Futhark.Tools
import Futhark.IR.Mem.LMAD qualified as LMAD
import Futhark.Util.Pretty hiding (line, sep, (</>))
import Futhark.Optimise.Schedules.EnvUtils( TopEnv(..), mulPes, peFromSe )

--import Debug.Trace

-------------------------------------------------------
--- LMAD-Like Representation of a High-Level Schedule 
-------------------------------------------------------

type LMAD = LMAD.LMAD (TPrimExp Int64 VName)

-- type LMAD = LMAD.LMAD (PrimExp VName)

data AdjustRes = ExactRes | ManifestRes | SubstituteRes

instance Pretty AdjustRes where
  pretty ExactRes      = "Exact Result"
  pretty ManifestRes   = "Manifest Result"
  pretty SubstituteRes = "Substitute Result"

data HLSched = HLS
  { dimlens :: [PrimExp VName]
  , origids :: [Int]
  , sigma   :: [Int]
  , strides :: [PrimExp VName]
  , signals :: [Int]
  , virthds :: [PrimExp VName]
  , whatres :: AdjustRes
  , permres :: [Int]
  , padinner:: [PrimExp VName]
  , fuselevs:: [Int]
  }

data BotEnv = BotEnv
  { schedules :: M.Map VName (PatElem (LetDec SOACS), HLSched, LMAD, Stms SOACS)
    -- ^ binds the name of the target soac reasult to a quad tuple:
    --   (1) the result pattern of the schedule, (2) the schedule,
    --   (3) an LMAD index-function supporting layout transformations,
    --   (4) the chain of statements until reaching the target soac;
    --       the last element must be the statement defining the schedule.
  , optstms   :: Stms SOACS
  }

freshBotEnv :: BotEnv
freshBotEnv = BotEnv mempty mempty


splitAtSched :: Int -> HLSched -> (HLSched, HLSched)
splitAtSched k sched =
  let (dimlens', dimlens'') = splitAt k (dimlens sched)
      (strides', strides'') = splitAt k (strides sched)
      (origids', origids'') = splitAt k (origids sched)
      (signals', signals'') = splitAt k (signals sched)
      (sigma',   sigma''  ) = splitAt k (sigma   sched)
      (permres', permres'') = splitAt k (permres sched)
  in ( mkSched dimlens'  origids'  sigma'  strides'  signals'  permres'
     , mkSched dimlens'' origids'' sigma'' strides'' signals'' permres'' 
     )
  where
    mkSched d o prm strd sign r =
      sched { dimlens = d, origids = o, sigma = prm, strides = strd, signals = sign, permres = r }

instance Pretty HLSched where
  pretty sched =
    "{\n\tDimLens: " <+> pretty (dimlens sched) <>
     "\n\tOrigIds: " <+> pretty (origids sched) <>
     "\n\tDimPerm: " <+> pretty (sigma   sched) <>
     "\n\tSignals: " <+> pretty (signals sched) <>
     "\n\tStrides: " <+> pretty (strides sched) <>
     "\n\tVirtThds: "<+> pretty (virthds sched) <>
     "\n\tWhatRes: " <+> pretty (whatres sched) <>
     "\n\tPadInner: "<+> pretty (padinner sched) <>
     "\n\tFuseLevs: "<+> pretty (fuselevs sched) <>
     "   }"

data ParMode = Par | Macc | Seq | Flip deriving Eq

parMode :: Int -> ParMode
parMode signal =
  case signal `rem` 4 of
    0 -> Par
    1 -> Macc
    2 -> Seq
--    3 -> Flip
    _ -> error "Impossible case reached!"

mkRegIndStrides :: HLSched -> [PrimExp VName]
mkRegIndStrides sched =
  let modes = map parMode $ signals sched
      mul_carries = scanl f pe1 $ zip modes (dimlens sched)
      strides = reverse $ scanl mulPes pe1 $ reverse (dimlens sched)
  in  map g $ zip3 modes mul_carries strides
  where
    pe1 = ValueExp $ IntValue $ Int64Value 1
    f carry (Flip, n) = mulPes n carry
    f carry _ = carry
    g (Flip, carry, _) = carry
    g (_, carry, strd) = mulPes strd carry

-------------------------------
---   Parsing the Schedule  ---
-------------------------------
parseSchedule :: Name -> TopEnv -> [SubExp] -> Pat (LetDec SOACS) -> 
                 (VName, PatElem (LetDec SOACS), HLSched, LMAD)
parseSchedule _fnm td_env args pat
  | [pat_el] <- patElems pat,
    Array _ptp shp _ <- patElemDec pat_el,
    arr : fus : pad : fps : ms_res : virt : strds : sigs : perm : oinds : ns : _ <- L.reverse args =
  let sched =
          HLS { dimlens = getPrimExpLit ns
              , origids = getIntegerLit oinds
              , sigma   = getIntegerLit perm
              , signals = getIntegerLit sigs
              , strides = getPrimExpLit strds
              , virthds = getPrimExpLit virt
              , whatres = getAdjustRes ms_res
              , permres = getIntegerLit fps
              , padinner= getPrimExpLit pad
              , fuselevs= getIntegerLit fus
              }
      shp_pes = map (isInt64 . peFromSe td_env (IntType Int64)) $ shapeDims shp 
      lmad = LMAD.iotaStrided (isInt64 pe0) (isInt64 pe1) shp_pes
  in  (getVName arr, pat_el, sched, lmad)
  where
    pe0 = ValueExp $ IntValue $ Int64Value 0
    pe1 = ValueExp $ IntValue $ Int64Value 1
    --
    getVName (Var nm) = nm
    getVName se = error ("Trying to read the soac-result name, but got: "++prettyString se)
    --
    getPrimExpLit (Constant _) =
      error ("The result of an array literal cannot be a constant")
    getPrimExpLit (Var nm)
      | Just pes <- M.lookup nm (arrayLits td_env) = pes
      | otherwise = error ("something went wrong with tracking literals of " ++ prettyString nm)
    getIntegerLit nm =
      map toInt $ getPrimExpLit nm
    --
    getAdjustRes (Var _) = error "Illegal var name denoting AdjustResult"
    getAdjustRes (Constant pval)
      | pval == IntValue (Int64Value 0) = ExactRes
      | pval == IntValue (Int64Value 1) = ManifestRes
      | pval == IntValue (Int64Value 2) = SubstituteRes
      | otherwise = error "Illegal integer for the kind of AdjustResult"
    --
    toInt :: PrimExp VName -> Int
    toInt (ValueExp (IntValue iv)) = valueIntegral iv
    toInt pe = error ("This was supposed to be a constant int value; instead is "++prettyString pe)
--
parseSchedule fnm _env args pat =
  error ( "Error when parsing the schedule named " ++ nameToString fnm ++
          " result pattern: " ++ prettyString pat ++ 
          " arguments in reverse order: " ++ prettyString (L.reverse args)
        ) 
-------------------------------
--- Other Utility Functions ---
-------------------------------

nullSched :: HLSched -> Bool
nullSched sched =
  null (dimlens sched) || null (strides sched) ||
  null (origids sched) || null (signals sched) || null (sigma sched)

headOfSched :: HLSched -> (PrimExp VName, PrimExp VName, Int, Int, Int)
headOfSched sched
  | nullSched sched =
    error ("Illegal Schedule passed as argument to headOfSched: "++prettyString sched)
headOfSched sched =
  ( head (dimlens sched)
  , head (strides sched)
  , head (origids sched)
  , head (sigma   sched)
  , head (signals sched)
  )

tailOfSched :: HLSched -> HLSched
tailOfSched sched
  | nullSched sched =
    error ("Illegal Schedule passed as argument to tailOfSched: "++prettyString sched)
tailOfSched sched =
  sched { dimlens = tail (dimlens sched)
        , strides = tail (strides sched)
        , origids = tail (origids sched)
        , sigma   = tail (sigma   sched)
        , signals = tail (signals sched)
        }

sortByPerm :: HLSched -> HLSched
sortByPerm sched =
  let lst = L.zip5 (dimlens sched) (strides sched) (origids sched) (sigma sched) (signals sched)
      (lens, strds, oids, sigm, signs)= L.unzip5 $ L.sortBy sortGT lst
  in  sched { dimlens = lens, strides = strds, origids = oids, sigma = sigm, signals = signs }
  where
    sortGT (_,_,_,p1,_) (_,_,_,p2,_)
      | p1 < p2 = LT
      | p1 > p2 = GT
      | True = GT

append2Sched :: (PrimExp VName, PrimExp VName, Int, Int, Int) -> HLSched -> HLSched
append2Sched (l, s, o, p, d) sched =
  sched { dimlens = l : (dimlens sched)
        , strides = s : (strides sched)
        , origids = o : (origids sched)
        , sigma   = p : (sigma   sched)
        , signals = d : (signals sched)
        }

oneFullyConsumedMapRed :: ScremaForm SOACS -> Maybe (Lambda SOACS)
oneFullyConsumedMapRed (ScremaForm map_lam [] [Reduce _com red_lam _ne])
  | lambdaReturnType red_lam == lambdaReturnType map_lam = Just red_lam
oneFullyConsumedMapRed _ = Nothing

-- | ToDo: extend for map-nest on top of the same binary operator.
equivLambdas :: (Lambda SOACS) -> (Lambda SOACS) -> Bool 
equivLambdas lam1 lam2
  | lambdaReturnType lam1 == lambdaReturnType lam2,
    [Let (Pat [pat1]) _ (BasicOp (BinOp bop1 se11 se12))] <- stmsToList $ bodyStms $ lambdaBody lam1,
    [Let (Pat [pat2]) _ (BasicOp (BinOp bop2 se21 se22))] <- stmsToList $ bodyStms $ lambdaBody lam1,
    [se11, se12] == map (Var . paramName) (lambdaParams lam1),
    [se21, se22] == map (Var . paramName) (lambdaParams lam2) =
  let res12  = concat $ map (bodyResult . lambdaBody) [lam1, lam2]
      res12' = map (subExpRes . Var . patElemName) [pat1, pat2]
  in  bop1 == bop2 && res12 == res12' 
equivLambdas _ _ = False

toFParam :: LParam SOACS -> FParam SOACS
toFParam p = Param (paramAttrs p) (paramName p) $ toDecl (paramDec p) Unique

fromFParam :: FParam SOACS -> LParam SOACS
fromFParam p = Param (paramAttrs p) (paramName p) $ fromDecl (paramDec p)

---------------------------------------------------------
--- Checking if a statement conforms with a schedule, ---
---   in particular because we need to find the       ---
---   statements that are part of the target chain of ---
---   recurrences of a schedule                       ---
---------------------------------------------------------

stmtMatchesSchedule :: TopEnv -> Stm SOACS -> HLSched -> Bool
stmtMatchesSchedule _ _ sched
  | length (dimlens sched) == 0  = True
  -- error "In stmMatchesSchedule: empty schedule as argument!"
stmtMatchesSchedule env stm sched =
  case getMBody stm of
    Nothing   -> False
    Just body -> bodyMatchesSchedule env body (tailOfSched sched)
  where
    getMBody (Let _ _ (Loop _ ForLoop{} loop_body)) = Just loop_body
    getMBody (Let _ _ (Op (Screma _ _ (ScremaForm lam [] [])))) = Just $ lambdaBody lam
    getMBody (Let _ _ (Op (Screma _ _ form@(ScremaForm lam _ _)))) =
      if isJust (oneFullyConsumedMapRed form) then Just (lambdaBody lam) else Nothing
    getMBody _ = Nothing

bodyMatchesSchedule :: TopEnv -> Body SOACS -> HLSched -> Bool
bodyMatchesSchedule env body sched =
  foldl f False $ reverse $ stmsToList $ bodyStms body
  where
    f True _ = True
    f _ stm = stmtMatchesSchedule env stm sched  

