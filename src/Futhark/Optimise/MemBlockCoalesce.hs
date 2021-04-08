{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Playground for work on merging memory blocks
module Futhark.Optimise.MemBlockCoalesce (memoryBlockMerging) where

import Control.Arrow
--import Futhark.Representation.AST.Syntax

-- default AbSyn

--import qualified Futhark.Representation.Aliases    as RepAls  -- In

-- Als

import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import qualified Futhark.Analysis.Alias as AnlAls
import Futhark.IR.Aliases
import Futhark.IR.SeqMem (SeqMem)
import qualified Futhark.IR.SeqMem as Mem
import Futhark.Optimise.MemBlockCoalesce.ArrayCoalescing
import Futhark.Optimise.MemBlockCoalesce.Bindage
import Futhark.Optimise.MemBlockCoalesce.DataStructs
import Futhark.Optimise.MemBlockCoalesce.LastUse
import Futhark.Pipeline
import Prelude

-----------------------------------
-----------------------------------
--- Inter-Kernel Memory Merging ---
-----------------------------------
-----------------------------------

type IntrfTab = M.Map VName Names

data IntrfEnv = IntrfEnv
  { -- | the interference table: a memory block
    --   may interfere with several other. Note
    --   that the relation is commutative, but
    --   we might not record both ways.
    intrf :: IntrfTab,
    -- | contains the already allocated memory blocks
    --   as keys and their corresponding sizes as values.
    alloc :: AllocTab,
    -- | alias table between variables and between
    --   memory block names (need to compute this
    --   via a pass)
    alias :: AliasTab,
    -- | records variable names to memory block mapping;
    --   a var might map to several mem blocks due to aliasing.
    v2mem :: V2MemTab,
    -- | the set of active memory blocks; subset of alloca;
    --   a memory block is removed from active upon a last use
    --   and added to active at array creation.
    --                         , fresh :: Names
    -- ^ array variables that are newly created, i.e., they "own"
    --   their memory block during their life span.
    active :: Names
  }

emptyInterfEnv :: IntrfEnv
emptyInterfEnv =
  IntrfEnv
    { intrf = mempty,
      alloc = mempty,
      alias = mempty,
      v2mem = mempty,
      active = mempty
    }

intrfAnPrg :: LUTabPrg -> Mem.Prog Mem.SeqMem -> M.Map Name IntrfEnv
intrfAnPrg lutab prg =
  let aliased_prg = AnlAls.aliasAnalysis prg
      in_place_updates = inPlaceUpdates aliased_prg
   in M.fromList $ map (intrfAnFun in_place_updates lutab) $ progFuns aliased_prg

intrfAnFun :: InPlaceUpdates -> LUTabPrg -> FunDef (Aliases Mem.SeqMem) -> (Name, IntrfEnv)
intrfAnFun in_place_updates lutabprg (FunDef _ _ fname _ _ body) =
  let lutab = fromMaybe M.empty (M.lookup fname lutabprg)
      env = intrfAnBdy in_place_updates lutab emptyInterfEnv body
   in (fname, env)

intrfAnBdy ::
  InPlaceUpdates ->
  LUTabFun ->
  IntrfEnv ->
  Body (Aliases Mem.SeqMem) ->
  IntrfEnv
intrfAnBdy in_place_updates lutab env (Body _ bnds _) =
  foldl (intrfAnBnd in_place_updates lutab) env bnds

intrfAnBnd ::
  InPlaceUpdates ->
  LUTabFun ->
  IntrfEnv ->
  Stm (Aliases Mem.SeqMem) ->
  IntrfEnv
intrfAnBnd in_place_updates _ env (Let pat _ (Op (Mem.Alloc sz _))) =
  case patternNames pat of
    [] -> env
    nm : _ ->
      let nm' = trace ("AllocNode: " ++ pretty nm ++ " size: " ++ pretty sz) nm
       in env {alloc = oneName nm' <> alloc env}
intrfAnBnd in_place_updates lutab env (Let pat _ (DoLoop memctx var_ses _ body)) =
  -- BUG!!! you need to handle the potential circular aliasing
  -- between loop-variant variables and their memory blocks;
  -- just borrow them from the pattern (via substitution)!
  let alias0 = updateAliasing in_place_updates (alias env) pat
      alias' =
        foldl
          ( \acc ((fpar, _), patel) ->
              let patnm = patElemName patel
                  parnm = paramName fpar
               in case trace ("LOOPMEMCTX: " ++ pretty patnm ++ " " ++ pretty parnm) $ M.lookup patnm alias0 of
                    Nothing -> acc
                    Just al -> trace (" found alias set: " ++ pretty (namesToList al)) M.insert parnm al acc
          )
          (alias env)
          $ zip memctx $ patternContextElements pat

      (lvars, _) = unzip var_ses
      lvarmems =
        mapMaybe
          ( \fpar -> case paramDec fpar of
              Mem.MemArray ptp shp _ (Mem.ArrayIn mem_nm idxfun) ->
                Just (paramName fpar, MemBlock ptp shp mem_nm idxfun)
              _ -> Nothing
          )
          lvars
      v2mem' = M.union (v2mem env) $ M.fromList lvarmems

      mems = namesFromList $ map (\(MemBlock _ _ mn _) -> mn) $ map snd lvarmems
      active' =
        active env
          <> namesIntersection
            (alloc env)
            (aliasTransClos alias' mems)

      env' = intrfAnBdy in_place_updates lutab (env {v2mem = v2mem', active = active', alias = alias'}) body
   in defInterference in_place_updates lutab env' pat
intrfAnBnd in_place_updates lutab env (Let pat _ _) =
  defInterference in_place_updates lutab env pat

defInterference ::
  InPlaceUpdates ->
  LUTabFun ->
  IntrfEnv ->
  Pattern (Aliases Mem.SeqMem) ->
  IntrfEnv
defInterference in_place_updates lutab env pat =
  let alias' = updateAliasing in_place_updates (alias env) pat

      arrmems = map (\(a, b, _) -> (a, b)) $ getArrMemAssoc in_place_updates pat
      v2mem' = M.union (v2mem env) $ M.fromList arrmems

      patmems = map (\(MemBlock _ _ mn _) -> mn) $ map snd arrmems
      intrf' = updateInterference alias' (alloc env) (active env) (intrf env) patmems

      lus = fromMaybe mempty (M.lookup (head $ patternNames pat) lutab)
      lumems = getLastUseMem v2mem' lus
      active1 = namesSubtract (active env) lumems

      active' =
        (<>) active1 $
          namesIntersection (alloc env) $
            aliasTransClos alias' $ namesFromList patmems
   in env {alias = alias', v2mem = v2mem', intrf = intrf', active = active'}

--             (Let pat _ (If _ then_body else_body _)) =
{--
intrfAnBnd lutab env (Let pat _ e) =
  let stab  = alias env
      stab' = foldl (\stabb patel->
                        -- compute the transitive closure of current pattern
                        -- name by concating all its aliases entries in stabb
                        let (al0,l1) = patElemAttr patel
                            al = case l1 of
                                   Mem.Scalar tp ->
                                     trace ("MemLore: "++(pretty (patElemName patel))++" is Scalar: "++pretty tp++" ("++pretty l1++") ") al0
                                   Mem.MemMem se sp ->
                                     trace ("MemLore: "++(pretty (patElemName patel))++" is MemMem: "++pretty se++" , "++pretty sp++" ("++pretty l1++") ") al0
                                   Mem.ArrayMem tp shp u nm indfun ->
                                     trace ("MemLore: "++(pretty (patElemName patel))++" is ArrayMem: "++pretty tp++" , "++pretty shp++" , "++pretty u++" , "++pretty nm++" , "++pretty indfun++" ("++pretty l1++") ") al0
                            al_nms = unNames al
                            al_trns= S.foldl' (\acc x -> case M.lookup x stabb of
                                                            Nothing -> acc
                                                            Just aal -> S.union acc aal
                                               ) al_nms al_nms
--al_trns' = trace ("AL Pattern: "++(pretty (patElemName patel))++" aliases: "++pretty (S.toList al_trns)) al_trns
                        in  if null al_trns then stabb
                        else M.insert (patElemName patel) al_trns stabb
                    ) stab $ patternValueElements pat -- patternContextElements pat
  in  env { alias = stab' }
--}

updateInterference :: AliasTab -> AllocTab -> Names -> IntrfTab -> [VName] -> IntrfTab
updateInterference alias0 alloc0 active0 =
  foldl
    ( \itrf mm ->
        let m_al = case M.lookup mm alias0 of
              Just al -> oneName mm <> al
              Nothing -> oneName mm
            m_ad = namesIntersection m_al alloc0
         in foldlNames
              ( \acc m -> case M.lookup m acc of
                  Just mst -> M.insert m (mst <> active0) acc
                  Nothing -> M.insert m active0 acc
              )
              itrf
              m_ad
    )

getLastUseMem :: V2MemTab -> Names -> Names
getLastUseMem v2mem0 =
  foldlNames
    ( \acc lu ->
        case M.lookup lu v2mem0 of
          Nothing -> acc
          Just (MemBlock _ _ m _) -> oneName m <> acc
    )
    mempty

----------------------------------------------------------------
--- Printer/Tester Main Program
----------------------------------------------------------------

{--

lastUseAnPrg :: Mem.Prog Mem.SeqMem -> LUTabPrg
lastUseAnPrg prg = let aliased_prg = AnlAls.aliasAnalysis prg
                   in  M.fromList $ map lastUseAnFun $ progFunctions aliased_prg

--}

memoryBlockMerging :: Mem.Prog Mem.SeqMem -> IO ()
memoryBlockMerging prg = do
  mapM_ lookAtFunction (progFuns prg)

  let lutab = lastUsePrg $ AnlAls.aliasAnalysis prg
      envtab = intrfAnPrg lutab prg

  putStrLn "LAST_USE RESULT:"
  putStrLn $ unlines (map ("  " ++) $ lines $ pretty $ concatMap (map (Control.Arrow.second namesToList) . M.toList) (M.elems lutab))

  putStrLn "ALLOCATIONS RESULT:"
  putStrLn $ unlines (map ("  " ++) $ lines $ pretty $ concatMap (namesToList . alloc) (M.elems envtab))
  --  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concat $ map (\env -> M.toList $ alloc env) $ M.elems envtab)

  putStrLn "ALIAS RESULT:"
  putStrLn $ unlines (map ("  " ++) $ lines $ pretty $ concatMap (map (Control.Arrow.second namesToList) . M.toList . alias) (M.elems envtab))

  putStrLn "INTERFERENCE RESULT:"
  putStrLn $ unlines (map ("  " ++) $ lines $ pretty $ concatMap (map (Control.Arrow.second namesToList) . M.toList . intrf) (M.elems envtab))

  let coaltab = mkCoalsTab $ AnlAls.aliasAnalysis prg
  putStrLn $ "COALESCING RESULT:" ++ pretty (length coaltab)
  let coal_info =
        map
          ( \env ->
              ( dstmem env,
                dstind env,
                namesToList $ alsmem env,
                M.toList $ optdeps env,
                map
                  ( \(k, Coalesced _ (MemBlock _ _ b indfun) sbst) ->
                      (k, (b, indfun, M.toList sbst))
                  )
                  $ M.toList $ vartab env
              )
          )
          $ M.elems coaltab
  putStrLn $ unlines (map ("  " ++) $ lines $ pretty coal_info)

action :: Action SeqMem
action =
  Action
    { actionName = "MemBlockCoalesce",
      actionDescription = "MemBlockCoalesce",
      actionProcedure = liftIO . memoryBlockMerging
    }

lookAtFunction :: Mem.FunDef Mem.SeqMem -> IO ()
lookAtFunction (Mem.FunDef _ _ fname _ params body) = do
  let Mem.Body () bnds res = body
  putStrLn $ "In Function: " ++ pretty fname ++ " with params " ++ pretty params
  mapM_ lookAtBinding bnds
  putStrLn $ "Result: " ++ pretty res
  where
    lookAtBinding (Mem.Let pat _ e) = do
      putStrLn $ "The binding with pattern: " ++ pretty pat
      putStrLn $
        "And corresponding expression:\n"
          ++ unlines (map ("  " ++) $ lines $ pretty e)

--futhark --gpu --memory-playground ../futhark-benchmarks/accelerate/canny/canny.fut
