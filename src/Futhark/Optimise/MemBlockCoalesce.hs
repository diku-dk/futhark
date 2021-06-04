{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Playground for work on merging memory blocks
module Futhark.Optimise.MemBlockCoalesce
       ( memoryBlockMerging )
       where

import Prelude
import Data.Maybe
import Control.Arrow
import qualified Data.Map.Strict as M

import Debug.Trace

--import Futhark.Representation.AST.Syntax

import Futhark.IR.Aliases
import qualified Futhark.IR.SeqMem as ExpMem
--import qualified Futhark.IR.Mem.IxFun as IxFun
import qualified Futhark.Analysis.Alias as AnlAls

import Futhark.Optimise.MemBlockCoalesce.DataStructs
import Futhark.Optimise.MemBlockCoalesce.LastUse
import Futhark.Optimise.MemBlockCoalesce.ArrayCoalescing


-----------------------------------
-----------------------------------
--- Inter-Kernel Memory Merging ---
-----------------------------------
-----------------------------------

type IntrfTab = M.Map VName Names

data IntrfEnv = IntrfEnv { intrf :: IntrfTab
                         -- ^ the interference table: a memory block
                         --   may interfere with several other. Note
                         --   that the relation is commutative, but
                         --   we might not record both ways.
                         , alloc :: AllocTab
                         -- ^ contains the already allocated memory blocks
                         --   as keys and their corresponding sizes as values.
                         , alias :: AliasTab
                         -- ^ alias table between variables and between
                         --   memory block names (need to compute this
                         --   via a pass)
                         , v2mem :: V2MemTab
                         -- ^ records variable names to memory block mapping;
                         --   a var might map to several mem blocks due to aliasing.
                         , active:: Names
                         -- ^ the set of active memory blocks; subset of alloca;
                         --   a memory block is removed from active upon a last use
                         --   and added to active at array creation.
--                         , fresh :: Names
                         -- ^ array variables that are newly created, i.e., they "own"
                         --   their memory block during their life span.
                         }

emptyInterfEnv :: IntrfEnv
emptyInterfEnv = IntrfEnv { intrf = M.empty, alloc = mempty
                          , alias = M.empty, v2mem = M.empty, active = mempty }

intrfAnPrg :: LUTabPrg -> ExpMem.Prog ExpMem.SeqMem -> M.Map Name IntrfEnv
intrfAnPrg lutab prg =
  let aliased_prg = AnlAls.aliasAnalysis prg
  in  M.fromList $ map (intrfAnFun lutab) $ progFuns aliased_prg


intrfAnFun :: LUTabPrg -> FunDef (Aliases ExpMem.SeqMem) -> (Name,IntrfEnv)
intrfAnFun lutabprg (FunDef _ _ fname _ _ body) =
  let lutab = fromMaybe M.empty (M.lookup fname lutabprg)
      env = intrfAnBdy lutab emptyInterfEnv body
  in  (fname, env)

intrfAnBdy :: LUTabFun -> IntrfEnv -> Body (Aliases ExpMem.SeqMem)
           -> IntrfEnv
intrfAnBdy lutab env (Body _ bnds _)  =
  foldl (intrfAnBnd lutab) env bnds

intrfAnBnd :: LUTabFun -> IntrfEnv -> Stm (Aliases ExpMem.SeqMem)
           -> IntrfEnv
intrfAnBnd _ env (Let pat _ (Op (ExpMem.Alloc sz _)) ) =
  case patternNames pat of
    []   -> env
    nm:_ -> let nm' = trace ("AllocNode: "++pretty nm++" size: "++pretty sz) nm
            in  env { alloc = oneName nm' <> alloc env }

intrfAnBnd lutab env (Let pat _ (DoLoop memctx var_ses _ body)) =
  -- BUG!!! you need to handle the potential circular aliasing
  -- between loop-variant variables and their memory blocks;
  -- just borrow them from the pattern (via substitution)!
  let alias0 = updateAliasing (alias env) (pat, Nothing)
      alias' = foldl (\acc ((fpar,_),patel) ->
                        let patnm = patElemName patel
                            parnm = paramName fpar
                        in  case trace ("LOOPMEMCTX: "++pretty patnm++" "++pretty parnm) $ M.lookup patnm alias0 of
                                    Nothing -> acc
                                    Just al -> trace (" found alias set: "++pretty (namesToList al)) $
                                                M.insert parnm al acc
                     ) (alias env) $ zip memctx $ patternContextElements pat
      -- ^ update the aliasing of the loop's memory-block context
      --   with the aliasing info borrowed from the pattern.

      (lvars, _) = unzip var_ses
      lvarmems =
        mapMaybe (\fpar -> case paramDec fpar of
                             ExpMem.MemArray ptp shp _ (ExpMem.ArrayIn mem_nm idxfun) ->
                                 Just (paramName fpar, MemBlock ptp shp mem_nm idxfun)
                             _ -> Nothing
                 ) lvars
      v2mem' = M.union (v2mem env) $ M.fromList lvarmems
      -- ^ update the v2mem with the memory blocks of the loop vars.

      mems = namesFromList $ map (\(MemBlock _ _ mn _) -> mn) $ snd $ unzip lvarmems
      active' = ( namesIntersection (alloc env) $ aliasTransClos alias' mems ) <> active env
      -- ^ add the alias-transitive closure of loop-vars mem blocks to active
      env' = intrfAnBdy lutab (env{v2mem = v2mem', active = active', alias = alias'}) body
  in  defInterference lutab env' (pat, Nothing)
-- in-place update
intrfAnBnd lutab env (Let pat _ (BasicOp (Update old _ _))) =
  defInterference lutab env (pat, Just old)
-- all other expressions
intrfAnBnd lutab env (Let pat _ _) =
  defInterference lutab env (pat, Nothing)

defInterference :: LUTabFun -> IntrfEnv -> (Pattern (Aliases ExpMem.SeqMem), Maybe VName)
                -> IntrfEnv
defInterference lutab env (pat, ip_v) =
  let alias' = updateAliasing (alias env) (pat, ip_v)
      -- ^ record the aliasing of the current statement

      arrmems= getArrMemAssoc pat
      v2mem' = M.union (v2mem env) $ M.fromList arrmems
      -- ^ update v2mem with pattern's (array-var -> mem-block) bindings

      patmems = map (\(MemBlock _ _ mn _) -> mn) $ snd $ unzip arrmems
      intrf' = updateInterference alias' (alloc env) (active env) (intrf env) patmems
      -- ^ update interference: get the alias-transitive closure of each memory
      --   block in pattern and mark its interference with the current active set.

      lus    = fromMaybe mempty (M.lookup (head $ patternNames pat) lutab)
      lumems = getLastUseMem v2mem' lus
      active1= namesSubtract (active env) lumems
      -- ^ get the memory blocks associated to the variables lastly used in this
      --   statement and subtract them from the active set.

      active'= active1 <> 
               ( namesIntersection (alloc env) $ aliasTransClos alias' $ namesFromList patmems )
      -- ^ update the active set with the alias-transitive closure of this stmt
      --   memory blocks (keep only the allocated ones of course).

  in env { alias = alias', v2mem = v2mem', intrf = intrf', active = active' }

updateInterference :: AliasTab -> AllocTab -> Names -> IntrfTab -> [VName] -> IntrfTab
updateInterference alias0 alloc0 active0 =
  foldl (\itrf mm -> let m_al = case M.lookup mm alias0 of
                                  Just al -> oneName mm <> al
                                  Nothing -> oneName mm
                         m_ad = namesToList $ namesIntersection m_al alloc0
                     in  foldl (\acc m -> case M.lookup m acc of
                                               Just mst-> M.insert m (mst <> active0) acc
                                               Nothing -> M.insert m active0 acc
                               ) itrf m_ad
        )

getLastUseMem :: V2MemTab -> Names -> Names
getLastUseMem v2mem0 =
  foldl (\acc lu ->
            case M.lookup lu v2mem0 of
              Nothing -> acc
              Just (MemBlock _ _ m _)  -> oneName m <> acc
        ) mempty . namesToList

----------------------------------------------------------------
--- Printer/Tester Main Program
----------------------------------------------------------------

-- run it with: `futhark dev --cpu --merge-mem test.fut`
memoryBlockMerging :: ExpMem.Prog ExpMem.SeqMem -> IO ()
memoryBlockMerging prg = do
  mapM_ lookAtFunction (progFuns prg)

  let lutab   = lastUsePrg $ AnlAls.aliasAnalysis prg
      envtab  = intrfAnPrg lutab prg

  putStrLn "LAST_USE RESULT:"
  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concatMap (map (Control.Arrow.second namesToList) . M.toList) (M.elems lutab))

  putStrLn "ALLOCATIONS RESULT:"
  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concatMap (namesToList . alloc) (M.elems envtab))
--  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concat $ map (\env -> M.toList $ alloc env) $ M.elems envtab)

  putStrLn "ALIAS RESULT:"
  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concatMap (map (Control.Arrow.second namesToList) . M.toList . alias) (M.elems envtab))

  putStrLn "INTERFERENCE RESULT:"
  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concatMap (map (Control.Arrow.second namesToList) . M.toList . intrf) (M.elems envtab))

  let coaltab = mkCoalsTab $ AnlAls.aliasAnalysis prg
  putStrLn $ "COALESCING RESULT:" ++ pretty (length coaltab)
  let coal_info = map (\env ->
                            ( dstmem env, dstind env, namesToList $ alsmem env, M.toList $ optdeps env
                            , map (\ (k,Coalesced _ (MemBlock _ _ b indfun) sbst) ->
                                    (k,(b,indfun,M.toList sbst))
                                  ) $ M.toList $ vartab env
                            )
                      ) $ M.elems coaltab
  putStrLn $ unlines (map ("  "++) $ lines $ pretty coal_info)


lookAtFunction :: ExpMem.FunDef ExpMem.SeqMem -> IO ()
lookAtFunction fdef = do
  putStrLn $ "Function:\n" ++ pretty fdef

-- try also ../futhark-benchmarks/accelerate/canny/canny.fut (?)
