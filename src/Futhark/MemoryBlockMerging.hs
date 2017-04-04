{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Playground for work on merging memory blocks
module Futhark.MemoryBlockMerging
       ( memoryBlockMerging )
       where

import Prelude
import Data.Maybe
import Control.Arrow
import qualified Data.Map.Strict as M
import qualified Data.Set      as S

import Debug.Trace

--import Futhark.Representation.AST.Syntax

import Futhark.Representation.Aliases -- default AbSyn
import qualified Futhark.Representation.ExplicitMemory as ExpMem
--import qualified Futhark.Representation.Aliases    as RepAls  -- In
import qualified Futhark.Analysis.Alias            as AnlAls  -- Als

import Futhark.Optimise.MemBlkMerging.DataStructs
import Futhark.Optimise.MemBlkMerging.LastUse
import Futhark.Optimise.MemBlkMerging.ArrayCoalescing


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
emptyInterfEnv = IntrfEnv { intrf = M.empty, alloc = S.empty
                          , alias = M.empty, v2mem = M.empty, active = S.empty }

intrfAnPrg :: LUTabPrg -> ExpMem.Prog ExpMem.ExplicitMemory -> M.Map Name IntrfEnv
intrfAnPrg lutab prg =
  let aliased_prg = AnlAls.aliasAnalysis prg
  in  M.fromList $ map (intrfAnFun lutab) $ progFunctions aliased_prg


intrfAnFun :: LUTabPrg -> FunDef (Aliases ExpMem.ExplicitMemory) -> (Name,IntrfEnv)
intrfAnFun lutabprg (FunDef _ fname _ _ body) =
  let lutab = fromMaybe M.empty (M.lookup fname lutabprg)
      env = intrfAnBdy lutab emptyInterfEnv body
  in  (fname, env)

intrfAnBdy :: LUTabFun -> IntrfEnv -> Body (Aliases ExpMem.ExplicitMemory)
           -> IntrfEnv
intrfAnBdy lutab env (Body _ bnds _)  =
  foldl (intrfAnBnd lutab) env bnds

intrfAnBnd :: LUTabFun -> IntrfEnv -> Stm (Aliases ExpMem.ExplicitMemory)
           -> IntrfEnv
intrfAnBnd _ env (Let pat _ (Op (ExpMem.Alloc sz _)) ) =
  case patternNames pat of
    []   -> env
    nm:_ -> let nm' = trace ("AllocNode: "++pretty nm++" size: "++pretty sz) nm
            in  env { alloc = S.insert nm' (alloc env)}

intrfAnBnd lutab env (Let pat _ (DoLoop memctx var_ses _ body)) =
  -- BUG!!! you need to handle the potential circular aliasing
  -- between loop-variant variables and their memory blocks;
  -- just borrow them from the pattern (via substitution)!
  let alias0 = updateAliasing (alias env) pat
      alias' = foldl (\acc ((fpar,_),patel) ->
                        let patnm = patElemName patel
                            parnm = paramName fpar
                        in  case trace ("LOOPMEMCTX: "++pretty patnm++" "++pretty parnm) $ M.lookup patnm alias0 of
                                    Nothing -> acc
                                    Just al -> trace (" found alias set: "++pretty (S.toList al)) M.insert parnm al acc
                     ) (alias env) $ zip memctx $ patternContextElements pat
      -- ^ update the aliasing of the loop's memory-block context
      --   with the aliasing info borrowed from the pattern.

      (lvars, _) = unzip var_ses
      lvarmems =
        mapMaybe (\fpar -> case paramAttr fpar of
                             ExpMem.ArrayMem ptp shp _ mem_nm idxfun ->
                                 Just (paramName fpar, MemBlock ptp shp mem_nm idxfun)
                             _ -> Nothing
                 ) lvars
      v2mem' = M.union (v2mem env) $ M.fromList lvarmems
      -- ^ update the v2mem with the memory blocks of the loop vars.

      mems = S.fromList $ map (\(MemBlock _ _ mn _) -> mn) $ snd $ unzip lvarmems
      active' = S.union (active env) $ S.intersection (alloc env) $
                aliasTransClos alias' mems
      -- ^ add the alias-transitive closure of loop-vars mem blocks to active
      env' = intrfAnBdy lutab (env{v2mem = v2mem', active = active', alias = alias'}) body
  in  defInterference lutab env' pat

intrfAnBnd lutab env (Let pat _ _) =
  defInterference lutab env pat

defInterference :: LUTabFun -> IntrfEnv -> Pattern (Aliases ExpMem.ExplicitMemory)
                -> IntrfEnv
defInterference lutab env pat =
  let alias' = updateAliasing (alias env) pat
      -- ^ record the aliasing of the current statement

      arrmems= map (\(a,b,_)->(a,b)) $ getArrMemAssoc pat
      v2mem' = M.union (v2mem env) $ M.fromList arrmems
      -- ^ update v2mem with pattern's (array-var -> mem-block) bindings

      patmems = map (\(MemBlock _ _ mn _) -> mn) $ snd $ unzip arrmems
      intrf' = updateInterference alias' (alloc env) (active env) (intrf env) patmems
      -- ^ update interference: get the alias-transitive closure of each memory
      --   block in pattern and mark its interference with the current active set.

      lus    = fromMaybe S.empty (M.lookup (head $ patternNames pat) lutab)
      lumems = getLastUseMem v2mem' lus
      active1= S.difference (active env) lumems
      -- ^ get the memory blocks associated to the variables lastly used in this
      --   statement and subtract them from the active set.

      active'= S.union active1 $ S.intersection (alloc env) $
               aliasTransClos alias' $ S.fromList patmems
      -- ^ update the active set with the alias-transitive closure of this stmt
      --   memory blocks (keep only the allocated ones of course).

  in env { alias = alias', v2mem = v2mem', intrf = intrf', active = active' }

--             (Let pat _ (If _ then_body else_body _)) =
{--
intrfAnBnd lutab env (Let pat _ e) =
  let stab  = alias env
      stab' = foldl (\stabb patel->
                        -- compute the transitive closure of current pattern
                        -- name by concating all its aliases entries in stabb
                        let (al0,l1) = patElemAttr patel
                            al = case l1 of
                                   ExpMem.Scalar tp ->
                                     trace ("MemLore: "++(pretty (patElemName patel))++" is Scalar: "++pretty tp++" ("++pretty l1++") ") al0
                                   ExpMem.MemMem se sp ->
                                     trace ("MemLore: "++(pretty (patElemName patel))++" is MemMem: "++pretty se++" , "++pretty sp++" ("++pretty l1++") ") al0
                                   ExpMem.ArrayMem tp shp u nm indfun ->
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
  foldl (\itrf mm -> let m_al = case M.lookup mm alias0 of
                                  Just al -> S.insert mm al
                                  Nothing -> S.singleton mm
                         m_ad = S.intersection m_al alloc0
                     in  S.foldl' (\acc m -> case M.lookup m acc of
                                               Just mst-> M.insert m (mst `S.union` active0) acc
                                               Nothing -> M.insert m active0 acc
                                   ) itrf m_ad
        )

getLastUseMem :: V2MemTab -> Names -> Names
getLastUseMem v2mem0 =
  S.foldl' (\acc lu ->
                case M.lookup lu v2mem0 of
                  Nothing -> acc
                  Just (MemBlock _ _ m _)  -> S.insert m acc
            ) S.empty

----------------------------------------------------------------
--- Printer/Tester Main Program
----------------------------------------------------------------

{--

lastUseAnPrg :: ExpMem.Prog ExpMem.ExplicitMemory -> LUTabPrg
lastUseAnPrg prg = let aliased_prg = AnlAls.aliasAnalysis prg
                   in  M.fromList $ map lastUseAnFun $ progFunctions aliased_prg

--}

memoryBlockMerging :: ExpMem.Prog ExpMem.ExplicitMemory -> IO ()
memoryBlockMerging prg = do
  mapM_ lookAtFunction (progFunctions prg)

  let lutab   = lastUsePrg $ AnlAls.aliasAnalysis prg
      envtab  = intrfAnPrg lutab prg

  putStrLn "LAST_USE RESULT:"
  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concatMap (map (Control.Arrow.second S.toList) . M.toList) (M.elems lutab))

  putStrLn "ALLOCATIONS RESULT:"
  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concatMap (S.toList . alloc) (M.elems envtab))
--  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concat $ map (\env -> M.toList $ alloc env) $ M.elems envtab)

  putStrLn "ALIAS RESULT:"
  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concatMap (map (Control.Arrow.second S.toList) . M.toList . alias) (M.elems envtab))

  putStrLn "INTERFERENCE RESULT:"
  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concatMap (map (Control.Arrow.second S.toList) . M.toList . intrf) (M.elems envtab))

  let coaltab = mkCoalsTab $ AnlAls.aliasAnalysis prg
  putStrLn $ "COALESCING RESULT:" ++ pretty (length coaltab)
  let coal_info = map (\env ->
                            ( dstmem env, dstind env, S.toList $ alsmem env, M.toList $ optdeps env
                            , map (\ (k,Coalesced _ (MemBlock _ _ b indfun) sbst) ->
                                    (k,(b,indfun,M.toList sbst))
                                  ) $ M.toList $ vartab env
                            )
                      ) $ M.elems coaltab
  putStrLn $ unlines (map ("  "++) $ lines $ pretty coal_info)


lookAtFunction :: ExpMem.FunDef ExpMem.ExplicitMemory -> IO ()
lookAtFunction (ExpMem.FunDef _ fname _ params body) = do
  let ExpMem.Body () bnds res = body
  putStrLn $ "In Function: " ++ pretty fname ++ " with params " ++ pretty params
  mapM_ lookAtBinding bnds
  putStrLn $ "Result: " ++ pretty res
  where lookAtBinding (ExpMem.Let pat () e) = do
          putStrLn $ "The binding with pattern: " ++ pretty pat
          putStrLn $ "And corresponding expression:\n" ++
                     unlines (map ("  "++) $ lines $ pretty e)


--futhark --gpu --memory-playground ../futhark-benchmarks/accelerate/canny/canny.fut
