{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Playground for work on merging memory blocks
module Futhark.MemoryBlockMerging
       ( memoryBlockMerging )
       where

import Prelude
import Data.Maybe
import Control.Arrow
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

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

type IntrfTab = HM.HashMap VName Names

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
emptyInterfEnv = IntrfEnv { intrf = HM.empty, alloc = HS.empty
                          , alias = HM.empty, v2mem = HM.empty, active = HS.empty }

intrfAnPrg :: LUTabPrg -> ExpMem.Prog ExpMem.ExplicitMemory -> HM.HashMap Name IntrfEnv
intrfAnPrg lutab prg =
  let aliased_prg = AnlAls.aliasAnalysis prg
  in  HM.fromList $ map (intrfAnFun lutab) $ progFunctions aliased_prg


intrfAnFun :: LUTabPrg -> FunDef (Aliases ExpMem.ExplicitMemory) -> (Name,IntrfEnv)
intrfAnFun lutabprg (FunDef _ fname _ _ body) =
  let lutab = fromMaybe HM.empty (HM.lookup fname lutabprg)
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
            in  env { alloc = HS.insert nm' (alloc env)}

intrfAnBnd lutab env (Let pat _ (DoLoop memctx var_ses _ body)) =
  -- BUG!!! you need to handle the potential circular aliasing
  -- between loop-variant variables and their memory blocks;
  -- just borrow them from the pattern (via substitution)!
  let alias0 = updateAliasing (alias env) pat
      alias' = foldl (\acc ((fpar,_),patel) ->
                        let patnm = patElemName patel
                            parnm = paramName fpar
                        in  case trace ("LOOPMEMCTX: "++pretty patnm++" "++pretty parnm) $ HM.lookup patnm alias0 of
                                    Nothing -> acc
                                    Just al -> trace (" found alias set: "++pretty (HS.toList al)) HM.insert parnm al acc
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
      v2mem' = HM.union (v2mem env) $ HM.fromList lvarmems
      -- ^ update the v2mem with the memory blocks of the loop vars.

      mems = HS.fromList $ map (\(MemBlock _ _ mn _) -> mn) $ snd $ unzip lvarmems
      active' = HS.union (active env) $ HS.intersection (alloc env) $
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
      v2mem' = HM.union (v2mem env) $ HM.fromList arrmems
      -- ^ update v2mem with pattern's (array-var -> mem-block) bindings

      patmems = map (\(MemBlock _ _ mn _) -> mn) $ snd $ unzip arrmems
      intrf' = updateInterference alias' (alloc env) (active env) (intrf env) patmems
      -- ^ update interference: get the alias-transitive closure of each memory
      --   block in pattern and mark its interference with the current active set.

      lus    = fromMaybe HS.empty (HM.lookup (head $ patternNames pat) lutab)
      lumems = getLastUseMem v2mem' lus
      active1= HS.difference (active env) lumems
      -- ^ get the memory blocks associated to the variables lastly used in this
      --   statement and subtract them from the active set.

      active'= HS.union active1 $ HS.intersection (alloc env) $
               aliasTransClos alias' $ HS.fromList patmems
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
                            al_trns= HS.foldl' (\acc x -> case HM.lookup x stabb of
                                                            Nothing -> acc
                                                            Just aal -> HS.union acc aal
                                               ) al_nms al_nms
--al_trns' = trace ("AL Pattern: "++(pretty (patElemName patel))++" aliases: "++pretty (HS.toList al_trns)) al_trns
                        in  if null al_trns then stabb
                        else HM.insert (patElemName patel) al_trns stabb
                    ) stab $ patternValueElements pat -- patternContextElements pat
  in  env { alias = stab' }
--}


updateInterference :: AliasTab -> AllocTab -> Names -> IntrfTab -> [VName] -> IntrfTab
updateInterference alias0 alloc0 active0 =
  foldl (\itrf mm -> let m_al = case HM.lookup mm alias0 of
                                  Just al -> HS.insert mm al
                                  Nothing -> HS.singleton mm
                         m_ad = HS.intersection m_al alloc0
                     in  HS.foldl' (\acc m -> case HM.lookup m acc of
                                               Just mst-> HM.insert m (mst `HS.union` active0) acc
                                               Nothing -> HM.insert m active0 acc
                                   ) itrf m_ad
        )

getLastUseMem :: V2MemTab -> Names -> Names
getLastUseMem v2mem0 =
  HS.foldl' (\acc lu ->
                case HM.lookup lu v2mem0 of
                  Nothing -> acc
                  Just (MemBlock _ _ m _)  -> HS.insert m acc
            ) HS.empty

----------------------------------------------------------------
--- Printer/Tester Main Program
----------------------------------------------------------------

{--

lastUseAnPrg :: ExpMem.Prog ExpMem.ExplicitMemory -> LUTabPrg
lastUseAnPrg prg = let aliased_prg = AnlAls.aliasAnalysis prg
                   in  HM.fromList $ map lastUseAnFun $ progFunctions aliased_prg

--}

memoryBlockMerging :: ExpMem.Prog ExpMem.ExplicitMemory -> IO ()
memoryBlockMerging prg = do
  mapM_ lookAtFunction (progFunctions prg)

  let lutab   = lastUsePrg $ AnlAls.aliasAnalysis prg
      envtab  = intrfAnPrg lutab prg

  putStrLn "LAST_USE RESULT:"
  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concatMap (map (Control.Arrow.second HS.toList) . HM.toList) (HM.elems lutab))

  putStrLn "ALLOCATIONS RESULT:"
  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concatMap (HS.toList . alloc) (HM.elems envtab))
--  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concat $ map (\env -> HM.toList $ alloc env) $ HM.elems envtab)

  putStrLn "ALIAS RESULT:"
  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concatMap (map (Control.Arrow.second HS.toList) . HM.toList . alias) (HM.elems envtab))

  putStrLn "INTERFERENCE RESULT:"
  putStrLn $ unlines (map ("  "++) $ lines $ pretty $ concatMap (map (Control.Arrow.second HS.toList) . HM.toList . intrf) (HM.elems envtab))

  let coaltab = mkCoalsTab $ AnlAls.aliasAnalysis prg
  putStrLn $ "COALESCING RESULT:" ++ pretty (length coaltab)
  let coal_info = map (\env ->
                            ( dstmem env, dstind env, HS.toList $ alsmem env, HM.toList $ optdeps env
                            , map (\ (k,Coalesced _ (MemBlock _ _ b indfun) sbst) ->
                                    (k,(b,indfun,HM.toList sbst))
                                  ) $ HM.toList $ vartab env
                            )
                      ) $ HM.elems coaltab
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
