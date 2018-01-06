-- | Move allocation statements upwards in the bodies of a program to enable
-- more memory block coalescings.
--
-- This should be run *before* the coalescing pass, as it enables more
-- optimisations.
module Futhark.Optimise.MemoryBlockMerging.Coalescing.AllocationHoisting
  ( hoistAllocsFunDef
  ) where

import qualified Data.Set as S
import Data.Maybe (mapMaybe)

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.CrudeHoisting


findAllocHoistees :: Body ExplicitMemory -> Maybe [FParam ExplicitMemory]
                  -> [VName]
findAllocHoistees body params =
  let all_found = mapMaybe findThemStm stms
                  ++ maybe [] (mapMaybe findThemFParam) params
      extras = concatMap snd all_found
      allocs = map fst all_found
      -- We must hoist the alloc expressions in the end.  If we hoist an alloc
      -- before we hoist one of its array creations (in case of in-place
      -- updates), that array creation might in turn hoist something depending
      -- on another memory block mem_y further up than the allocation of memory
      -- block mem_x.  This will become a problem if mem_y can get coalesced
      -- into mem_x.
      --
      -- Maybe there is a nicer way to guarantee that this does not happen, but
      -- this seems to work for now.
      --
      -- We reverse the non-alloc dependencies to ensure (sloppily) that they do
      -- not change positions internally compared to the original program: For
      -- example, if a statement x is located before a statement y, and both x
      -- and y need to be hoisted, then we need to hoist x in the end, so that
      -- it can be hoisted further than y, which might have been hoisted to
      -- before x.  A better solution is welcome!
      in reverse extras ++ reverse allocs

  where stms :: [Stm ExplicitMemory]
        stms = stmsToList $ bodyStms body

        findThemStm :: Stm ExplicitMemory -> Maybe (VName, [VName])
        findThemStm (Let (Pattern _ [PatElem xmem _]) _ (Op ExpMem.Alloc{})) =
          usedByCopyOrConcat xmem
        findThemStm _ = Nothing

        -- A function paramater can be a unique memory block.  While we cannot
        -- hoist that, we may have to hoist an index in an in-place update that
        -- uses the memory.
        findThemFParam :: FParam ExplicitMemory -> Maybe (VName, [VName])
        findThemFParam (Param xmem ExpMem.MemMem{}) = usedByCopyOrConcat xmem
        findThemFParam _ = Nothing

        -- Is the allocated memory used by either Copy or Concat in the function
        -- body?  Those are the only kinds of memory we care about, since those
        -- are the cases handled by coalescing.  Also find the names used by
        -- in-place updates, since those also need to be hoisted (as an example
        -- of this, consider the 'copy/pos1.fut' test where the replicate
        -- expression needs to be hoisted as well as its memory allocation).
        usedByCopyOrConcat :: VName -> Maybe (VName, [VName])
        usedByCopyOrConcat xmem_alloc =
          let vs = mapMaybe checkStm stms
              vs' = if null vs then Nothing else Just (xmem_alloc, concat vs)

              debug =
                putBlock [ "usedByCopyOrConcat:"
                         , "xmem_alloc: " ++ pretty xmem_alloc
                         , "vars: " ++ prettySet (S.fromList vs)
                         , "vars': " ++ show vs'
                         ]

          in withDebug debug vs'

          where checkStm :: Stm ExplicitMemory -> Maybe [VName]
                checkStm (Let
                          (Pattern _
                           [PatElem _ (ExpMem.MemArray _ _ _ (ExpMem.ArrayIn xmem_pat _))])
                           _
                           (BasicOp bop))
                  | xmem_pat == xmem_alloc =
                    case bop of
                      Update v slice _ ->
                        -- The source array must also be hoisted so that it
                        -- is initialized before it is used by the
                        -- coalesced party.  Any index variables are also
                        -- hoisted.
                        Just $ v : S.toList (freeIn slice)
                      Copy{} -> Just []
                      Concat{} -> Just []
                      _ -> Nothing
                checkStm _ = Nothing

hoistAllocsFunDef :: FunDef ExplicitMemory
                  -> FunDef ExplicitMemory
hoistAllocsFunDef fundef =
  hoistInFunDef fundef findAllocHoistees
