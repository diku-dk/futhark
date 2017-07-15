module Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizes where


import qualified Data.Map.Strict as M

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem


type Sizes = M.Map VName SubExp

memBlockSizes :: FunDef ExplicitMemory -> Sizes
memBlockSizes fundef = M.union fromParams fromBody
  where fromParams = M.fromList $ concatMap onParam $ funDefParams fundef
        onParam (Param mem (ExpMem.MemMem size _space)) = [(mem, size)]
        onParam _ = []

        fromBody = M.fromList $ onBody $ funDefBody fundef
        onBody = concatMap onStm . bodyStms
        onStm (Let (Pattern _ [PatElem mem _ _]) ()
               (Op (ExpMem.Alloc size _))) = [(mem, size)]
        onStm (Let (Pattern patctxelems _) () e) =
          concatMap onPatCtxElem patctxelems ++ foldExp folder [] e
        onPatCtxElem (PatElem mem _ (ExpMem.MemMem size _)) = [(mem, size)]
        onPatCtxElem _ = []
        folder = identityFolder
          { foldOnBody = \sizes body -> return (sizes ++ onBody body)

          -- Sizes found from the functions below are scope-local, but that does
          -- not matter; we want all sizes so that we can lookup anything.
          , foldOnFParam = \sizes fparam -> return (sizes ++ onParam fparam)
          , foldOnLParam = \sizes lparam -> return (sizes ++ onParam lparam)
          }
