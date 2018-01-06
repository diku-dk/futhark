{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Transform a function based on a mapping from variable to memory and index
-- function: Change every variable in the mapping to its possibly new memory
-- block.
module Futhark.Optimise.MemoryBlockMerging.MemoryUpdater
  ( transformFromVarMemMappings
  ) where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Monad
import Control.Monad.Reader

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
       (ExplicitMemorish, ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Reuse.AllocationSizes (Sizes)


data Context = Context { ctxVarToMem :: VarMemMappings MemoryLoc
                       , ctxVarToMemOrig :: VarMemMappings MName
                       , ctxAllocSizes :: Sizes
                       , ctxAllocSizesOrig :: Sizes
                       }
  deriving (Show)

newtype FindM lore a = FindM { unFindM :: Reader Context a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context)

type LoreConstraints lore = (ExplicitMemorish lore,
                             FullMap lore,
                             BodyAttr lore ~ (),
                             ExpAttr lore ~ ())

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

-- | Transform a function to use new memory blocks.
transformFromVarMemMappings :: VarMemMappings MemoryLoc ->
                               VarMemMappings MName ->
                               Sizes -> Sizes ->
                               FunDef ExplicitMemory -> FunDef ExplicitMemory
transformFromVarMemMappings var_to_mem var_to_mem_orig alloc_sizes alloc_sizes_orig fundef =
  let m = unFindM $ transformFunDefBody $ funDefBody fundef
      ctx = Context { ctxVarToMem = var_to_mem
                    , ctxVarToMemOrig = var_to_mem_orig
                    , ctxAllocSizes = alloc_sizes
                    , ctxAllocSizesOrig = alloc_sizes_orig
                    }
      body' = runReader m ctx
  in fundef { funDefBody = body' }

transformFunDefBody :: LoreConstraints lore =>
                       Body lore -> FindM lore (Body lore)
transformFunDefBody (Body () bnds res) = do
  bnds' <- mapM transformStm $ stmsToList bnds
  res' <- transformFunDefBodyResult res
  return $ Body () (stmsFromList bnds') res'

transformFunDefBodyResult :: LoreConstraints lore =>
                             [SubExp] -> FindM lore [SubExp]
transformFunDefBodyResult ses = do
  var_to_mem <- asks ctxVarToMem
  var_to_mem_orig <- asks ctxVarToMemOrig
  mem_to_size <- asks ctxAllocSizes
  mem_to_size_orig <- asks ctxAllocSizesOrig

  let check se
        | Var v <- se
        , Just orig <- M.lookup v var_to_mem_orig
        , Just new <- memLocName <$> M.lookup v var_to_mem
        = (Var orig, (Var new, [])) : case (fst <$> M.lookup orig mem_to_size_orig,
                                            fst <$> M.lookup new mem_to_size) of
            (Just size_orig, Just size_new) ->
              [(size_orig, (size_new, [Var orig]))]
            _ -> []
        | otherwise = []

      check_size_only se
        | Var v <- se
        , Just orig <- fst <$> M.lookup v mem_to_size_orig
        , Just new <- fst <$> M.lookup v mem_to_size
        , orig /= new
        = [(orig, (new, [Var v]))]
        | otherwise = []
      mem_orig_to_new1 = concatMap check ses
      mem_orig_to_new2 = concatMap check_size_only ses
      mem_orig_to_new = mem_orig_to_new1 ++ mem_orig_to_new2

  let debug =
        putBlock [ "memory updater"
                 , show var_to_mem
                 , show var_to_mem_orig
                 , show mem_to_size
                 , show mem_to_size_orig
                 , ""
                 , show mem_orig_to_new1
                 , show mem_orig_to_new2
                 ]
  withDebug debug $ return $ zipWith (
    \se ts -> fromMaybe se (do
                               (se', reqts) <- se `L.lookup` mem_orig_to_new
                               if reqts == take (length reqts) ts
                                 then return se'
                                 else Nothing
                           )
    ) ses (L.tail $ L.tails ses)

transformBody :: LoreConstraints lore =>
                       Body lore -> FindM lore (Body lore)
transformBody (Body () bnds res) = do
  bnds' <- mapM transformStm $ stmsToList bnds
  return $ Body () (stmsFromList bnds') res

transformKernelBody :: LoreConstraints lore =>
                       KernelBody lore -> FindM lore (KernelBody lore)
transformKernelBody (KernelBody () bnds res) = do
  bnds' <- mapM transformStm $ stmsToList bnds
  return $ KernelBody () (stmsFromList bnds') res

transformMemInfo :: ExpMem.MemInfo d u ExpMem.MemReturn -> MemoryLoc ->
                    ExpMem.MemInfo d u ExpMem.MemReturn
transformMemInfo meminfo memloc = case meminfo of
  ExpMem.MemArray pt shape u _memreturn ->
    let extixfun = ExpMem.existentialiseIxFun [] $ memLocIxFun memloc
    in ExpMem.MemArray pt shape u
       (ExpMem.ReturnsInBlock (memLocName memloc) extixfun)
  _ -> meminfo

transformStm :: LoreConstraints lore =>
                Stm lore -> FindM lore (Stm lore)
transformStm (Let (Pattern patctxelems patvalelems) aux e) = do
  patvalelems' <- mapM transformPatValElem patvalelems

  e' <- fullMapExpM mapper mapper_kernel e
  var_to_mem <- asks ctxVarToMem
  e'' <- case e' of
    If cond body_then body_else (IfAttr rets sort) -> do
      let bodyVarMemLocs body =
            map (flip M.lookup var_to_mem <=< fromVar)
            $ drop (length patctxelems) $ bodyResult body

          -- FIXME: This is a mess.  We try to "reverse-engineer" the origin of
          -- how the If results came to look as they do, so that we can produce
          -- a correct IfAttr.
          findBodyResMem i body_results =
            let imem = patElemName (patctxelems L.!! i)
                matching_var = mapMaybe (
                  \(p, p_i) ->
                    case patElemAttr p of
                      ExpMem.MemArray _ _ _ (ExpMem.ArrayIn vmem _) ->
                        if imem == vmem
                        then Just p_i
                        else Nothing
                      _ ->
                        Nothing
                  ) (zip patvalelems [0..])
            in do
              j <- case matching_var of
                [t] -> Just t
                _ -> Nothing
              body_res_var <- fromVar (body_results L.!! (length patctxelems + j))
              MemoryLoc mem _ixfun <- M.lookup body_res_var var_to_mem
              return mem

          fixBodyExistentials body =
            body { bodyResult =
                   zipWith (\res i -> if i < length patctxelems
                                      then maybe res Var $ findBodyResMem i (bodyResult body)
                                      else res)
                   (bodyResult body) [0..] }

      let ms_then = bodyVarMemLocs body_then
          ms_else = bodyVarMemLocs body_else

      let body_then' = fixBodyExistentials body_then
          body_else' = fixBodyExistentials body_else

      let rets_new =
            if ms_then == ms_else
            then zipWith (\r m -> case m of
                                    Nothing -> r
                                    Just m' ->
                                      transformMemInfo r m'
                         ) rets ms_then
            else rets

      let debug = putBlock [ "ifattr rets: " ++ show rets
                           , "ifattr rets_new: " ++ show rets_new
                           , "ifattr ms_then: " ++ show ms_then
                           , "ifattr ms_else: " ++ show ms_else
                           ]
      withDebug debug $ return $ If cond body_then' body_else' (IfAttr rets_new sort)

    DoLoop mergectxparams mergevalparams loopform body -> do
      -- More special loop handling because of its extra
      -- pattern-like info.
      mergectxparams' <- mapM (transformMergeCtxParam mergevalparams) mergectxparams
      mergevalparams' <- mapM transformMergeValParam mergevalparams

      -- The body of a loop can return a memory block in its results.  This is
      -- the memory block used by a variable which is also part of the results.
      -- If the memory block of that variable is changed, we need a way to
      -- record that the memory block in the body result also needs to change.
      let zipped = zip [(0::Int)..] (patctxelems ++ patvalelems)

          findMemLinks (i, PatElem _x (ExpMem.MemArray _ _ _ (ExpMem.ArrayIn xmem _))) =
            case L.find (\(_, PatElem ymem _) -> ymem == xmem) zipped of
              Just (j, _) -> Just (j, i)
              Nothing -> Nothing
          findMemLinks _ = Nothing

          mem_links = mapMaybe findMemLinks zipped

          res = bodyResult body

          fixResRecord i se
            | Var _mem <- se
            , Just j <- L.lookup i mem_links
            , Var related_var <- res L.!! j
            , Just mem_new <- M.lookup related_var var_to_mem =
                Var $ memLocName mem_new
            | otherwise = se

          res' = zipWith fixResRecord [(0::Int)..] res
          body' = body { bodyResult = res' }

      loopform' <- case loopform of
        ForLoop i it bound loop_vars -> do
          loop_vars' <- mapM transformForLoopVar loop_vars
          return $ ForLoop i it bound loop_vars'
        WhileLoop _ -> return loopform
      return $ DoLoop mergectxparams' mergevalparams' loopform' body'
    _ -> return e'
  return $ Let (Pattern patctxelems patvalelems') aux e''
  where mapper = identityMapper
          { mapOnBody = const transformBody
          , mapOnFParam = transformFParam
          , mapOnLParam = transformLParam
          }
        mapper_kernel = identityKernelMapper
          { mapOnKernelBody = coerce . transformBody
          , mapOnKernelKernelBody = coerce . transformKernelBody
          , mapOnKernelLambda = coerce . transformLambda
          , mapOnKernelLParam = transformLParam
          }



-- Update the actual memory block referred to by a context (existential) memory
-- block in a loop.
transformMergeCtxParam :: LoreConstraints lore =>
                          [(FParam ExplicitMemory, SubExp)] ->
                          (FParam ExplicitMemory, SubExp)
                       -> FindM lore (FParam ExplicitMemory, SubExp)
transformMergeCtxParam mergevalparams (param@(Param ctxmem ExpMem.MemMem{}), mem) = do
  var_to_mem <- asks ctxVarToMem

  let usesCtxMem (Param _ (ExpMem.MemArray _ _ _ (ExpMem.ArrayIn pmem _))) = ctxmem == pmem
      usesCtxMem _ = False

      -- If the initial value of a loop merge parameter is a memory block name,
      -- we may have to update that.  If the context memory block is used in an
      -- array in one of the value merge parameters, see if that array variable
      -- refers to an array that has been set to reuse a memory block.
      mem' = fromMaybe mem $ do
        (_, Var orig_var) <- L.find (usesCtxMem . fst) mergevalparams
        orig_mem <- M.lookup orig_var var_to_mem
        return $ Var $ memLocName orig_mem
  return (param, mem')
transformMergeCtxParam _ t = return t

transformMergeValParam :: LoreConstraints lore =>
                          (FParam ExplicitMemory, SubExp)
                       -> FindM lore (FParam ExplicitMemory, SubExp)
transformMergeValParam (Param x membound, se) = do
  membound' <- newMemBound membound x
  return (Param x membound', se)

transformPatValElem :: LoreConstraints lore =>
                       PatElem ExplicitMemory -> FindM lore (PatElem ExplicitMemory)
transformPatValElem (PatElem x membound) = do
  membound' <- newMemBound membound x
  return $ PatElem x membound'

transformFParam :: LoreConstraints lore =>
                   FParam lore -> FindM lore (FParam lore)
transformFParam (Param x membound) = do
  membound' <- newMemBound membound x
  return $ Param x membound'

transformLParam :: LoreConstraints lore =>
                   LParam lore -> FindM lore (LParam lore)
transformLParam (Param x membound) = do
  membound' <- newMemBound membound x
  return $ Param x membound'

transformLambda :: LoreConstraints lore =>
                   Lambda lore -> FindM lore (Lambda lore)
transformLambda (Lambda params body types) = do
  params' <- mapM transformLParam params
  body' <- transformBody body
  return $ Lambda params' body' types

transformForLoopVar :: LoreConstraints lore =>
                       (LParam lore, VName) ->
                       FindM lore (LParam lore, VName)
transformForLoopVar (Param x membound, array) = do
  membound' <- newMemBound membound x
  return (Param x membound', array)

-- Find a new memory block and index function if they exist.
newMemBound :: ExpMem.MemBound u -> VName -> FindM lore (ExpMem.MemBound u)
newMemBound membound var = do
  var_to_mem <- asks ctxVarToMem

  let membound'
        | ExpMem.MemArray pt shape u _ <- membound
        , Just (MemoryLoc mem ixfun) <- M.lookup var var_to_mem =
            Just $ ExpMem.MemArray pt shape u $ ExpMem.ArrayIn mem ixfun
        | otherwise = Nothing

  return $ fromMaybe membound membound'
