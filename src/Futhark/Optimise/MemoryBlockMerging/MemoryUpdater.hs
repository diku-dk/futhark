{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | Transform a function based on a mapping from variable to memory and index
-- function: Change every variable in the mapping to its possibly new memory
-- block.
module Futhark.Optimise.MemoryBlockMerging.MemoryUpdater
  ( transformFromVarMemMappings
  ) where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Monad
import Control.Monad.RWS

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
       (ExplicitMemorish, ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.Miscellaneous


data Context = Context { ctxVarToMem :: VarMemMappings MemoryLoc
                       , ctxVarToMemOrig :: VarMemMappings MName
                       , ctxAllocSizes :: M.Map MName SubExp
                       , ctxAllocSizesOrig :: M.Map MName SubExp
                       , ctxHasMaxedSize :: Bool
                       }
  deriving (Show)

newtype FindM lore a = FindM { unFindM :: RWS Context () (VNameSource, [(MName, VName)]) a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context, MonadState (VNameSource, [(MName, VName)]))

instance MonadFreshNames (FindM lore) where
  getNameSource = gets fst
  putNameSource s = modify $ \(_, m) -> (s, m)

modifyMemSizeMapping :: ([(MName, VName)] -> [(MName, VName)]) -> FindM lore ()
modifyMemSizeMapping f = modify $ second f

type LoreConstraints lore = (ExplicitMemorish lore,
                             FullMap lore,
                             BodyAttr lore ~ (),
                             ExpAttr lore ~ ())

coerce :: (ExplicitMemorish flore, ExplicitMemorish tlore) =>
          FindM flore a -> FindM tlore a
coerce = FindM . unFindM

-- | Transform a function to use new memory blocks.
transformFromVarMemMappings :: MonadFreshNames m =>
                               VarMemMappings MemoryLoc ->
                               VarMemMappings MName ->
                               M.Map MName SubExp -> M.Map MName SubExp -> Bool ->
                               FunDef ExplicitMemory ->
                               m (FunDef ExplicitMemory)
transformFromVarMemMappings var_to_mem var_to_mem_orig alloc_sizes alloc_sizes_orig has_maxed_size fundef =
  let m = unFindM $ transformFunDefBody $ funDefBody fundef
      ctx = Context { ctxVarToMem = var_to_mem
                    , ctxVarToMemOrig = var_to_mem_orig
                    , ctxAllocSizes = alloc_sizes
                    , ctxAllocSizesOrig = alloc_sizes_orig
                    , ctxHasMaxedSize = has_maxed_size
                    }
  in modifyNameSource (\src ->
                         let (body', (src', _), ()) = runRWS m ctx (src, [])
                         in (fundef { funDefBody = body' }, src')
                      )

transformFunDefBody :: LoreConstraints lore =>
                       Body lore -> FindM lore (Body lore)
transformFunDefBody (Body () bnds res) = do
  bnds' <- mapM transformStm $ stmsToList bnds
  res' <- transformFunDefBodyResult res
  return $ Body () (stmsFromList bnds') res'

transformFunDefBodyResult :: LoreConstraints lore =>
                             [SubExp] -> FindM lore [SubExp]
transformFunDefBodyResult ses = do
  var_to_mem_orig <- asks ctxVarToMemOrig
  var_to_mem <- asks ctxVarToMem
  mem_to_size_orig <- asks ctxAllocSizesOrig
  mem_to_size <- asks ctxAllocSizes
  mem_to_new_size <- gets snd

  let check se
        | Var v <- se
        , Just orig <- M.lookup v var_to_mem_orig
        , Just new <- memLocName <$> M.lookup v var_to_mem
        = ((Var orig, Nothing), Var new) : case (M.lookup orig mem_to_size_orig,
                                                 (Var <$> L.lookup new mem_to_new_size) <|> M.lookup new mem_to_size) of
            (Just size_orig, Just size_new) ->
              [((size_orig, Just (Var orig)), size_new)]
            _ -> []
        | otherwise = []

      check_size_only se
        | Var v <- se
        , Just orig <- M.lookup v mem_to_size_orig
        , Just new <- (Var <$> L.lookup v mem_to_new_size) <|> M.lookup v mem_to_size
        , orig /= new
        = [((orig, Just (Var v)), new)]
        | otherwise = []
      mem_orig_to_new1 = concatMap check ses
      mem_orig_to_new2 = concatMap check_size_only ses
      mem_orig_to_new = mem_orig_to_new1 ++ mem_orig_to_new2

  return $ zipWith (
    \se ts -> fromMaybe se (
      -- FIXME: This assumes that a memory block always
      -- comes just after its size variable.  We ought
      -- to instead properly find this information from
      -- the funDefRetType 'ExtSize's.
      (se, Nothing) `L.lookup` mem_orig_to_new
        <|> case ts of
              (ts0 : _) ->
                (se, Just ts0) `L.lookup` mem_orig_to_new
              _ -> Nothing
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

data BranchReturn = ExistingBranchReturn ExpMem.BodyReturns
                  | NewBranchReturn (Int -> ExpMem.BodyReturns)
                    VName VName VName

transformStm :: LoreConstraints lore =>
                Stm lore -> FindM lore (Stm lore)
transformStm (Let (Pattern patctxelems patvalelems) aux e) = do
  patvalelems' <- mapM transformPatValElem patvalelems

  e' <- fullMapExpM mapper mapper_kernel e
  var_to_mem <- asks ctxVarToMem
  var_to_mem_orig <- asks ctxVarToMemOrig
  mem_to_size <- asks ctxAllocSizes
  mem_to_new_size <- gets snd
  (e'', patctxelems') <- case e' of
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

      -- Fix values.
      let rets' =
            if ms_then == ms_else
            then zipWith (\r m -> case m of
                                    Nothing -> r
                                    Just m' ->
                                      transformMemInfo r m'
                         ) rets ms_then
            else rets

      let body_then' = fixBodyExistentials body_then
          body_else' = fixBodyExistentials body_else


      -- Fix existential memory blocks.
      let mem_size mem = L.lookup mem mem_to_new_size <|> (fromVar =<< M.lookup mem mem_to_size)
          v_size v = do
            mem <- M.lookup v (M.map memLocName var_to_mem) <|> M.lookup v var_to_mem_orig
            mem_size mem

      has_maxed_size <- asks ctxHasMaxedSize
      let rets_branch_returns =
            L.zipWith4 (\r pat th el -> case (r, pat, th, el) of
                           (ExpMem.MemArray pt shape u
                            (ExpMem.ReturnsNewBlock space n
                             (Free (Var _size)) extixfun),
                            PatElem _
                            (ExpMem.MemArray _ _ _
                             (ExpMem.ArrayIn patmem _)),
                            Var v_th, Var v_el) ->
                             case (v_size v_th, v_size v_el) of
                               (Just s_th, Just s_el) ->
                                 if not has_maxed_size --s_th == s_el || not has_maxed_size
                                 then ExistingBranchReturn r
                                 else NewBranchReturn
                                      (\nth_ctxelem ->
                                         ExpMem.MemArray pt shape u
                                         (ExpMem.ReturnsNewBlock space n
                                          (Ext nth_ctxelem) extixfun))
                                      s_th s_el patmem
                               _ -> error ("both branch return arrays should use a memory block with a size: " ++ show v_th ++ " and " ++ show v_el)
                           _ -> ExistingBranchReturn r
                       )
            rets'
            patvalelems
            (drop (length patctxelems) (bodyResult body_then'))
            (drop (length patctxelems) (bodyResult body_else'))

      patctxelems_new <-
        replicateM
        (length (filter (\case
                            NewBranchReturn{} -> True
                            ExistingBranchReturn{} -> False
                        ) rets_branch_returns))
        (newVName "new_memory_size")
      let (rets'', _, body_ext_new, _, patmem_to_new_size) =
            foldl (\(prev, i, ext, patctxelems_new', mapping) rb -> case rb of
                               ExistingBranchReturn r ->
                                 (prev ++ [r], i, ext, patctxelems_new', mapping)
                               NewBranchReturn rf s_th s_el patmem ->
                                 (prev ++ [rf i], i + 1, ext ++ [(s_th, s_el)],
                                  tail patctxelems_new',
                                  mapping ++ [(patmem, head patctxelems_new')])
                           ) ([], length patctxelems, [], patctxelems_new, []) rets_branch_returns
      modifyMemSizeMapping (++ patmem_to_new_size)
      let (th_ext_new, el_ext_new) = unzip body_ext_new
          body_then'' = body_then' { bodyResult =
                                       take (length patctxelems) (bodyResult body_then') ++
                                       map Var th_ext_new ++
                                       drop (length patctxelems) (bodyResult body_then')
                                   }
          body_else'' = body_else' { bodyResult =
                                       take (length patctxelems) (bodyResult body_else') ++
                                       map Var el_ext_new ++
                                       drop (length patctxelems) (bodyResult body_else')
                                   }
          patctxelems_replaced = map (\pe -> case pe of
                                         PatElem name (ExpMem.MemMem _size space) ->
                                           case L.lookup name patmem_to_new_size of
                                             Just size_new ->
                                               PatElem name (ExpMem.MemMem (Var size_new) space)
                                             Nothing -> pe
                                         _ -> pe
                                     ) patctxelems
          patctxelems' = patctxelems_replaced ++ map (\v -> PatElem v (ExpMem.MemPrim (IntType Int64))) patctxelems_new

      return (If cond body_then'' body_else'' (IfAttr rets'' sort),
              patctxelems')

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
      return (DoLoop mergectxparams' mergevalparams' loopform' body',
              patctxelems)
    _ -> return (e', patctxelems)
  return (Let (Pattern patctxelems' patvalelems') aux e'')
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
