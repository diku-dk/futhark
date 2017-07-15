{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Find the actual variables that need updating when a variable attribute
-- needs updating.
module Futhark.Optimise.MemoryBlockMerging.ActualVariables
  ( findActualVariables
  ) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe (mapMaybe, maybeToList)
import Control.Monad
import Control.Monad.RWS

import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory (ExplicitMemory)
import qualified Futhark.Representation.ExplicitMemory as ExpMem

import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Types

-- Apart from the obvious benefits of keeping the calculation of this in a
-- separate module, it also makes it easier to find variables that occur later
-- in a body but uses the same memory block as the source variable, e.g. if a
-- rearrange or a reshape.


type ActualVariables = M.Map VName Names

getFullMap :: [M.Map VName Names] -> M.Map VName Names
getFullMap = M.unionsWith S.union

data Context = Context { ctxFirstUses :: FirstUses
                       , ctxVarToMem ::  VarMemMappings MemorySrc }
  deriving (Show)

newtype FindM a = FindM { unFindM :: RWS Context [ActualVariables] Names a }
  deriving (Monad, Functor, Applicative,
            MonadReader Context,
            MonadWriter [ActualVariables],
            MonadState Names)

recordActuals :: VName -> Names -> FindM ()
recordActuals stmt_var more_actuals = tell [M.singleton stmt_var more_actuals]

findActualVariables :: FirstUses -> VarMemMappings MemorySrc
                    -> FunDef ExplicitMemory -> (ActualVariables, Names)
findActualVariables first_uses var_mem_mappings fundef =
  let context = Context first_uses var_mem_mappings
      m = unFindM $ lookInBody $ funDefBody fundef
      res = execRWS m context S.empty
      actual_variables = getFullMap $ snd res
      existentials = fst res

      debug = do
        putStrLn $ replicate 70 '='
        putStrLn "all actual vars found"
        print actual_variables
        print existentials
        putStrLn $ replicate 70 '='
  in withDebug debug (actual_variables, existentials)

lookInBody :: Body ExplicitMemory -> FindM ()
lookInBody (Body _ bnds _res) =
  mapM_ lookInStm bnds

lookInStm :: Stm ExplicitMemory -> FindM ()
lookInStm (Let (Pattern patctxelems patvalelems) _ e) = do
  forM_ patvalelems $ \(PatElem var bindage _) ->
    case bindage of
      BindInPlace _ orig _ -> do
        -- Record that when coalescing an in-place update statement, also look
        -- at the original array.
        let actuals = S.fromList [var, orig]
        recordActuals var actuals
      _ -> return ()

  -- Special handling of loops, ifs, etc.
  case e of
    DoLoop _mergectxparams mergevalparams _loopform body ->
      forM_ patvalelems $ \(PatElem var _ membound) ->
        case membound of
          ExpMem.ArrayMem _ _ _ mem _ -> do
            -- If mem is existential, we need to find the return memory that it
            -- refers to.  We cannot just look at its memory aliases, since it
            -- likely aliases both the initial memory and the final memory.
            let zipped = zip patctxelems (bodyResult body)
            (var_actual, mem_search) <- case L.find ((== mem) . patElemName . fst) zipped of
              Just (_, Var res_mem) ->
                return (Nothing, res_mem)
              _ -> return (Just var, mem)
            let body_vars0 = mapMaybe (fromVar . snd) mergevalparams
                body_vars1 = map (paramName . fst) mergevalparams
                body_vars2 = concatMap (map patElemName
                                        . patternValueElements . stmPattern)
                             $ bodyStms body
                body_vars = body_vars0 ++ body_vars1 ++ body_vars2
            -- Find the ones using the same memory as the result of the loop
            -- expression.
            body_vars' <- filterM (\v -> do
                                      m <- M.lookup v <$> asks ctxVarToMem
                                      return $ Just mem_search == (memSrcName <$> m)
                                  ) body_vars
            -- Not only the result variable needs to change its memory block in case
            -- of a future coalescing with it; also the variables extracted above.
            -- FIXME: This is probably an okay way to do it?  What if the memory is
            -- used, but with a different index function?  Can that happen?
            let actuals = S.fromList (maybeToList var_actual ++ body_vars')
            recordActuals var actuals

            let debug = do
                  putStrLn "UUUUUU"
                  print var
                  print (var_actual, mem_search)
                  print actuals
                  putStrLn "UUUUUU"
            withDebug debug $ return ()

            -- If you extend this loop handling, make sure not to target existential
            -- memory blocks.  We want those to stay.
          _ -> return ()

    If _se body_then body_else _types ->
      -- We don't want to coalesce the existiential memory block of the if.
      -- However, if a branch result has a memory block that is firstly used
      -- inside the branch, it is okay to coalesce that in a future statement.
      forM_ (zip3 patvalelems (bodyResult body_then) (bodyResult body_else))
        $ \(PatElem var _ membound, res_then, res_else) ->
        case membound of
          ExpMem.ArrayMem _ _ _ mem _ ->
            if mem `L.elem` map patElemName patctxelems
              then do
              modify $ S.insert var -- existentials, make clearer

              var_to_mem <- asks ctxVarToMem
              first_uses_all <- asks ctxFirstUses
              let ifBlocks body se = do
                    res <- fromVar se
                    res_mem <- memSrcName <$> M.lookup res var_to_mem
                    let body_vars = concatMap (map patElemName . patternValueElements
                                               . stmPattern)
                                    $ bodyStms body
                        body_first_uses = S.unions $ map (`lookupEmptyable` first_uses_all)
                                          body_vars
                    Just $ return (S.member res_mem body_first_uses, res)

              case (ifBlocks body_then res_then,
                    ifBlocks body_else res_else) of
                (Just th, Just el) -> do -- prettify this stuff
                  (alloc_inside_then, var_then) <- th
                  (alloc_inside_else, var_else) <- el
                  when (alloc_inside_then || alloc_inside_else) $ do
                    let actuals = S.fromList [var, var_then, var_else]
                    recordActuals var actuals
                _ -> return ()

              else do
              -- If the memory block is not existential, we need to find all the
              -- variables in any sub-bodies using the same memory block.
              let varFromStm stm = map patElemName (patternValueElements $ stmPattern stm)
                                 ++ foldExp folder [] (stmExp stm)
                  folder = identityFolder { foldOnBody = \vs fbody -> return (vs ++ concatMap varFromStm (bodyStms fbody)) }
                  body_vars = foldExp folder [] e -- Get all statements in the
                                                  -- body *and* any sub-bodies,
                                                  -- etc.
              -- Find the ones using the same memory as the result of the loop
              -- expression.
              body_vars' <- filterM (\v -> do
                                        m <- M.lookup v <$> asks ctxVarToMem
                                        return $ Just mem == (memSrcName <$> m)
                                    ) body_vars
              -- Not only the result variable needs to change its memory block in case
              -- of a future coalescing with it; also the variables extracted above.
              -- FIXME: This is probably an okay way to do it?  What if the memory is
              -- used, but with a different index function?  Can that happen?
              let actuals = S.fromList (var : body_vars')
              recordActuals var actuals

              let debug = actuals `seq` do
                    putStrLn "AAAAAAAAAAAAAAAAA"
                    print var
                    print actuals
                    putStrLn "AAAAAAAAAAAAAAAAA"
              withDebug debug $ return ()

          _ -> return ()

    BasicOp (Reshape _ _ src0) -> do
      let var = patElemName $ head patvalelems -- probably ok
      -- Also refer to the array that it's reshaping.

      let actuals = S.fromList [var, src0]
      recordActuals var actuals

    --
    BasicOp (Rearrange _ _ orig) ->
      forM_ (map patElemName patvalelems) $ \var ->
        let debug = do
              putStrLn "AAAAAAAAAAAAAAAAA"
              print (var, orig)
              putStrLn "AAAAAAAAAAAAAAAAA"
        in withDebug debug $ do
          recordActuals var S.empty -- disable its use for now
          recordActuals orig (S.singleton var)

    _ -> return ()


  walkExpM walker e
  where walker = identityWalker { walkOnBody = lookInBody }
