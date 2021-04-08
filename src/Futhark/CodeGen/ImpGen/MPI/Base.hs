{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Futhark.CodeGen.ImpGen.MPI.Base
  ( Env (..),
    MPIGen,
    getReturnParams,
    segOpString,
    freeParams,
    extractAllocations,
    getIterationDomain,
    gather,
  )
where

import Control.Monad
import Data.Bifunctor
import qualified Futhark.CodeGen.ImpCode.MPI as Imp
import Futhark.CodeGen.ImpGen
import Futhark.IR.MCMem
import Prelude hiding (quot, rem)

data Env = Env

type MPIGen = ImpM MCMem Env Imp.MPIOp

segOpString :: SegOp () MCMem -> MPIGen String
segOpString SegMap {} = return "segmap"
segOpString SegRed {} = return "segred"
segOpString SegScan {} = return "segscan"
segOpString SegHist {} = return "seghist"

getReturnParams :: Pattern MCMem -> SegOp () MCMem -> MPIGen [Imp.Param]
getReturnParams _ _ = return mempty

freeVariables :: Imp.Code -> [VName] -> [VName]
freeVariables code names =
  namesToList $ freeIn code `namesSubtract` namesFromList names

toParam :: VName -> TypeBase shape u -> MPIGen Imp.Param
toParam name (Prim pt) = return $ Imp.ScalarParam name pt
toParam name (Mem space) = return $ Imp.MemParam name space
toParam name Array {} = do
  name_entry <- lookupVar name
  case name_entry of
    ArrayVar _ (ArrayEntry (MemLocation mem _ _) _) ->
      return $ Imp.MemParam mem DefaultSpace
    _ -> error $ "[toParam] Could not handle array for " ++ show name
toParam name Acc {} = error $ "toParam Acc: " ++ pretty name

freeParams :: Imp.Code -> [VName] -> MPIGen [Imp.Param]
freeParams code names = do
  let freeVars = freeVariables code names
  ts <- mapM lookupType freeVars
  zipWithM toParam freeVars ts

-- | Try to extract invariant allocations.  If we assume that the
-- given 'Code' is the body of a 'SegOp', then it is always safe to
-- move the immediate allocations to the prebody.
extractAllocations :: Imp.Code -> (Imp.Code, Imp.Code)
extractAllocations segop_code = f segop_code
  where
    declared = Imp.declaredIn segop_code
    f (Imp.DeclareMem name space) =
      -- Hoisting declarations out is always safe.
      (Imp.DeclareMem name space, mempty)
    f (Imp.Allocate name size space)
      | not $ freeIn size `namesIntersect` declared =
        (Imp.Allocate name size space, mempty)
    f (x Imp.:>>: y) = f x <> f y
    f (Imp.While cond body) =
      (mempty, Imp.While cond body)
    f (Imp.For i bound body) =
      (mempty, Imp.For i bound body)
    f (Imp.Comment s code) =
      second (Imp.Comment s) (f code)
    f Imp.Free {} =
      mempty
    f (Imp.If cond tcode fcode) =
      let (ta, tcode') = f tcode
          (fa, fcode') = f fcode
       in (ta <> fa, Imp.If cond tcode' fcode')
    f (Imp.Op (Imp.DistributedLoop s i prebody body postbody free info)) =
      let (body_allocs, body') = extractAllocations body
          (free_allocs, here_allocs) = f body_allocs
          free' =
            filter
              ( not
                  . (`nameIn` Imp.declaredIn body_allocs)
                  . Imp.paramName
              )
              free
       in ( free_allocs,
            here_allocs
              <> Imp.Op (Imp.DistributedLoop s i prebody body' postbody free' info)
          )
    f code =
      (mempty, code)

getIterationDomain :: SegOp () MCMem -> SegSpace -> MPIGen (Imp.TExp Int64)
getIterationDomain SegMap {} space = do
  let ns = map snd $ unSegSpace space
      ns_64 = map toInt64Exp ns
  return $ product ns_64
getIterationDomain _ space = do
  let ns = map snd $ unSegSpace space
      ns_64 = map toInt64Exp ns
  case unSegSpace space of
    [_] -> return $ product ns_64
    -- A segmented SegOp is over the segments
    -- so we drop the last dimension, which is
    -- executed sequentially
    _ -> return $ product $ init ns_64

gather :: VName -> PrimType -> MPIGen ()
gather mem pt = do
  emit $ Imp.Op $ Imp.Gather mem $ primByteSize pt
