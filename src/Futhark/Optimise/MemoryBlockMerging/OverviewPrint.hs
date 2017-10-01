{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Print the important parts of a program.
--
-- Enable by setting the environment variable
-- MEMORY_BLOCK_MERGING_OVERVIEW_PRINT=1.
module Futhark.Optimise.MemoryBlockMerging.OverviewPrint
  ( overviewPrintProg
  ) where

import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (intercalate)
import Control.Monad.Reader
import Control.Monad.Writer
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)

import Futhark.Util (isEnvVarSet)
import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
  (ExplicitMemorish, ExplicitMemory, InKernel)
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Representation.Kernels.Kernel

import Futhark.Optimise.MemoryBlockMerging.Types
import Futhark.Optimise.MemoryBlockMerging.Miscellaneous
import Futhark.Optimise.MemoryBlockMerging.Liveness.FirstUse (createsNewArrayBase)


type LoreConstraints lore = (ExplicitMemorish lore,
                             ExpHandling lore,
                             OverviewPrintOp lore,
                             FullWalk lore)

-- Also include index reads and writes in the filtered program.
alsoPrintsIndexAccesses :: Bool
alsoPrintsIndexAccesses =
  isEnvVarSet "MEMORY_BLOCK_MERGING_OVERVIEW_PRINT_ALSO_INDEX_ACCESSES" False

overviewPrintProg :: MonadFreshNames m
                  => Log -> Prog ExplicitMemory
                  -> m (Prog ExplicitMemory) -- Actually we'll just print the
                                             -- smaller program and then exit.
overviewPrintProg proglog (Prog prog) =
  (`seq` return (Prog prog)) $ unsafePerformIO $ (>> exitSuccess) $ do
  let prog' = map opFunDef prog
  putStrLn $ runReader (opPrettyProg (Prog prog')) proglog

opFunDef :: LoreConstraints lore =>
            FunDef lore -> FunDef lore
opFunDef fundef = fundef { funDefBody = opBody $ funDefBody fundef }

opBody :: LoreConstraints lore =>
          Body lore -> Body lore
opBody body = body { bodyStms = mapMaybe opStm $ bodyStms body }

opKernelBody :: LoreConstraints lore =>
                KernelBody lore -> KernelBody lore
opKernelBody body = body { kernelBodyStms = mapMaybe opStm $ kernelBodyStms body }

opStm :: LoreConstraints lore =>
         Stm lore -> Maybe (Stm lore)
opStm stm@(Let (Pattern _ patvalelems) _ e)
  | any (isBindInPlace . patElemBindage) patvalelems && alsoPrintsIndexAccesses =
      Just $ stm { stmExp = fromMaybe e $ opExp e }
  | any (isArrayMem . patElemAttr) patvalelems = do
      e' <- opExp e
      return stm { stmExp = e' }
  | otherwise = Nothing

isBindInPlace :: Bindage -> Bool
isBindInPlace BindInPlace{} = True
isBindInPlace BindVar = False

isArrayMem :: ExpMem.MemBound u -> Bool
isArrayMem ExpMem.ArrayMem{} = True
isArrayMem _ = False

class ExpHandling lore where
  opExp :: Exp lore -> Maybe (Exp lore)

instance ExpHandling ExplicitMemory where
  opExp e = case e of
    Op (ExpMem.Inner (ExpMem.Kernel a c d kernelbody)) ->
      let kernelbody' = opKernelBody kernelbody
      in Just $ Op (ExpMem.Inner (ExpMem.Kernel a c d kernelbody'))
    _ -> opExpBase e

instance ExpHandling InKernel where
  opExp e = case e of
    Op (ExpMem.Inner (ExpMem.GroupReduce a lambda b)) ->
      Just $ Op (ExpMem.Inner (ExpMem.GroupReduce a (opLambda lambda) b))
    Op (ExpMem.Inner (ExpMem.GroupScan a lambda b)) ->
      Just $ Op (ExpMem.Inner (ExpMem.GroupScan a (opLambda lambda) b))
    Op (ExpMem.Inner (ExpMem.GroupStream a b gslambda c d)) ->
      let gslambda' = gslambda { ExpMem.groupStreamLambdaBody =
                                 opBody $ ExpMem.groupStreamLambdaBody gslambda }
      in Just $ Op (ExpMem.Inner (ExpMem.GroupStream a b gslambda' c d))
    Op (ExpMem.Inner (ExpMem.Combine a b c body)) ->
      Just $ Op (ExpMem.Inner (ExpMem.Combine a b c (opBody body)))
    _ -> opExpBase e

opExpBase :: LoreConstraints lore =>
             Exp lore -> Maybe (Exp lore)
opExpBase e = case e of
  DoLoop a b c loopbody ->
    Just $ DoLoop a b c (opBody loopbody)
  If a ifbody0 ifbody1 b ->
    let ifbody0' = opBody ifbody0
        ifbody1' = opBody ifbody1
    in if null (bodyStms ifbody0') || null (bodyStms ifbody1')
       then Nothing
       else Just $ If a ifbody0' ifbody1' b
  BasicOp bop ->
    if createsNewArrayBase e
    then Just e
    else case bop of
      Index{} -> if alsoPrintsIndexAccesses
                 then Just e
                 else Nothing
      _ -> Nothing
  _ -> Nothing

opLambda :: LoreConstraints lore =>
            Lambda lore -> Lambda lore
opLambda (Lambda a body b) = Lambda a (opBody body) b

opUnlines :: [String] -> String
opUnlines = intercalate "\n"

opIndent :: String -> String
opIndent = opUnlines . map (replicate 4 ' ' ++) . lines

opPrettyProg :: LoreConstraints lore => Prog lore -> Reader Log String
opPrettyProg (Prog fundefs) =
  intercalate "\n\n" <$> mapM opPrettyFunDef fundefs

opPrettyFunDef :: LoreConstraints lore => FunDef lore -> Reader Log String
opPrettyFunDef fundef = do
  pbody <- opPrettyBody (funDefBody fundef)
  return $ pretty (funDefName fundef) ++ " =\n" ++ opIndent pbody

opPrettyBody :: LoreConstraints lore => Body lore -> Reader Log String
opPrettyBody body = opUnlines <$> mapM opPrettyStm (bodyStms body)

opPrettyStm :: LoreConstraints lore => Stm lore -> Reader Log String
opPrettyStm (Let (Pattern _ patvalelems) _ e) = do
  Log proglog <- ask
  let patpretty (PatElem name _ (ExpMem.ArrayMem _ _ _ mem _)) =
        let attrs = ("memory block", pretty mem) : lookupEmptyable name proglog
        in map (\(topic, content) ->
                   let base = " | " ++ pretty name ++ " | " ++ topic ++ ": "
                       content_splits = stringSplitInLines 150 content
                       content' = case content_splits of
                         [] -> [base]
                         (hd : tl) -> (base ++ hd)
                                      : map (replicate (length base) ' ' ++) tl
                   in opUnlines content') attrs
      patpretty _ = []
      variable_details = concatMap patpretty patvalelems
  case e of
        If _ ifbody0 ifbody1 _ -> do
          ifbody0' <- opPrettyBody ifbody0
          ifbody1' <- opPrettyBody ifbody1
          return $ opUnlines [ "if"
                             , "then"
                             , opIndent ifbody0'
                             , "else"
                             , opIndent ifbody1'
                             ]
        _ -> let expname = case e of
                   DoLoop{} -> "loop"
                   Op{} -> unRes $ opPrettyOp e
                   _ -> pretty e

                 line = opUnlines (expname : variable_details)

                 opSubStms :: LoreConstraints lore => [Stm lore] -> Writer String ()
                 opSubStms = tell . opIndent . opUnlines . map (\stm -> runReader (opPrettyStm stm) (Log proglog))
                 walker = identityWalker
                   { walkOnBody = opSubStms . bodyStms }
                 walker_kernel = identityKernelWalker
                   { walkOnKernelBody = opSubStms . bodyStms
                   , walkOnKernelKernelBody = opSubStms . kernelBodyStms
                   , walkOnKernelLambda = opSubStms . bodyStms . lambdaBody
                   }
                 substms = execWriter $ fullWalkExpM walker walker_kernel e
             in return $ opUnlines (line : if null substms then [] else [substms])

stringSplitInLines :: Int -> String -> [String]
stringSplitInLines n = sjoin . words
  where sjoin (s : t : us)
          | length s + 1 + length s <= n = sjoin ((s ++ " " ++ t) : us)
          | otherwise = s : sjoin (t : us)
        sjoin us = us

newtype Res lore a = Res { unRes :: a }

class OverviewPrintOp lore where
  opPrettyOp :: Exp lore -> Res lore String

instance OverviewPrintOp ExplicitMemory where
  opPrettyOp (Op (ExpMem.Inner (ExpMem.Kernel (KernelDebugHints name _) _ _ _))) =
    Res ("kernel " ++ name)
  opPrettyOp e = Res $ pretty e

instance OverviewPrintOp InKernel where
  opPrettyOp (Op (ExpMem.Inner kexp)) = Res $ case kexp of
    ExpMem.GroupReduce{} -> "reduce"
    ExpMem.GroupScan{} -> "scan"
    ExpMem.GroupStream{} -> "stream"
    ExpMem.Combine{} -> "combine"
    _ -> pretty kexp
  opPrettyOp e = Res $ pretty e
