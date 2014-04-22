{-# LANGUAGE QuasiQuotes #-}
-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program. The C code is strictly
-- sequential and leaks memory like a sieve, so it's not very useful
-- yet.
module Futhark.CodeGen.Backends.SequentialC
  ( compileProgBadly
  , compileProg
  ) where

import Control.Monad
import Control.Monad.Writer

import qualified Language.C.Quote.C as C

import Futhark.InternalRep

import qualified Futhark.CodeGen.ImpCode as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen
import qualified Futhark.CodeGen.Backends.GenericC as GenericC
import Futhark.CodeGen.FirstOrderSOACS

compileProgBadly :: Prog -> String
compileProgBadly = GenericC.compileProg codeCompiler . ImpGen.compileProg firstOrderSOACS
  where codeCompiler :: GenericC.OpCompiler ()
        codeCompiler () = return GenericC.Done

-- Some operations can be implemented to more efficient C than with
-- the default ImpCode.

-- TODO: maybe add rearrange and others?
data ArrayOp = ReshapeOp VName [Imp.Exp] VName
             | SplitOp VName VName Imp.Exp VName

compileProg :: Prog -> String
compileProg = GenericC.compileProg codeCompiler . ImpGen.compileProg compileExp
  where compileExp :: ImpGen.ExpCompiler ArrayOp
        compileExp [target] (Reshape _ shape src _) = do
          let shape' = map ImpGen.compileSubExp shape
          src' <- ImpGen.expAsName $ ImpGen.compileSubExp src
          ImpGen.declareVar target
          tell $ Imp.Op $ ReshapeOp (identName target) shape' src'
          return ImpGen.Done
        compileExp [target1,target2] (Split _ n e _ _) = do
          let n' = ImpGen.compileSubExp n
          e' <- ImpGen.expAsName $ ImpGen.compileSubExp e
          ImpGen.declareVar target1
          ImpGen.declareVar target2
          tell $ Imp.Op $ SplitOp (identName target1) (identName target2) n' e'
          return ImpGen.Done
        compileExp targets e = firstOrderSOACS targets e

        codeCompiler :: GenericC.OpCompiler ArrayOp
        codeCompiler (ReshapeOp target shape src) = do
          let target' = textual target
              src' = textual src
          GenericC.stm [C.cstm|$id:target'.data = $id:src'.data;|]
          shape' <- mapM GenericC.compileExp shape
          forM_ (zip [(0::Int)..] shape') $ \(i,e) ->
            GenericC.stm [C.cstm|$id:target'.shape[$int:i] = $exp:e;|]
          return GenericC.Done
        codeCompiler (SplitOp target1 target2 n src) = do
          let target1' = textual target1
              target2' = textual target2
              src'     = textual src
          n' <- GenericC.compileExp n
          GenericC.stm [C.cstm|$id:target1' = $id:src';|]
          GenericC.stm [C.cstm|$id:target1'.shape[0] = $exp:n';|]
          GenericC.stm [C.cstm|$id:target2' = $id:src';|]
          GenericC.stm [C.cstm|$id:target2'.data  += $exp:n';|]
          GenericC.stm [C.cstm|$id:target2'.shape[0] -= $exp:n';|]
          return GenericC.Done
