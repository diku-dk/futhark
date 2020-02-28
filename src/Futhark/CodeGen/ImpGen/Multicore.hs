{-# LANGUAGE TypeFamilies #-}
module Futhark.CodeGen.ImpGen.Multicore
  ( Futhark.CodeGen.ImpGen.Multicore.compileProg
  )
  where

import Control.Monad

import Futhark.Error
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp
import Futhark.CodeGen.ImpGen
import Futhark.Representation.ExplicitMemory
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory
            -> m (Either InternalError Imp.Program)
compileProg = Futhark.CodeGen.ImpGen.compileProg ops Imp.DefaultSpace
  where ops = defaultOperations opCompiler
        opCompiler :: OpCompiler ExplicitMemory Imp.Multicore
        opCompiler dest (Alloc e space) =
          compileAlloc dest e space
        opCompiler dest (Inner (SegOp op)) =
          compileSegOp dest op
        opCompiler _ (Inner SizeOp{}) =
          -- FIXME: we will have to do something here at some point.
          return ()
        opCompiler _ (Inner (OtherOp ())) =
          return ()

compileSegOp :: Pattern ExplicitMemory -> SegOp ExplicitMemory
             -> ImpM ExplicitMemory Imp.Multicore ()
compileSegOp pat (SegMap _ space _ (KernelBody _ kstms kres)) = do
  let (is, ns) = unzip $ unSegSpace space
  ns' <- mapM toExp ns

  body <- collect $ do
    zipWithM_ dPrimV_ is $ unflattenIndex ns' $ Imp.vi32 $ segFlat space
    compileStms (freeIn kres) kstms $ do
      let writeResult pe (Returns _ se) =
            copyDWIMFix (patElemName pe) (map Imp.vi32 is) se []
          writeResult _ res =
            error $ "writeResult: cannot handle " ++ pretty res
      zipWithM_ writeResult (patternElements pat) kres

  emit $ Imp.Op $ Imp.ParLoop (segFlat space) (product ns') body
compileSegOp _ op =
  error $ "compileSegOp: unhandled: " ++ pretty op
