{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.Analysis.InterferenceTests
  ( tests,
  )
where

import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Set as Set
import qualified Futhark.Analysis.Interference as Interference
import Futhark.IR.KernelsMem (KernelsMem)
import Futhark.IR.Syntax
import Language.SexpGrammar
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "InterferenceTests"
    [psumTest]

psumTest :: TestTree
psumTest =
  testCase "psum.fut" $ do
    let prog =
          decode @(Prog KernelsMem) $
            UTF8.fromString "(prog () (fundef (just ((direct) (direct))) {} main ((array i32 ((ext 0) (ext 1)) unique (returns-new-block default 2 (ixfun ((lmad (value 0i32) (dim (leaf (ext 1) i32) (value 0i32) (leaf (ext 0) i32) 0 inc) (dim (value 1i32) (value 0i32) (leaf (ext 1) i32) 1 inc))) ((leaf (ext 0) i32) (leaf (ext 1) i32)) true)))) ((param xss_mem_759 (mem default)) (param impl₀_245 (prim i32)) (param impl₁_246 (prim i32)) (param xss_247 (array i32 (impl₀_245 impl₁_246) nonunique (xss_mem_759 (ixfun ((lmad (value 0i32) (dim (leaf impl₁_246 i32) (value 0i32) (leaf impl₀_245 i32) 0 inc) (dim (value 1i32) (value 0i32) (leaf impl₁_246 i32) 1 inc))) ((leaf impl₀_245 i32) (leaf impl₁_246 i32)) true))))) (nil ((let (() ((binop_x_774 (prim i64)))) (aux () {(comp incremental_flattening ((atom only_intra)))} nil) ((sext i32 i64) impl₀_245)) (let (() ((binop_y_775 (prim i64)))) (aux () {(comp incremental_flattening ((atom only_intra)))} nil) ((sext i32 i64) impl₁_246)) (let (() ((binop_x_776 (prim i64)))) (aux () {(comp incremental_flattening ((atom only_intra)))} nil) ((mul i64 undef) binop_x_774 binop_y_775)) (let (() ((bytes_773 (prim i64)))) (aux () {(comp incremental_flattening ((atom only_intra)))} nil) ((mul i64 undef) 4i64 binop_x_776)) (let (() ((mem_777 (mem default)))) (aux () {(comp incremental_flattening ((atom only_intra)))} nil) (alloc bytes_773 default)) (let (() ((binop_x_763 (prim i64)))) (aux () {} nil) (binop_y_775)) (let (() ((bytes_762 (prim i64)))) (aux () {} nil) ((mul i64 undef) 4i64 binop_y_775)) (let (() ((binop_x_767 (prim i64)))) (aux () {} nil) (binop_y_775)) (let (() ((bytes_766 (prim i64)))) (aux () {} nil) (bytes_762)) (let (() ((binop_x_771 (prim i64)))) (aux () {} nil) (binop_y_775)) (let (() ((bytes_770 (prim i64)))) (aux () {} nil) (bytes_762)) (let (() ((res_408 (array i32 (impl₀_245 impl₁_246) no-uniqueness (mem_777 (ixfun ((lmad (value 0i32) (dim (leaf impl₁_246 i32) (value 0i32) (leaf impl₀_245 i32) 0 inc) (dim (value 1i32) (value 0i32) (leaf impl₁_246 i32) 1 inc))) ((leaf impl₀_245 i32) (leaf impl₁_246 i32)) true)))))) (aux () {(comp incremental_flattening ((atom only_intra)))} nil) (inner (segmap (group impl₀_245 impl₁_246 no-virt) (phys_tid_305 ((gtid_292 impl₀_245))) ((array i32 (impl₁_246) no-uniqueness)) (nil ((let (() ((mem_764 (mem (space \"local\"))))) (aux () {} nil) (alloc bytes_762 (space \"local\"))) (let (() ((resarr0_415 (array i32 (impl₁_246) no-uniqueness (mem_764 (ixfun ((lmad (value 0i32) (dim (value 1i32) (value 0i32) (leaf impl₁_246 i32) 0 inc))) ((leaf impl₁_246 i32)) true)))))) (aux () {} nil) (inner (segscan (thread impl₀_245 impl₁_246 no-virt) (phys_tid_296 ((gtid_295 impl₁_246))) ((noncommutative (lambda ((param x_416 (prim i32)) (param x_417 (prim i32))) (nil ((let (() ((res_418 (prim i32)))) (aux () {} nil) ((add i32 wrap) x_416 x_417))) (res_418)) (i32)) (0i32) ())) (i32) (nil ((let (() ((x_419 (prim i32)))) (aux () {} nil) (index xss_247 (gtid_292 gtid_295)))) ((returns may-simplify x_419)))))) (let (() ((mem_768 (mem (space \"local\"))))) (aux () {} nil) (alloc bytes_762 (space \"local\"))) (let (() ((resarr0_425 (array i32 (impl₁_246) no-uniqueness (mem_768 (ixfun ((lmad (value 0i32) (dim (value 1i32) (value 0i32) (leaf impl₁_246 i32) 0 inc))) ((leaf impl₁_246 i32)) true)))))) (aux () {} nil) (inner (segscan (thread impl₀_245 impl₁_246 no-virt) (phys_tid_298 ((gtid_297 impl₁_246))) ((noncommutative (lambda ((param x_426 (prim i32)) (param x_427 (prim i32))) (nil ((let (() ((res_428 (prim i32)))) (aux () {} nil) ((add i32 wrap) x_426 x_427))) (res_428)) (i32)) (0i32) ())) (i32) (nil ((let (() ((x_429 (prim i32)))) (aux () {} nil) (index resarr0_415 (gtid_297)))) ((returns may-simplify x_429)))))) (let (() ((mem_772 (mem (space \"local\"))))) (aux () {} nil) (alloc bytes_762 (space \"local\"))) (let (() ((resarr0_434 (array i32 (impl₁_246) no-uniqueness (mem_772 (ixfun ((lmad (value 0i32) (dim (value 1i32) (value 0i32) (leaf impl₁_246 i32) 0 inc))) ((leaf impl₁_246 i32)) true)))))) (aux () {} nil) (inner (segscan (thread impl₀_245 impl₁_246 no-virt) (phys_tid_300 ((gtid_299 impl₁_246))) ((noncommutative (lambda ((param x_435 (prim i32)) (param x_436 (prim i32))) (nil ((let (() ((res_437 (prim i32)))) (aux () {} nil) ((add i32 wrap) x_435 x_436))) (res_437)) (i32)) (0i32) ())) (i32) (nil ((let (() ((x_438 (prim i32)))) (aux () {} nil) (index resarr0_425 (gtid_299)))) ((returns may-simplify x_438))))))) ((returns may-simplify resarr0_434))))))) (impl₀_245 impl₁_246 mem_777 res_408))))"
    case prog of
      Right prog' ->
        assertEqual
          "Interference graph incorrect"
          [ (VName (nameFromString "mem") 764, VName (nameFromString "mem") 768),
            (VName (nameFromString "mem") 768, VName (nameFromString "mem") 772)
          ]
          (Set.toList $ Interference.analyse prog')
      Left e ->
        assertFailure $ "Could not decode test program:\n" ++ e
