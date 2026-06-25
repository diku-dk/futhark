# `revLoop` merge/body shape mismatch when a loop result is a constant

## Trigger

Run the AD pass on a `for` loop whose body's result tuple contains a
**constant** `SubExp` in some position (e.g. `in {false, x, y}`).  Such a loop
arises naturally after `Futhark.Analysis.HORep.SOAC.soacToStream` lowers a
scanomap to a `Stream` whose body uses the *is_first* trick: the body sets the
boolean accumulator to the constant `false` after the first iteration.

A minimal reproducer that triggers this bug is `tests/ad/issue2256.fut`,
once scan fusion is permitted inside a `vjp` body (the change in
`Optimise/Fusion.hs` from `dontFuseScans` to `doFuseScans` for the
`VJP` case).  With scan fusion enabled, the fusion pass converts the
scanomap producer into a stream, AD's `vjpSOAC` for `Stream` calls
`sequentialStreamWholeArray`, and the inlined stream body — containing
the new `is_first` for-loop — is what AD then differentiates.

## Symptom

Type error after pass `ad`:

```
Declaration of function <loop body> declares return type
  {f64, [m_5995]f64, i64, [m_5995]f64, bool}
But body has type
  {bool, f64, [m_5995]f64, i64, [m_5995]f64}
```

The `bool` accumulator ends up at the *end* of the declared merge but at
the *front* of the body's result tuple.

## Root cause

In `src/Futhark/AD/Rev/Loop.hs`, `revLoop` constructs the adjoint loop's
merge in two halves that disagree about whether constant body results
participate.

1. The **merge parameters** are produced by `valPatAdjs loop_vnames`,
   sized from

   ```haskell
   loopRes = mapMaybe subExpResVName $ bodyResult body'
   ```

   `mapMaybe subExpResVName` silently **drops constants**, so a body
   result of `Constant (BoolValue False)` does not get a merge slot in
   `val_pat_adjs.loopRes`.

2. The **body results** are produced by

   ```haskell
   loop_res_adjs <- mapM (lookupAdjVal . paramName) loop_params'
   ```

   This iterates over every **loop parameter**, including the one that
   was paired with the constant body result, so it always yields one
   adjoint per loop parameter.

When the body returns `n` results of which `k` are constants, the
adjoint loop ends up with `n - k` `loopRes` merge slots but `n`
`loopRes` body adjoints — the body has `k` more results than the
declaration.

The subsequent `simplifyLambda` (called on the AD output in
`src/Futhark/Pass/AD.hs`) collapses the malformed Loop by deleting
unused merge slots, but it also reshuffles the surviving slots so that
the body and declared types end up in different orders.  The
typechecker then reports the misaligned bool at the front of the body
vs. the end of the declaration.

## Why scan-fusion-in-VJP triggers it

Before the neutral-removal refactor, a scanomap stream body contained a
nested `scanomap` SOAC initialised with the neutral element.  No
`is_first` boolean was needed, no boolean-valued constant ever appeared
in a body result, and `revLoop` never hit the `mapMaybe
subExpResVName` skew.

After neutral removal, `soacToStream`'s scanomap branch
(`src/Futhark/Analysis/HORep/SOAC.hs`) uses an explicit `for j < chunk`
loop with the is_first trick: the loop body's first result is
`Constant (BoolValue False)` after the first iteration.  Once VJP scan
fusion is permitted, this loop reaches `revLoop`, which then exhibits
the merge/body shape skew documented above.

## Fix (applied)

`revLoop` (`src/Futhark/AD/Rev/Loop.hs`) now filters `loop_params'` to
keep only those whose corresponding body result is a `Var`, before
collecting body-side `loopRes` adjoints.  The post-loop
`updateSubExpAdj` over initial values is filtered the same way.  This
brings the body-adjoint count in lock-step with `val_pat_adjs.loopRes`
(which already excluded constants via `mapMaybe subExpResVName`), so
no malformed Loop is ever constructed.

## Optional: peel away the is_first toggle

In addition to the correctness fix above, a top-down simplification
rule `peelIsFirstParam` in
`src/Futhark/Optimise/Simplify/Rules/Loop.hs` recognises the canonical
is_first shape — bool merge param, init `true`, body result `false`,
used only as the sole scrutinee of top-level matches — and peels
iteration 0 outside the loop, dropping the merge slot.  After peeling,
the loop body has no constant result and no `if` branch on the bool,
which is also the cleanest form for AD.

The rule is restricted to constant bounds ≥ 1 (so peeling is always
safe) and to top-level matches (no nested-match recognition).  For
symbolic bounds — including the chunked-stream loops that originally
exposed the AD bug — the rule does not fire; the correctness fix in
`revLoop` carries those cases.

### Placement: post-extraction backend simplifies only

The is_first pattern is born when a SOACS-level `Stream` is lowered
into a sequential `for` loop, either by `FirstOrderTransform` (Seq
pipeline), `extractMulticore` followed by `sequentialStreamWholeArray`
(MC pipeline), or `extractKernels` (GPU pipeline).  It never appears
during the standard fusion-era SOACS simplify passes.

To keep that property explicit — and to guarantee the rule cannot
interfere with fusion or any other SOACS-stage transform — the rule
is *not* part of the shared `loopRules` / `standardRules`.  It is
exported separately as `peelIsFirstRules` and added only to the
per-backend rule books:

- `IR/GPU/Simplify.hs`  → `kernelRules`
- `IR/MC.hs`            → `rules` (in `simplifyProg`)
- `IR/Seq.hs`           → `rules` (in `simplifyProg`)
- `IR/Mem/Simplify.hs`  → `memRuleBook` (covers `SeqMem`, `MCMem`, `GPUMem`)

Empirically, on `tests/intrablock/big0.fut` (a `scan` over `iota 256`
inside a `map`), the rule fires once after `first order transform`
(Seq), twice after `extract multicore parallelism` + `unstream` (MC),
and twice after `extract kernels` (GPU).  Zero fires during the
SOACS-stage standard pipeline.
