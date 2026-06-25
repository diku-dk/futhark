# Scan Neutral Element Removal — Implementation Notes

This document describes the changes made to remove `scanNeutral :: [SubExp]` from
the `Scan` IR node, and the knock-on adaptations required in ISRWIM, stream
lowering, fusion, and AD.

## Motivation

The neutral element was stored in `Scan` so that backends and transforms could
initialise the scan accumulator before the first element.  Removing it forces
every pass to use the *is_first trick*: on the zeroth iteration, the map result
is used directly as the scan output, which is correct for any monoid because
`f(ne, x) = x`.

## `Scan` type change (`IR/SOACS/SOAC.hs`)

```haskell
-- Before
data Scan rep = Scan { scanLambda :: Lambda rep, scanNeutral :: [SubExp] }

-- After
newtype Scan rep = Scan { scanLambda :: Lambda rep }
```

Helper functions that previously derived sizes from the neutral list now use
`lambdaReturnType . scanLambda` instead.  The type-checker no longer checks
neutral element types.

## `FirstOrderTransform` (`Transform/FirstOrderTransform.hs`)

The sequential lowering of a scanomap into a `for` loop previously initialised
the accumulator with `scanNeutral`.  Now it:

1. Allocates *blank* accumulators (`eBlank`) — the actual value does not matter
   because the is_first guard prevents the scan lambda from seeing them on the
   first iteration.
2. Adds `is_first = (i == 0)` inside the loop body.
3. Branches on `is_first`:
   - True  → use the map result directly as the new accumulator.
   - False → apply `scan_lam(acc, map_result)`.

This is semantically equivalent to initialising with `ne` because
`f(ne, x) = x` for any valid monoid neutral.

## Stream lowering (`Analysis/HORep/SOAC.hs` — `soacToStream`)

`soacToStream` converts a scanomap SOAC into a `Stream` so that downstream
stream–stream fusion is not blocked.  The old implementation embedded a nested
`scanomap` SOAC inside the stream body, which required neutrals for the inner
accumulator initialisation.

### New approach: sequential for-loop body

The stream body now contains an explicit `for j < chunk` loop that applies the
is_first trick element-by-element:

```
stream body (chunk, is_first, acc..., inp_arrs...) =
  scan_out = scratch [chunk]
  map_out  = scratch [chunk]
  (_, acc', scan_out', map_out') =
    loop (is_first=is_first, acc=acc, scan_out, map_out) for j < chunk do
      x = inp_arrs[j]                       -- index element j
      map_res = map_lam(x)                  -- apply pre-scan map
      new_acc = if is_first then map_res
                else scan_lam(acc, map_res) -- apply scan lambda
      scan_out[j] = new_acc
      map_out[j]  = map_only_part_of_map_res
      (False, new_acc, scan_out, map_out)
  return (False, acc', scan_out', map_out')
```

The stream accumulators are:
- `is_first : Bool`  — initialised `True`, set `False` after first element.
- `acc... : scan_ts` — initialised with blank scalars (value irrelevant).

The outer `Stream` node carries `init_ne = [True, blank...]`.

### Why the for-loop is necessary

The alternative (index only `inp[0]` + replicate) was correct when the stream
was consumed by `FOT.transformSOAC` (which runs the body with `chunk = 1` per
loop iteration).  But `ExtractMulticore.transformSOAC` for `Stream` calls
`sequentialStreamWholeArray`, which inlines the body once with `chunk = w`.
With `chunk = w` and a body that only read `inp[0]`, every output position got
the scan value of the first element — producing wrong results.

The for-loop body is correct for any chunk size, including `chunk = w`.

### Performance trade-off

When `ExtractMulticore` encounters a stream that was produced by `soacToStream`
for a scanomap, the parallel path inlines the sequential for-loop rather than
emitting a `SegScan`.  Outer parallelism over the enclosing `map`/`tabulate`
dimension is still extracted as a `SegMap`; only the inner scan runs
sequentially per thread.  This is acceptable correctness-wise and is the same
behaviour as before the neutrals were removed (the old stream body also
contained a sequential scanomap that would have been sequentialised similarly).

The `soacToStream` signature now returns a third component `Stms rep` (currently
always `mempty`) so callers can emit prerequisite statements if needed in the
future.

## ISRWIM (`Pass/ExtractKernels/ISRWIM.hs`)

ISRWIM (Interchange Scan With Inner Map) rewrites `scan(f, map(g, a))` into
`map(scan(f), transpose(a))`.

### Signature change

```haskell
-- Before
iswim :: ... => Pat Type -> SubExp -> Lambda SOACS -> [(SubExp, VName)] -> Maybe (m ())
-- scan_input carried (neutral, array) pairs

-- After
iswim :: ... => Pat Type -> SubExp -> Lambda SOACS -> [VName] -> Maybe (m ())
-- just the arrays; neutrals are no longer part of Scan
```

### Body change

The old code prepended the accumulated neutral elements as extra map inputs
(so the inner scan could see them as its initial accumulator).  The new code
does not need this because the inner `Scan` is neutral-free and
`FirstOrderTransform` handles initialisation via the is_first trick:

```haskell
-- Before: map_arrs' = accs' ++ arrs'
-- After:  map_arrs' = arrs'  (no prepended neutral arrays)

-- Before: scan_input' = zip nes' scan_arrs  (neutrals from params)
-- After:  scan_arrs = map paramName map_params  (no neutrals)

-- Before: scanSOAC [Scan scan_fun' nes']
-- After:  scanSOAC [Scan scan_fun']
```

The `scan_elem_params` are now taken as `drop (length arrs)` (skipping the old
accumulator params), and `map_params` are built only from those element params.
The helper `mkMapPlusAccLam` that combined accumulator and element params was
removed entirely.

## Fusion (`Optimise/Fusion.hs`)

### `SoacNode` pre-statement field

`SoacNode` gained a fifth field `pre_stms :: Stms SOACS` to carry statements
that must be emitted before the fused SOAC.  This is plumbing for `soacToStream`
and is currently always `mempty` in practice.

### `okToFuseProducer` → `okToFuseScan`

The guard that was applied only to the *producer* is now applied to both
producer and consumer:

```haskell
ok_prod <- okToFuseScan soac1
ok_cons <- okToFuseScan soac2
if ok_prod && ok_cons && ...
```

This prevents a scan from being fused even when it appears on the consumer side.

### `dontFuseScans` for VJP bodies

VJP bodies require scan neutrals to be available at the point where AD runs.
Fusing a scanomap before AD would destroy this.  The fix:

```haskell
-- Before (wrong): VJP handler called doFuseScans
StmNode (Let pat aux (Op (Futhark.VJP args vec lam))) -> doFuseScans $ do ...

-- After (correct): VJP handler calls dontFuseScans
StmNode (Let pat aux (Op (Futhark.VJP args vec lam))) -> dontFuseScans $ do ...
```

JVP and `WithAcc` continue to use `doFuseScans` because they do not impose the
same constraint.

## Summary of what is disabled / degraded

| Area | What changed | Effect |
|---|---|---|
| Inner scan in `soacToStream` | For-loop body instead of nested `scanomap` | No `SegScan` extracted for inner scan in multicore par path; runs sequentially per thread |
| ISRWIM | Neutral arrays no longer prepended to the map input | Semantically equivalent; `FOT` handles initialisation |
| VJP fusion | `dontFuseScans` prevents scan fusion inside VJP bodies | Scans inside VJP bodies are not fused before AD runs |
| `soacToStream` return type | Added third `Stms rep` component (always `mempty`) | Callers updated; no functional change |

Nothing is permanently disabled — scan fusion still works for non-VJP contexts,
ISRWIM still fires, and streams are still produced for scanomap SOACs.  The only
real degradation is the missing inner `SegScan` parallelism in the multicore
path for scanomap-inside-stream, which matches the behaviour that existed before
this change.
