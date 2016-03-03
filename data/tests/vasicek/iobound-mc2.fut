-- An I/O-bound MC2 implementation.
--
-- Useful for verification with a "sequential" R implementation.

default(f32)

-- Some useful (for MC2) Futhark extensions.
fun f32 sum([f32] xs) = reduce(+, 0.0, xs)
fun f32 mean([f32,n] xs) = sum(map(/f32(n), xs))


-- Vasicek model parameters.
fun f32 r0() = 0.03       -- initial interest rate
fun f32 thetaP() = 0.03   -- short-rate mean
fun f32 thetaQ() = 0.045  -- long-rate mean
fun f32 kappa() = 0.1     -- speed of mean reversion
fun f32 sigma() = 0.01    -- interest rate volatility


fun f32 nextrP(f32 lastr, f32 WP) =
  lastr + kappa() * (thetaP() - lastr) + sigma() * WP

fun f32 nextrQ(f32 lastr, f32 WQ) =
  lastr + kappa() * (thetaQ() - lastr) + sigma() * WQ

fun f32 seqRedSumP(f32 lastr, [f32] Ws) =
  if (size(0,Ws) == 0)
  then lastr
  else
    let {W0, Wns} = split((1),Ws) in
    seqRedSumP(nextrP(lastr, W0[0]), Wns)

fun f32 seqRedSumQ(f32 lastr, [f32] Ws) =
  if (size(0,Ws) == 0)
  then lastr
  else
    let {W0, Wns} = split((1),Ws) in
    lastr + seqRedSumQ(nextrQ(lastr, W0[0]), Wns)

fun [f32] MC1([[f32]] WPss) =
  map(MC1step, WPss)
fun f32 MC1step([f32] WPs) =
  seqRedSumP(r0(), WPs)

fun [f32] MC2([[[f32]]] WQsss, [f32] r1s) =
  map(MC2sim, zip(WQsss, r1s))
fun f32 MC2sim({[[f32]], f32} arg) =
  let { WQss, r1 } = arg in
  let tn = size(0, WQss) in
  let sum_r = zipWith(MC2step, WQss, replicate(tn, r1)) in
  mean(sum_r)
fun f32 MC2step({[f32], f32} arg) =
  let { WQs, r1 } = arg in
  seqRedSumQ(r1, WQs)

fun [f32] main([[f32]] WPss, [[[f32]]] WQsss) = --MC1(WPss)
  MC2(WQsss, MC1(WPss))
