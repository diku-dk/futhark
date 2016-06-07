-- An I/O-bound MC2 implementation.
--
-- Useful for verification with a "sequential" R implementation.
--
-- ==
-- tags { no_opencl }

default(float)

-- Some useful (for MC2) Futhark extensions.
fun float sum([float] xs) = reduce(+, 0.0, xs)
fun float mean([float,n] xs) = sum(map(/float(n), xs))


-- Vasicek model parameters.
fun float r0() = 0.03       -- initial interest rate
fun float thetaP() = 0.03   -- short-rate mean
fun float thetaQ() = 0.045  -- long-rate mean
fun float kappa() = 0.1     -- speed of mean reversion
fun float sigma() = 0.01    -- interest rate volatility


fun float nextrP(float lastr, float WP) =
  lastr + kappa() * (thetaP() - lastr) + sigma() * WP

fun float nextrQ(float lastr, float WQ) =
  lastr + kappa() * (thetaQ() - lastr) + sigma() * WQ

fun float seqRedSumP(float lastr, [float] Ws) =
  if (size(0,Ws) == 0)
  then lastr
  else
    let (W0, Wns) = split((1),Ws) in
    seqRedSumP(nextrP(lastr, W0[0]), Wns)

fun float seqRedSumQ(float lastr, [float] Ws) =
  if (size(0,Ws) == 0)
  then lastr
  else
    let (W0, Wns) = split((1),Ws) in
    lastr + seqRedSumQ(nextrQ(lastr, W0[0]), Wns)

fun [float] MC1([[float]] WPss) =
  map(MC1step, WPss)
fun float MC1step([float] WPs) =
  seqRedSumP(r0(), WPs)

fun [float] MC2([[[float]]] WQsss, [float] r1s) =
  map(MC2sim, zip(WQsss, r1s))
fun float MC2sim(([[float]], float) arg) =
  let ( WQss, r1 ) = arg in
  let tn = size(0, WQss) in
  let sum_r = zipWith(MC2step, WQss, replicate(tn, r1)) in
  mean(sum_r)
fun float MC2step(([float], float) arg) =
  let ( WQs, r1 ) = arg in
  seqRedSumQ(r1, WQs)

fun [float] main([[float]] WPss, [[[float]]] WQsss) = --MC1(WPss)
  MC2(WQsss, MC1(WPss))
