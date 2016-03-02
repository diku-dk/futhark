-- An I/O-bound MC2 implementation.
--
-- Useful for verification with a "sequential" R implementation.

default(f64)

-- Some useful (for MC2) Futhark extensions.
fun f64 sum([f64] xs) = reduce(+, 0.0, xs)
fun f64 mean([f64,n] xs) = sum(map(/f64(n), xs))


-- Vasicek model parameters.
fun f64 r0() = 0.03       -- initial interest rate
fun f64 thetaP() = 0.03   -- short-rate mean
fun f64 thetaQ() = 0.045  -- long-rate mean
fun f64 kappa() = 0.1     -- speed of mean reversion
fun f64 sigma() = 0.01    -- interest rate volatility


fun int n() = 50          -- number of simulation periods

fun f64 nextrP(f64 lastr, f64 WP) =
  lastr + kappa() * (thetaP() - lastr) + sigma() * WP

fun f64 nextrQ(f64 lastr, f64 WQ) =
  lastr + kappa() * (thetaQ() - lastr) + sigma() * WQ

fun [f64] MC1([[f64]] WPss) = map(MC1step, WPss)
fun f64 MC1step([f64] WPs) = sum(scan(nextrP, r0(), WPs))

fun [f64] MC2([[[f64]]] WQsss, [f64] r1s) =
  map(MC2sim, zip(WQsss, r1s))
fun f64 MC2sim({[[f64]], f64} arg) =
  let { WQss, r1 } = arg in
  mean(map(exp, (map(MC2step, zip(WQss, replicate(n(), r1))))))
fun f64 MC2step({[f64], f64} arg) =
  let { WQs, r1 } = arg in
  sum(scan(nextrQ, r1, WQs))

fun [f64] main([[f64]] WPss, [[[f64]]] WQsss) =
  MC2(WQsss, MC1(WPss))
