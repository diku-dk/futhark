-- An I/O-bound MC2 implementation.
--
-- Useful for verification with a "sequential" R implementation.

default(f64)

-- Some useful (for MC2) Futhark extensions.
fun f64 sum([f64] xs) = reduce(+, 0.0, xs)
fun f64 mean([f64] xs) = sum(xs) / f64(size(0, xs))
fun f64 e() = -- wolframalpha.com
  2.718281828459045235360287471352662497757247093699959574966
fun [f64] exp([f64] xs) = map(fn f64 (f64 x) => e() ** x, xs)


-- Vasicek model parameters.
fun f64 r0() = 0.03       -- initial interest rate
fun f64 thetaP() = 0.03   -- short-rate mean
fun f64 thetaQ() = 0.045  -- long-rate mean
fun f64 kappa() = 0.1     -- speed of mean reversion
fun f64 sigma() = 0.01    -- interest rate volatility


fun f64 nextrP(f64 lastr, f64 WP) =
  lastr + kappa() * (thetaP() - lastr) + sigma() * WP

fun f64 nextrQ(f64 lastr, f64 WQ) =
  lastr + kappa() * (thetaQ() - lastr) + sigma() * WQ

fun [f64] MC1([[f64]] WPss) = map(MC1Step, WPss)
fun f64 MC1Step([f64] WPs) = sum(scan(nextrP, r0(), WPs))

fun [f64] main([[f64]] WPss) = MC1(WPss)
