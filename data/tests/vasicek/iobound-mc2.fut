-- An I/O-bound mc2 implementation.
--
-- Useful for verification with a "sequential" R implementation.
--
-- ==
-- tags { no_opencl }

default(float)

-- Some useful (for mc2) Futhark extensions.
fun sum(xs: []float): float = reduce((+), 0.0, xs)
fun mean(xs: [n]float): float = sum(map((/float(n)), xs))


-- Vasicek model parameters.
fun r0(): float = 0.03       -- initial interest rate
fun thetaP(): float = 0.03   -- short-rate mean
fun thetaQ(): float = 0.045  -- long-rate mean
fun kappa(): float = 0.1     -- speed of mean reversion
fun sigma(): float = 0.01    -- interest rate volatility


fun nextrP(lastr: float, wp: float): float =
  lastr + kappa() * (thetaP() - lastr) + sigma() * wp

fun nextrQ(lastr: float, wq: float): float =
  lastr + kappa() * (thetaQ() - lastr) + sigma() * wq

fun seqRedSumP(lastr: float, ws: [n]float): float =
  if n == 0
  then lastr
  else
    let (w0, wns) = split((1),ws) in
    seqRedSumP(nextrP(lastr, w0[0]), wns)

fun seqRedSumQ(lastr: float, ws: [n]float): float =
  if n == 0
  then lastr
  else
    let (w0, wns) = split((1),ws) in
    lastr + seqRedSumQ(nextrQ(lastr, w0[0]), wns)

fun mc1(wpss: [][]float): []float =
  map(mc1step, wpss)
fun mc1step(wps: []float): float =
  seqRedSumP(r0(), wps)

fun mc2(wqsss: [][][]float, r1s: []float): []float =
  map(mc2sim, zip(wqsss, r1s))
fun mc2sim(arg: ([tn][]float, float)): float =
  let ( wqss, r1 ) = arg in
  let sum_r = zipWith(mc2step, wqss, replicate(tn, r1)) in
  mean(sum_r)
fun mc2step(arg: ([]float, float)): float =
  let ( wqs, r1 ) = arg in
  seqRedSumQ(r1, wqs)

fun main(wpss: [][]float, wqsss: [][][]float): []float = --mc1(wpss)
  mc2(wqsss, mc1(wpss))
