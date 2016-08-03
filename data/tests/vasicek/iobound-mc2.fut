-- An I/O-bound mc2 implementation.
--
-- Useful for verification with a "sequential" R implementation.
--
-- ==
-- tags { no_opencl }

default(float)

-- Some useful (for mc2) Futhark extensions.
fun float sum([]float xs) = reduce(+, 0.0, xs)
fun float mean([n]float xs) = sum(map(/float(n), xs))


-- Vasicek model parameters.
fun float r0() = 0.03       -- initial interest rate
fun float thetaP() = 0.03   -- short-rate mean
fun float thetaQ() = 0.045  -- long-rate mean
fun float kappa() = 0.1     -- speed of mean reversion
fun float sigma() = 0.01    -- interest rate volatility


fun float nextrP(float lastr, float wp) =
  lastr + kappa() * (thetaP() - lastr) + sigma() * wp

fun float nextrQ(float lastr, float wq) =
  lastr + kappa() * (thetaQ() - lastr) + sigma() * wq

fun float seqRedSumP(float lastr, [n]float ws) =
  if n == 0
  then lastr
  else
    let (w0, wns) = split((1),ws) in
    seqRedSumP(nextrP(lastr, w0[0]), wns)

fun float seqRedSumQ(float lastr, [n]float ws) =
  if n == 0
  then lastr
  else
    let (w0, wns) = split((1),ws) in
    lastr + seqRedSumQ(nextrQ(lastr, w0[0]), wns)

fun []float mc1([][]float wpss) =
  map(mc1step, wpss)
fun float mc1step([]float wps) =
  seqRedSumP(r0(), wps)

fun []float mc2([][][]float wqsss, []float r1s) =
  map(mc2sim, zip(wqsss, r1s))
fun float mc2sim(([tn][]float, float) arg) =
  let ( wqss, r1 ) = arg in
  let sum_r = zipWith(mc2step, wqss, replicate(tn, r1)) in
  mean(sum_r)
fun float mc2step(([]float, float) arg) =
  let ( wqs, r1 ) = arg in
  seqRedSumQ(r1, wqs)

fun []float main([][]float wpss, [][][]float wqsss) = --mc1(wpss)
  mc2(wqsss, mc1(wpss))
