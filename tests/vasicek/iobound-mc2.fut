-- An I/O-bound mc2 implementation.
--
-- Useful for verification with a "sequential" R implementation.
--
-- ==
-- tags { no_opencl }

default(f32)

-- Some useful (for mc2) Futhark extensions.
fun sum(xs: []f32): f32 = reduce (+) (0.0) xs
fun mean(xs: [n]f32): f32 = sum(map (/f32(n)) xs)


-- Vasicek model parameters.
fun r0(): f32 = 0.03       -- initial interest rate
fun thetaP(): f32 = 0.03   -- short-rate mean
fun thetaQ(): f32 = 0.045  -- long-rate mean
fun kappa(): f32 = 0.1     -- speed of mean reversion
fun sigma(): f32 = 0.01    -- interest rate volatility


fun nextrP(lastr: f32, wp: f32): f32 =
  lastr + kappa() * (thetaP() - lastr) + sigma() * wp

fun nextrQ(lastr: f32, wq: f32): f32 =
  lastr + kappa() * (thetaQ() - lastr) + sigma() * wq

fun seqRedSumP(lastr: f32, ws: [n]f32): f32 =
  if n == 0
  then lastr
  else
    let (w0, wns) = split (1) ws in
    seqRedSumP(nextrP(lastr, w0[0]), wns)

fun seqRedSumQ(lastr: f32, ws: [n]f32): f32 =
  if n == 0
  then lastr
  else
    let (w0, wns) = split (1) ws in
    lastr + seqRedSumQ(nextrQ(lastr, w0[0]), wns)

fun mc1(wpss: [][]f32): []f32 =
  map mc1step wpss
fun mc1step(wps: []f32): f32 =
  seqRedSumP(r0(), wps)

fun mc2(wqsss: [][][]f32, r1s: []f32): []f32 =
  map mc2sim (zip wqsss r1s)
fun mc2sim(arg: ([tn][]f32, f32)): f32 =
  let ( wqss, r1 ) = arg
  let sum_r = map mc2step wqss (replicate tn r1) in
  mean(sum_r)
fun mc2step (wqs: []f32) (r1: f32): f32 =
  seqRedSumQ(r1, wqs)

fun main(wpss: [][]f32, wqsss: [][][]f32): []f32 = --mc1(wpss)
  mc2(wqsss, mc1(wpss))
