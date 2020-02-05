-- An I/O-bound mc2 implementation.
--
-- Useful for verification with a "sequential" R implementation.
--
-- ==

-- Some useful (for mc2) Futhark extensions.
let sum(xs: []f32): f32 = reduce (+) (0.0) xs
let mean [n] (xs: [n]f32): f32 = sum(map (/r32(n)) xs)


-- Vasicek model parameters.
let r0(): f32 = 0.03       -- initial interest rate
let thetaP(): f32 = 0.03   -- short-rate mean
let thetaQ(): f32 = 0.045  -- long-rate mean
let kappa(): f32 = 0.1     -- speed of mean reversion
let sigma(): f32 = 0.01    -- interest rate volatility


let nextrP(lastr: f32, wp: f32): f32 =
  lastr + kappa() * (thetaP() - lastr) + sigma() * wp

let nextrQ(lastr: f32, wq: f32): f32 =
  lastr + kappa() * (thetaQ() - lastr) + sigma() * wq

let seqRedSumP [n] (lastr: f32, ws: [n]f32): f32 =
  loop (lastr) for i < n do nextrP(lastr, ws[i])

let seqRedSumQ [n] (lastr: f32, ws: [n]f32): f32 =
  loop (lastr) for i < n do nextrQ(lastr, ws[i])

let mc1step(wps: []f32): f32 =
  seqRedSumP(r0(), wps)

let mc1 [n] (wpss: [n][]f32): [n]f32 =
  map mc1step wpss

let mc2step (wqs: []f32) (r1: f32): f32 =
  seqRedSumQ(r1, wqs)

let mc2sim [tn][k] (arg: ([tn][k]f32, f32)): f32 =
  let ( wqss, r1 ) = arg
  let sum_r = map2 mc2step wqss (replicate tn r1) in
  mean(sum_r)

let mc2(wqsss: [][][]f32, r1s: []f32): []f32 =
  map mc2sim (zip wqsss r1s)

let main(wpss: [][]f32, wqsss: [][][]f32): []f32 = --mc1(wpss)
  mc2(wqsss, mc1(wpss))
