-- An I/O-bound mc2 implementation.
--
-- Useful for verification with a "sequential" R implementation.
--
-- ==

-- Some useful (for mc2) Futhark extensions.
def sum (xs: []f32) : f32 = reduce (+) (0.0) xs
def mean [n] (xs: [n]f32) : f32 = sum (map (/ f32.i64 (n)) xs)

-- Vasicek model parameters.
def r0 () : f32 = 0.03

-- initial interest rate
def thetaP () : f32 = 0.03

-- short-rate mean
def thetaQ () : f32 = 0.045

-- long-rate mean
def kappa () : f32 = 0.1

-- speed of mean reversion
def sigma () : f32 = 0.01

-- interest rate volatility

def nextrP (lastr: f32, wp: f32) : f32 =
  lastr + kappa () * (thetaP () - lastr) + sigma () * wp

def nextrQ (lastr: f32, wq: f32) : f32 =
  lastr + kappa () * (thetaQ () - lastr) + sigma () * wq

def seqRedSumP [n] (lastr: f32, ws: [n]f32) : f32 =
  loop (lastr) for i < n do nextrP (lastr, ws[i])

def seqRedSumQ [n] (lastr: f32, ws: [n]f32) : f32 =
  loop (lastr) for i < n do nextrQ (lastr, ws[i])

def mc1step (wps: []f32) : f32 =
  seqRedSumP (r0 (), wps)

def mc1 [n] (wpss: [n][]f32) : [n]f32 =
  map mc1step wpss

def mc2step (wqs: []f32) (r1: f32) : f32 =
  seqRedSumQ (r1, wqs)

def mc2sim [tn] [k] (arg: ([tn][k]f32, f32)) : f32 =
  let (wqss, r1) = arg
  let sum_r = map2 mc2step wqss (replicate tn r1)
  in mean (sum_r)

def mc2 (wqsss: [][][]f32, r1s: []f32) : []f32 =
  map mc2sim (zip wqsss r1s)

def main (wpss: [][]f32, wqsss: [][][]f32) : []f32 =
  --mc1(wpss)
  mc2 (wqsss, mc1 (wpss))
