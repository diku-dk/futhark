-- A map of a parallel tridag.  This is intended to test
-- parallelisation of the inner scans and maps.  The real test for
-- this is LocVolCalib.
--
-- ==
-- compiled input { 1000i64 256i64 }
--
-- output { [0.010000f32, 0.790000f32, 2.660000f32,
-- 21474836.000000f32, 21474836.000000f32, 21474836.000000f32,
-- 21474836.000000f32, 21474836.000000f32, 21474836.000000f32,
-- 5625167.000000f32] }
--
-- no_python compiled input { 100i64 2560i64 }
--
-- output { [0.000000f32, 0.120000f32, 0.260000f32, 0.430000f32,
-- 0.620000f32, 0.840000f32, 1.110000f32, 1.440000f32, 1.840000f32,
-- 2.360000f32] }
--
-- no_python compiled input { 10i64 25600i64 }
--
-- output { [0.000000f32, 0.110000f32, 0.250000f32, 0.410000f32,
-- 0.590000f32, 0.800000f32, 1.040000f32, 1.340000f32, 1.710000f32,
-- 2.170000f32] }

def tridagPar [n] (a: [n]f32, b: []f32, c: []f32, y: []f32) : *[n]f32 =
  ----------------------------------------------------
  -- Recurrence 1: b[i] = b[i] - a[i]*c[i-1]/b[i-1] --
  --   solved by scan with 2x2 matrix mult operator --
  ----------------------------------------------------
  let b0 = b[0]
  let mats =
    map (\(i: i32) : (f32, f32, f32, f32) ->
           if 0 < i
           then (b[i], 0.0 - a[i] * c[i - 1], 1.0, 0.0)
           else (1.0, 0.0, 0.0, 1.0))
        (map i32.i64 (iota n))
  let scmt =
    scan (\(a: (f32, f32, f32, f32)) (b: (f32, f32, f32, f32)) : (f32, f32, f32, f32) ->
            let (a0, a1, a2, a3) = a
            let (b0, b1, b2, b3) = b
            let value = 1.0 / (a0 * b0)
            in ( (b0 * a0 + b1 * a2) * value
               , (b0 * a1 + b1 * a3) * value
               , (b2 * a0 + b3 * a2) * value
               , (b2 * a1 + b3 * a3) * value
               ))
         (1.0, 0.0, 0.0, 1.0)
         mats
  let b =
    map (\(tup: (f32, f32, f32, f32)) : f32 ->
           let (t0, t1, t2, t3) = tup
           in (t0 * b0 + t1) / (t2 * b0 + t3))
        scmt
  ------------------------------------------------------
  -- Recurrence 2: y[i] = y[i] - (a[i]/b[i-1])*y[i-1] --
  --   solved by scan with linear func comp operator  --
  ------------------------------------------------------
  let y0 = y[0]
  let lfuns =
    map (\(i: i32) : (f32, f32) ->
           if 0 < i
           then (y[i], 0.0 - a[i] / b[i - 1])
           else (0.0, 1.0))
        (map i32.i64 (iota n))
  let cfuns =
    scan (\(a: (f32, f32)) (b: (f32, f32)) : (f32, f32) ->
            let (a0, a1) = a
            let (b0, b1) = b
            in (b0 + b1 * a0, a1 * b1))
         (0.0, 1.0)
         lfuns
  let y =
    map (\(tup: (f32, f32)) : f32 ->
           let (a, b) = tup
           in a + b * y0)
        cfuns
  ------------------------------------------------------
  -- Recurrence 3: backward recurrence solved via     --
  --             scan with linear func comp operator  --
  ------------------------------------------------------
  let yn = y[n - 1] / b[n - 1]
  let lfuns =
    map (\(k: i32) : (f32, f32) ->
           let i = i32.i64 n - k - 1
           in if 0 < k
              then (y[i] / b[i], 0.0 - c[i] / b[i])
              else (0.0, 1.0))
        (map i32.i64 (iota n))
  let cfuns =
    scan (\(a: (f32, f32)) (b: (f32, f32)) : (f32, f32) ->
            let (a0, a1) = a
            let (b0, b1) = b
            in (b0 + b1 * a0, a1 * b1))
         (0.0, 1.0)
         lfuns
  let y =
    map (\(tup: (f32, f32)) : f32 ->
           let (a, b) = tup
           in a + b * yn)
        cfuns
  let y = map (\i : f32 -> y[n - i - 1]) (iota n)
  in y

def map_tridag_par [inner] [outer]
                   ( myD: [inner][3]f32
                   , myDD: [inner][3]f32
                   , myMu: [outer][inner]f32
                   , myVar: [outer][inner]f32
                   , u: [outer][inner]f32
                   , dtInv: f32
                   ) : *[][]f32 =
  map3 (\mu_row var_row u_row ->
          let (a, b, c) =
            unzip3 (map4 (\mu var d dd : (f32, f32, f32) ->
                            ( 0.0 - 0.5 * (mu * d[0] + 0.5 * var * dd[0])
                            , dtInv - 0.5 * (mu * d[1] + 0.5 * var * dd[1])
                            , 0.0 - 0.5 * (mu * d[2] + 0.5 * var * dd[2])
                            ))
                         mu_row
                         var_row
                         myD
                         myDD)
          in tridagPar (a, b, c, u_row))
       myMu
       myVar
       u

-- To avoid floating-point jitter.
def trunc2dec (x: f32) =
  f32.abs (f32.i32 (i32.f32 (x * 100.0)) / 100.0)

def main (outer: i64) (inner: i64) =
  let myD = replicate inner [0.10, 0.20, 0.30]
  let myDD = replicate inner [0.20, 0.30, 0.40]
  let scale (s: i64) (x: i64) = f32.i64 (s + x) / f32.i64 inner
  let scale_row (s: i64) (i: i64) (row: [inner]i64) = map (scale (s + i)) row
  let myMu = map2 (scale_row 1) (iota outer) (replicate outer (iota inner))
  let myVar = map2 (scale_row 2) (iota outer) (replicate outer (iota inner))
  let u = map2 (scale_row 3) (iota outer) (replicate outer (iota inner))
  let dtInv = 0.8874528f32
  let res = map_tridag_par (myD, myDD, myMu, myVar, u, dtInv)
  in map (\i -> trunc2dec (res[i * (outer / 10), i * (inner / 10)])) (iota 10)
