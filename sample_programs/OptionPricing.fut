-- Generic pricing
-- ==
-- compiled input @ OptionPricing-data/small.in
-- output @ OptionPricing-data/small.out
--
-- compiled input @ OptionPricing-data/medium.in
-- output @ OptionPricing-data/medium.out
--
-- compiled input @ OptionPricing-data/large.in
-- output @ OptionPricing-data/large.out

def grayCode(x: i32): i32 = (x >> 1) ^ x

----------------------------------------
--- Sobol Generator
----------------------------------------
def testBit(n: i32, ind: i32): bool =
  let t = (1 << ind) in (n & t) == t

-----------------------------------------------------------------
---- INDEPENDENT FORMULA:
----    filter is redundantly computed inside map.
----    Currently Futhark hoists it outside, but this will
----    not allow fusing the filter with reduce -> redomap,
-----------------------------------------------------------------
def xorInds [num_bits] (n: i32) (dir_vs: [num_bits]i32): i32 =
  let reldv_vals = map2 (\dv i -> if testBit(grayCode n,i32.i64 i) then dv else 0)
                        dir_vs (indices dir_vs)
  in reduce (^) 0 reldv_vals

def sobolIndI [m][num_bits] (dir_vs: [m][num_bits]i32, n: i32): [m]i32 =
  map (xorInds n) dir_vs

def sobolIndR [m][num_bits] (dir_vs: [m][num_bits]i32) (n: i32): [m]f32 =
  let divisor = 2.0 ** f32.i64(num_bits)
  let arri    = sobolIndI( dir_vs, n )
  in map (\x -> f32.i32(x) / divisor) arri

--------------------------------
---- STRENGTH-REDUCED FORMULA
--------------------------------
def index_of_least_significant_0(x: i32): i32 =
  loop i = 0 while i < 32 && ((x>>i)&1) != 0 do i + 1

def sobolRecI [num_bits][n] (sob_dir_vs: [n][num_bits]i32, prev: [n]i32, x: i32): [n]i32 =
  let bit = index_of_least_significant_0 x
  in map2 (\vct_row prev -> vct_row[bit] ^ prev) sob_dir_vs prev

def recM [n][num_bits] (sob_dirs:  [n][num_bits]i32, i: i32 ): [n]i32 =
  let bit = index_of_least_significant_0 i
  in #[unsafe] sob_dirs[:,bit]

def sobolRecMap [n][num_bits] (sob_fact:  f32, dir_vs: [n][num_bits]i32, (lb_inc, ub_exc): (i32,i32) ): [][]f32 =
  -- the if inside may be particularly ugly for
  -- flattening since it introduces control flow!
  let contribs = map (\k -> if k==0
                            then sobolIndI(dir_vs,lb_inc+1)
                            else recM(dir_vs,k+lb_inc))
                     (map i32.i64 (iota (i64.i32 (ub_exc-lb_inc))))
  let vct_ints = scan (map2 (^)) (replicate n 0) contribs
  in  map (\xs -> map (\x -> f32.i32 x * sob_fact) xs) vct_ints

def sobolReci2 [n][num_bits] (sob_dirs: [n][num_bits]i32, prev: [n]i32, i: i32): [n]i32=
  let col = recM(sob_dirs, i)
  in map2 (^) prev col

-- computes sobol numbers: n,..,n+chunk-1
def sobolChunk [len][num_bits] (dir_vs: [len][num_bits]i32) (n: i32) (chunk: i64): [chunk][len]f32 =
  let sob_fact= 1.0 / f32.i64(1 << num_bits)
  let sob_beg = sobolIndI(dir_vs, n+1)
  let contrbs = map (\(k: i64): [len]i32  ->
                       if k==0 then sob_beg
                       else recM(dir_vs, i32.i64 k+n))
                    (iota chunk)
  let vct_ints= scan (map2 (^)) (replicate len 0) contrbs
  in map (\xs -> map (\x -> f32.i32(x) * sob_fact) xs) vct_ints

----------------------------------------
--- Inverse Gaussian
----------------------------------------
def polyAppr(x: f32,
             a0: f32, a1: f32, a2: f32, a3: f32,
             a4: f32, a5: f32, a6: f32, a7: f32,
             b0: f32, b1: f32, b2: f32, b3: f32,
             b4: f32, b5: f32, b6: f32, b7: f32
            ): f32 =
  (x*(x*(x*(x*(x*(x*(x*a7+a6)+a5)+a4)+a3)+a2)+a1)+a0) /
  (x*(x*(x*(x*(x*(x*(x*b7+b6)+b5)+b4)+b3)+b2)+b1)+b0)

def smallcase(q: f32): f32 =
  q * polyAppr( 0.180625 - q * q,

                3.387132872796366608,
                133.14166789178437745,
                1971.5909503065514427,
                13731.693765509461125,
                45921.953931549871457,
                67265.770927008700853,
                33430.575583588128105,
                2509.0809287301226727,

                1.0,
                42.313330701600911252,
                687.1870074920579083,
                5394.1960214247511077,
                21213.794301586595867,
                39307.89580009271061,
                28729.085735721942674,
                5226.495278852854561
              )

def intermediate(r: f32): f32 =
  polyAppr( r - 1.6,

            1.42343711074968357734,
            4.6303378461565452959,
            5.7694972214606914055,
            3.64784832476320460504,
            1.27045825245236838258,
            0.24178072517745061177,
            0.0227238449892691845833,
            7.7454501427834140764e-4,

            1.0,
            2.05319162663775882187,
            1.6763848301838038494,
            0.68976733498510000455,
            0.14810397642748007459,
            0.0151986665636164571966,
            5.475938084995344946e-4,
            1.05075007164441684324e-9
          )

def tail(r: f32): f32 =
  polyAppr( r - 5.0,

            6.6579046435011037772,
            5.4637849111641143699,
            1.7848265399172913358,
            0.29656057182850489123,
            0.026532189526576123093,
            0.0012426609473880784386,
            2.71155556874348757815e-5,
            2.01033439929228813265e-7,

            1.0,
            0.59983220655588793769,
            0.13692988092273580531,
            0.0148753612908506148525,
            7.868691311456132591e-4,
            1.8463183175100546818e-5,
            1.4215117583164458887e-7,
            2.04426310338993978564e-5
          )

def ugaussianEl(p: f32): f32 =
  let dp = p - 0.5
  in  --if  ( fabs(dp) <= 0.425 )
  if ( ( (dp < 0.0 ) && (0.0 - dp <= 0.425) ) ||
       ( (0.0 <= dp) && (dp <= 0.425)       )  )
  then smallcase(dp)
  else let pp = if(dp < 0.0) then dp + 0.5
                else 0.5 - dp
       let r  = f32.sqrt( - f32.log(pp) )
       let x = if(r <= 5.0) then intermediate(r)
               else tail(r)
       in if(dp < 0.0) then 0.0 - x else x

-- Transforms a uniform distribution [0,1)
-- into a gaussian distribution (-inf, +inf)
def ugaussian [n] (ps: [n]f32): [n]f32 = map ugaussianEl ps


---------------------------------
--- Brownian Bridge
---------------------------------
def brownianBridgeDates [num_dates]
                        (bb_inds: [3][num_dates]i32)
                        (bb_data: [3][num_dates]f32)
                        (gauss: [num_dates]f32): [num_dates]f32 =
  let bi = bb_inds[0]
  let li = bb_inds[1]
  let ri = bb_inds[2]
  let sd = bb_data[0]
  let lw = bb_data[1]
  let rw = bb_data[2]
  let bbrow = replicate num_dates 0.0
  let bbrow[ bi[0]-1 ] = sd[0] * gauss[0]
  let bbrow = loop bbrow for i in 1..<num_dates do
                #[unsafe]
  let j  = li[i] - 1
  let k  = ri[i] - 1
  let l  = bi[i] - 1
  let wk = bbrow[k]
  let zi = gauss[i]
  let tmp= rw[i] * wk + sd[i] * zi
  let bbrow[ l ] = if j == -1
                   then tmp
                   else tmp + lw[i] * bbrow[j]
  in  bbrow

  -- This can be written as map-reduce, but it
  --   needs delayed arrays to be mapped nicely!
  in loop bbrow for ii in 1..<num_dates do
       #[unsafe]
let i = num_dates - ii
let bbrow[i] = bbrow[i] - bbrow[i-1]
in  bbrow

def brownianBridge [num_dates][k]
                   (num_und: i64)
                   (bb_inds: [3][num_dates]i32)
                   (bb_data: [3][num_dates]f32)
                   (gaussian_arr: [k]f32)
                   : [num_dates][num_und]f32 =
  let gauss2d  = unflatten num_dates num_und gaussian_arr
  let gauss2dT = transpose gauss2d
  in transpose (map (brownianBridgeDates bb_inds bb_data) gauss2dT)


---------------------------------
--- Black-Scholes
---------------------------------

def correlateDeltas [num_und][num_dates]
                   (md_c:  [num_und][num_und]f32,
                    zds:   [num_dates][num_und]f32)
                   : [num_dates][num_und]f32 =
  map (\zi: [num_und]f32  ->
         map (\(j: i64): f32  ->
                let x = map2 (*)
                             (#[unsafe] take (j+1) zi)
                             (#[unsafe] take (j+1) md_c[j])
                in  f32.sum x)
             (iota num_und))
      zds

def combineVs [num_und]
             (n_row:   [num_und]f32)
             (vol_row: [num_und]f32)
             (dr_row:  [num_und]f32): [num_und]f32 =
  map2 (+) dr_row (map2 (*) n_row vol_row)

def mkPrices [num_und][num_dates]
            (md_starts:    [num_und]f32,
             md_vols: [num_dates][num_und]f32,
             md_drifts: [num_dates][num_und]f32,
             noises: [num_dates][num_und]f32)
            : [num_dates][num_und]f32 =
  let c_rows = map3 combineVs noises md_vols md_drifts
  let e_rows = map (\x: [num_und]f32 -> map f32.exp x) c_rows
  in  map (map2 (*) md_starts) (scan (map2 (*)) (replicate num_und 1.0) e_rows)

def blackScholes [num_dates][num_und]
                (bb_arr: [num_dates][num_und]f32)
                (md_c: [num_und][num_und]f32)
                (md_vols: [num_dates][num_und]f32)
                (md_drifts: [num_dates][num_und]f32)
                (md_starts: [num_und]f32)
                : [num_dates][num_und]f32 =
  let noises = correlateDeltas(md_c, bb_arr)
  in  mkPrices(md_starts, md_vols, md_drifts, noises)

----------------------------------------
-- PAYOFF FUNCTIONS
----------------------------------------

def fminPayoff(xs: []f32): f32 =
  --    MIN( map(/, xss, {3758.05, 11840.0, 1200.0}) )
  let (a,b,c) = (xs[0]/3758.05, xs[1]/11840.0, xs[2]/1200.0)
  in if a < b
     then if a < c then a else c
     else if b < c then b else c

def trajInner(amount: f32, ind: i32, disc: []f32): f32 = amount * #[unsafe] disc[ind]

def payoff1(md_disct: []f32, md_detval: []f32, xss: [1][1]f32): f32 =
  let detval = #[unsafe] md_detval[0]
  let amount = ( xss[0,0] - 4000.0 ) * detval
  let amount0= if (0.0 < amount) then amount else 0.0
  in  trajInner(amount0, 0, md_disct)

def payoff2 (md_disc: []f32, xss: [5][3]f32): f32 =
  let (date, amount) =
    if 1.0 <= fminPayoff(xss[0]) then (0, 1150.0) else
    if 1.0 <= fminPayoff(xss[1]) then (1, 1300.0) else
    if 1.0 <= fminPayoff(xss[2]) then (2, 1450.0) else
    if 1.0 <= fminPayoff(xss[3]) then (3, 1600.0) else
    let x50  = fminPayoff(xss[4])
    let value  = if 1.0 <= x50 then 1750.0
                 else if 0.75 < x50 then 1000.0
                 else x50*1000.0
    in (4, value)
  in trajInner(amount, date, md_disc)

def payoff3(md_disct: []f32, xss: [367][3]f32): f32 =
  let conds  = map (\x ->
                      x[0] <= 2630.6349999999998 ||
                      x[1] <= 8288.0             ||
                      x[2] <=  840.0)
                   xss
  let cond  = or conds
  let price1= trajInner(100.0,  0, md_disct)
  let goto40= cond && ( xss[366,0] < 3758.05 ||
                        xss[366,1] < 11840.0 ||
                        xss[366,2] < 1200.0)
  let amount= if goto40
              then 1000.0 * fminPayoff(xss[366])
              else 1000.0
  let price2 = trajInner(amount, 1, md_disct)
  in price1 + price2


def genericPayoff(contract: i32) (md_disct: []f32) (md_detval: []f32) (xss: [][]f32): f32 =
  if      contract == 1 then #[unsafe] payoff1(md_disct, md_detval, xss :> [1][1]f32)
  else if contract == 2 then #[unsafe] payoff2(md_disct, xss :> [5][3]f32)
  else if contract == 3 then #[unsafe] payoff3(md_disct, xss :> [367][3]f32)
  else 0.0

-- Entry point
def main [k][num_bits][num_models][num_und][num_dates][num_discts]
        (contract_number: i32)
        (num_mc_it: i32)
        (dir_vs: [k][num_bits]i32)
        (md_cs: [num_models][num_und][num_und]f32)
        (md_vols: [num_models][num_dates][num_und]f32)
        (md_drifts: [num_models][num_dates][num_und]f32)
        (md_sts: [num_models][num_und]f32)
        (md_detvals: [num_models][1]f32)
        (md_discts: [num_models][num_discts]f32)
        (bb_inds: [3][num_dates]i32)
        (bb_data: [3][num_dates]f32)
         : []f32 =
  let sobvctsz  = num_dates*num_und
  let dir_vs = dir_vs :> [sobvctsz][num_bits]i32
  let sobol_mat = map_stream (\chunk (ns: [chunk]i32): [chunk][sobvctsz]f32  ->
                                sobolChunk dir_vs (#[unsafe] ns[0]) chunk)
                             (map i32.i64 (iota (i64.i32 num_mc_it)))
  let gauss_mat = map ugaussian sobol_mat
  let bb_mat    = map (brownianBridge num_und bb_inds bb_data) gauss_mat
  let payoffs   = map (\bb_row: [num_models]f32  ->
                         let bd_row = map4 (blackScholes bb_row) md_cs md_vols md_drifts md_sts
                         in map3 (genericPayoff contract_number) md_discts md_detvals bd_row)
                      bb_mat
  let payoff    = #[sequential_inner] reduce (map2 (+)) (replicate num_models 0.0) payoffs
  in  map (/f32.i32 num_mc_it) payoff
