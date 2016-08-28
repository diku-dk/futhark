-- This test was derived from CalibVolDiff and exposed a bug in
-- in-place-lowering (after the maps had been turned into do-loops).
-- ==

fun tridagSeq(a:  []f64, b: []f64, c: []f64, y: []f64 ): []f64 =
  copy(concat(a,b,c,y))

fun explicitMethod(myD:  [][m]f64,  myDD: [][]f64,
                              myMu: [][]f64, myVar: [][]f64, result: [][]f64 ): [][]f64 =
  copy( map( fn (tup:  ([]f64,[]f64,[]f64) ): []f64  =>
               let (mu_row, var_row, result_row) = tup in
               map( fn (tup: ([]f64, []f64, f64, f64, int)): f64  =>
                      let ( dx, dxx, mu, var, j ) = tup in
                      ( mu*dx[1] + 0.5*var*dxx[1] ) * result_row[j]
                  , zip( myD, myDD, mu_row, var_row, iota(m) )
                  )
           , zip(myMu, myVar, result)))

fun implicitMethod(myD:  [][]f64,  myDD: [][]f64,
                              myMu: [][]f64, myVar: [][]f64,
                             u: [][]f64,    dtInv: f64  ): [][]f64 =
  map( fn (tup:  ([]f64,[]f64,[]f64) ): []f64   =>
         let (mu_row,var_row,u_row) = tup in
         let abc = map( fn (tup: (f64,f64,[]f64,[]f64)): (f64,f64,f64)  =>
                          let (mu, var, d, dd) = tup in
                          ( 0.0   - 0.5*(mu*d[0] + 0.5*var*dd[0])
                          , dtInv - 0.5*(mu*d[1] + 0.5*var*dd[1])
                          , 0.0   - 0.5*(mu*d[2] + 0.5*var*dd[2])
                          )
                      , zip(mu_row, var_row, myD, myDD)
                      ) in
         let (a,b,c) = unzip(abc) in
         tridagSeq( a, copy(b), c, u_row )
     , zip(myMu,myVar,u)
     )

fun main(numX: int, numY: int, numT: int, s0: f64, strike: f64, t: f64, alpha: f64, nu: f64, beta: f64): f64 =
    let myX = map(f64, iota(numX)) in
    let myY = map(f64, iota(numY)) in
    let (myDx, myDxx) = (empty([]f64), empty([]f64)) in
    let (myDy, myDyy) = (empty([]f64), empty([]f64)) in
    let myResult = copy(empty([]f64)) in
    let myMuX  = replicate numY (replicate numX 0.0) in
    let myVarX = map(fn (yj: f64): []f64  => map(exp64, myX ), myY) in

    -- explicitX
    let u = explicitMethod( myDx, myDxx, myMuX, myVarX, myResult ) in
    -- implicitX
    let u = implicitMethod( myDx, myDxx, myMuX, myVarX, u, 1.0 ) in
    -- implicitY
    let y = map( fn (u_row: []f64): []f64  =>
                   map((+1.0), u_row),
                   transpose(u)) in
    y[0,0]
