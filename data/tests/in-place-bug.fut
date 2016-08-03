-- This test was derived from CalibVolDiff and exposed a bug in
-- in-place-lowering (after the maps had been turned into do-loops).
-- ==

fun []f64 tridagSeq( []f64 a, []f64 b, []f64 c, []f64 y ) =
  copy(concat(a,b,c,y))

fun [][]f64 explicitMethod( [][m]f64 myD,  [][]f64 myDD,
                              [][]f64 myMu, [][]f64 myVar, [][]f64 result ) =
  copy( map( fn []f64 ( ([]f64,[]f64,[]f64) tup ) =>
               let (mu_row, var_row, result_row) = tup in
               map( fn f64 (([]f64, []f64, f64, f64, int) tup) =>
                      let ( dx, dxx, mu, var, j ) = tup in
                      ( mu*dx[1] + 0.5*var*dxx[1] ) * result_row[j]
                  , zip( myD, myDD, mu_row, var_row, iota(m) )
                  )
           , zip(myMu, myVar, result)))

fun [][]f64 implicitMethod( [][]f64 myD,  [][]f64 myDD,
                              [][]f64 myMu, [][]f64 myVar,
                             [][]f64 u,    f64     dtInv  ) =
  map( fn []f64 ( ([]f64,[]f64,[]f64) tup )  =>
         let (mu_row,var_row,u_row) = tup in
         let abc = map( fn (f64,f64,f64) ((f64,f64,[]f64,[]f64) tup) =>
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

fun f64 main(int numX, int numY, int numT, f64 s0, f64 strike, f64 t, f64 alpha, f64 nu, f64 beta) =
    let myX = map(f64, iota(numX)) in
    let myY = map(f64, iota(numY)) in
    let (myDx, myDxx) = (empty([]f64), empty([]f64)) in
    let (myDy, myDyy) = (empty([]f64), empty([]f64)) in
    let myResult = copy(empty([]f64)) in
    let myMuX  = replicate(numY, replicate(numX, 0.0)) in
    let myVarX = map(fn []f64 (f64 yj) => map(exp64, myX ), myY) in

    -- explicitX
    let u = explicitMethod( myDx, myDxx, myMuX, myVarX, myResult ) in
    -- implicitX
    let u = implicitMethod( myDx, myDxx, myMuX, myVarX, u, 1.0 ) in
    -- implicitY
    let y = map( fn []f64 ([]f64 u_row) =>
                   map(+1.0, u_row),
                   transpose(u)) in
    y[0,0]
