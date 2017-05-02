-- This test was derived from CalibVolDiff and exposed a bug in
-- in-place-lowering (after the maps had been turned into do-loops).
-- ==

import "/futlib/math"
import "/futlib/array"

let tridagSeq(a:  []f64, b: []f64, c: []f64, y: []f64 ): []f64 =
  copy(concat a b c y)

let explicitMethod(myD:  [][#m]f64,  myDD: [][]f64,
                              myMu: [][]f64, myVar: [][]f64, result: [][]f64 ): [][]f64 =
  copy( map (\(tup:  ([]f64,[]f64,[]f64) ): []f64  ->
               let (mu_row, var_row, result_row) = tup in
               map (\(tup: ([]f64, []f64, f64, f64, i32)): f64  ->
                      let ( dx, dxx, mu, var, j ) = tup in
                      ( mu*dx[1] + 0.5*var*dxx[1] ) * result_row[j]
                  ) (zip myD myDD (mu_row) (var_row) (iota(m) )
                  )
           ) (zip myMu myVar result))

let implicitMethod(myD:  [][]f64,  myDD: [][]f64,
                              myMu: [][]f64, myVar: [][]f64,
                             u: [][]f64,    dtInv: f64  ): [][]f64 =
  map (\(tup:  ([]f64,[]f64,[]f64) ): []f64   ->
         let (mu_row,var_row,u_row) = tup
         let abc = map (\(tup: (f64,f64,[]f64,[]f64)): (f64,f64,f64)  ->
                          let (mu, var, d, dd) = tup in
                          ( 0.0   - 0.5*(mu*d[0] + 0.5*var*dd[0])
                          , dtInv - 0.5*(mu*d[1] + 0.5*var*dd[1])
                          , 0.0   - 0.5*(mu*d[2] + 0.5*var*dd[2])
                          )
                      ) (zip (mu_row) (var_row) myD myDD
                      )
         let (a,b,c) = unzip(abc) in
         tridagSeq( a, copy(b), c, u_row )
     ) (zip myMu myVar u
     )

let main(numX: i32, numY: i32, numT: i32, s0: f64, strike: f64, t: f64, alpha: f64, nu: f64, beta: f64): f64 =
    let myX = map f64 (iota(numX))
    let myY = map f64 (iota(numY))
    let (myDx, myDxx) = (empty([]f64), empty([]f64))
    let (myDy, myDyy) = (empty([]f64), empty([]f64))
    let myResult = copy(empty([]f64))
    let myMuX  = replicate numY (replicate numX 0.0)
    let myVarX = map (\(yj: f64): []f64  -> map f64.exp myX) myY

    -- explicitX
    let u = explicitMethod( myDx, myDxx, myMuX, myVarX, myResult )
    -- implicitX
    let u = implicitMethod( myDx, myDxx, myMuX, myVarX, u, 1.0 )
    -- implicitY
    let y = map (\(u_row: []f64): []f64  ->
                   map (+1.0) (u_row))
                (rearrange (1,0) u) in
    y[0,0]
