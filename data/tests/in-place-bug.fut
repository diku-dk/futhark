// This test was derived from CalibVolDiff and exposed a bug in
// in-place-lowering (after the maps had been turned into do-loops).
// --

fun [real] tridagSeq( [real] a, [real] b, [real] c, [real] y ) =
  copy(concat(a,b,c,y))

fun [[real]] explicitMethod( [[real]] myD,  [[real]] myDD,
                              [[real]] myMu, [[real]] myVar, [[real]] result ) =
  let m = size(0,myD) in
  copy( map( fn [real] ( {[real],[real],[real]} tup ) =>
               let {mu_row, var_row, result_row} = tup in
               map( fn real ({[real], [real], real, real, int} tup) =>
                      let { dx, dxx, mu, var, j } = tup in
                      ( mu*dx[1] + 0.5*var*dxx[1] ) * result_row[j]
                  , zip( myD, myDD, mu_row, var_row, iota(m) )
                  )
           , zip(myMu, myVar, result)))

fun [[real]] implicitMethod( [[real]] myD,  [[real]] myDD,
                              [[real]] myMu, [[real]] myVar,
                             [[real]] u,    real     dtInv  ) =
  map( fn [real] ( {[real],[real],[real]} tup )  =>
         let {mu_row,var_row,u_row} = tup in
         let abc = map( fn {real,real,real} ({real,real,[real],[real]} tup) =>
                          let {mu, var, d, dd} = tup in
                          { 0.0   - 0.5*(mu*d[0] + 0.5*var*dd[0])
                          , dtInv - 0.5*(mu*d[1] + 0.5*var*dd[1])
                          , 0.0   - 0.5*(mu*d[2] + 0.5*var*dd[2])
                          }
                      , zip(mu_row, var_row, myD, myDD)
                      ) in
         let {a,b,c} = unzip(abc) in
         tridagSeq( a, copy(b), c, u_row )
     , zip(myMu,myVar,u)
     )

fun real main(int numX, int numY, int numT, real s0, real strike, real t, real alpha, real nu, real beta) =
    let myX = map(toFloat, iota(numX)) in
    let myY = map(toFloat, iota(numY)) in
    let {myDx, myDxx} = {empty([real]), empty([real])} in
    let {myDy, myDyy} = {empty([real]), empty([real])} in
    let myResult = copy(empty([real])) in
    let myMuX  = replicate(numY, replicate(numX, 0.0)) in
    let myVarX = map(fn [real] (real yj) => map(exp, myX ), myY) in

    // explicitX
    let u = explicitMethod( myDx, myDxx, myMuX, myVarX, myResult ) in
    // implicitX
    let u = implicitMethod( myDx, myDxx, myMuX, myVarX, u, 1.0 ) in
    // implicitY
    let y = map( fn [real] ([real] u_row) =>
                   map(+1.0, u_row),
                   transpose(u)) in
    y[0,0]
