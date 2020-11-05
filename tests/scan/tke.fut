type DTYPE = f32
let INNER_DIM : i32 = 115

let tridagPar [n] (a:  [n]DTYPE, b: [n]DTYPE, c: [n]DTYPE, y: [n]DTYPE ): *[n]DTYPE =
  #[unsafe]
  ----------------------------------------------------
  -- Recurrence 1: b[i] = b[i] - a[i]*c[i-1]/b[i-1] --
  --   solved by scan with 2x2 matrix mult operator --
  ----------------------------------------------------
  let b0   = b[0]
  let mats = map  (\(i: i32): (DTYPE,DTYPE,DTYPE,DTYPE)  ->
                     if 0 < i
                     then (b[i], 0.0-a[i]*c[i-1], 1.0, 0.0)
                     else (1.0,  0.0,             0.0, 1.0))
                  (iota n)
  let scmt = scan (\(a0,a1,a2,a3) (b0,b1,b2,b3) ->
                     let value = 1.0/(a0*b0)
                     in ( (b0*a0 + b1*a2)*value,
                          (b0*a1 + b1*a3)*value,
                          (b2*a0 + b3*a2)*value,
                          (b2*a1 + b3*a3)*value))
                  (1.0,  0.0, 0.0, 1.0) mats
  let b    = map (\(t0,t1,t2,t3) ->
                    (t0*b0 + t1) / (t2*b0 + t3))
                 scmt
  ------------------------------------------------------
  -- Recurrence 2: y[i] = y[i] - (a[i]/b[i-1])*y[i-1] --
  --   solved by scan with linear func comp operator  --
  ------------------------------------------------------
  let y0   = y[0]
  let lfuns= map  (\(i: i32): (DTYPE,DTYPE)  ->
                     if 0 < i
                     then (y[i], 0.0-a[i]/b[i-1])
                     else (0.0,  1.0))
                  (iota n)
  let cfuns = scan (\(a: (DTYPE,DTYPE)) (b: (DTYPE,DTYPE)): (DTYPE,DTYPE)  ->
                     let (a0,a1) = a
                     let (b0,b1) = b
                     in ( b0 + b1*a0, a1*b1 ))
                  (0.0, 1.0) lfuns
  let y    = map (\(tup: (DTYPE,DTYPE)): DTYPE  ->
                    let (a,b) = tup
                    in a + b*y0)
                 cfuns
  ------------------------------------------------------
  -- Recurrence 3: backward recurrence solved via     --
  --             scan with linear func comp operator  --
  ------------------------------------------------------
  let yn   = y[n-1]/b[n-1]
  let lfuns= map (\(k: i32): (DTYPE,DTYPE)  ->
                    let i = n-k-1
                    in  if   0 < k
                        then (y[i]/b[i], 0.0-c[i]/b[i])
                        else (0.0,       1.0))
                 (iota n)
  let cfuns= scan (\(a: (DTYPE,DTYPE)) (b: (DTYPE,DTYPE)): (DTYPE,DTYPE)  ->
                     let (a0,a1) = a
                     let (b0,b1) = b
                     in (b0 + b1*a0, a1*b1))
                  (0.0, 1.0) lfuns
  let y    = map (\(tup: (DTYPE,DTYPE)): DTYPE  ->
                    let (a,b) = tup
                    in a + b*yn)
                 cfuns
  let y    = map (\i -> y[n-i-1]) (iota n)
  in y

-- ==
-- entry: tridagNested tridagNestedConst tridagNestedSeqConst tridagNestedSeq
--
-- compiled random input { [57][15]f32 [57][15]f32 [57][15]f32 [57][15]f32 }
entry tridagNested [n][m] (a: [n][m]DTYPE) (b: [n][m]DTYPE) (c: [n][m]DTYPE) (y: [n][m]DTYPE): *[n][m]DTYPE =
   map4 (\a b c y -> tridagPar (a,b,c,y)) a b c y

entry tridagNestedConst [n] (a: [n][INNER_DIM]DTYPE) (b: [n][INNER_DIM]DTYPE) (c: [n][INNER_DIM]DTYPE) (y: [n][INNER_DIM]DTYPE): *[n][INNER_DIM]DTYPE =
   map4 (\a b c y -> tridagPar (a,b,c,y)) a b c y



let tridagSeq [m] (a:  [m]DTYPE, b: [m]DTYPE, c: [m]DTYPE, y: [m]DTYPE ): *[m]DTYPE =
   let cp = map (\i -> if i==0 then c[0]/b[0] else 0) (indices c)
   let yp = map (\i -> if i==0 then y[0]/b[0] else 0) (indices y)
   let (cp_full, yp_full) = 
      loop (cp, yp) for i in 1..<m do
         let norm_factor = 1.0 / (b[i] - a[i] * cp[i-1])
         let cp[i] = c[i] * norm_factor
         let yp[i] = (y[i] - a[i] * yp[i-1]) * norm_factor
         in (cp, yp)
   let solution = replicate m (0.0 : DTYPE)
   let solution[m-1] = yp_full[m-1]
   let inds = reverse (init (indices y))
   in loop (solution) for i in inds do
      let solution[i] = yp_full[i] - cp_full[i] * solution[i+1]
      in solution      

entry tridagNestedSeqConst [n] (a: [n][INNER_DIM]DTYPE) (b: [n][INNER_DIM]DTYPE) (c: [n][INNER_DIM]DTYPE) (y: [n][INNER_DIM]DTYPE): *[n][INNER_DIM]DTYPE =
   #[sequential_inner]
   map4 (\a b c y -> tridagSeq (a,b,c,y)) a b c y

entry tridagNestedSeq [n][m] (a: [n][m]DTYPE) (b: [n][m]DTYPE) (c: [n][m]DTYPE) (y: [n][m]DTYPE): *[n][m]DTYPE =
   #[sequential_inner]
   map4 (\a b c y -> tridagSeq (a,b,c,y)) a b c y

