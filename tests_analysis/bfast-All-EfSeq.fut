-- A "fused" version of bfast that; the creation of "X" and the batched matrix-matrix
-- multiplication are kept separate (otherwise performance is awaful), the remaining
-- steps up to and including the calculation of "hmax" is fused in one kernel.
-- ==
--
-- compiled input @ data/D1.in.gz
-- compiled input @ data/D2.in.gz
-- compiled input @ data/D3.in.gz
-- compiled input @ data/D4.in.gz
-- compiled input @ data/D5.in.gz
-- compiled input @ data/D6.in.gz
-- compiled input @ data/peru.in.gz
-- output @ data/peru.out.gz
-- compiled input @ data/africa.in.gz
-- output @ data/africa.out.gz

-- compiled input @ data/sahara.in.gz
-- output @ data/sahara.out.gz

let logplus (x: f32) : f32 =
  if x > (f32.exp 1)
  then f32.log x else 1

let adjustValInds [N] (n : i32) (ns : i32) (Ns : i32) (val_inds : [N]i32) (ind: i32) : i32 =
    if ind < Ns - ns then (#[unsafe] val_inds[ind+ns]) - n else -1

let filterPadWithKeys [n] 't
           (p : (t -> bool))
           (dummy : t)
           (arr : [n]t) : ([n](t,i64), i64) =
  let tfs = map (\a -> if p a then 1 else 0) arr
  let isT = scan (+) 0 tfs
  let i   = last isT
  let inds= map2 (\a iT -> if p a then iT-1 else -1) arr isT
  let rs  = scatter (replicate n dummy) inds arr
  let ks  = scatter (replicate n 0) inds (iota n)
  in  (zip rs ks, i)

-- | builds the X matrices; first result dimensions of size 2*k+2
let mkX_with_trend [N] (k2p2: i64) (f: f32) (mappingindices: [N]i64): [k2p2][N]f32 =
  map (\ i ->
        map (\ind ->
                if i == 0 then 1f32
                else if i == 1 then r32 <| i32.i64 ind
                else let (i', j') = (r32 <| i32.i64 (i / 2), r32 <| i32.i64 ind)
                     let angle = 2f32 * f32.pi * i' * j' / f
                     in  if i % 2 == 0 then f32.sin angle
                                       else f32.cos angle
            ) mappingindices
      ) (iota k2p2)

let mkX_no_trend [N] (k2p2m1: i64) (f: f32) (mappingindices: [N]i64): [k2p2m1][N]f32 =
  map (\ i ->
        map (\ind ->
                if i == 0 then 1f32
                else let i = i + 1
		     let (i', j') = (r32 <| i32.i64 (i / 2), r32 <| i32.i64 ind)
                     let angle = 2f32 * f32.pi * i' * j' / f
                     in  if i % 2 == 0 then f32.sin angle
                                       else f32.cos angle
            ) mappingindices
      ) (iota k2p2m1)

---------------------------------------------------
-- Adapted matrix inversion so that it goes well --
-- with intra-blockparallelism                   --
---------------------------------------------------

  let gauss_jordan [n][m] (A: *[n*m]f32): [n*m]f32 =
    loop A for i < n do
      let v1 = A[i]
      let A' = map (\ind -> let (k, j) = (ind / m, ind % m)
                            in if v1 == 0.0 then #[unsafe] A[k*m+j] else
                            let x = #[unsafe] (A[j] / v1) in
                                if k < n-1  -- Ap case
                                then #[unsafe] ( A[(k+1)*m+j] - A[(k+1)*m+i] * x )
                                else x      -- irow case
                   ) (iota (n*m))
      in  scatter A (iota (n*m)) A'

  let mat_inv [n] (A: [n][n]f32): [n][n]f32 =
    let m = 2*n
    -- Pad the matrix with the identity matrix.
    let Ap = map (\ind -> let (i, j) = (ind / m, ind % m)
                          in  if j < n then #[unsafe] ( A[i,j] )
                                       else if j == n+i
                                            then 1.0
                                            else 0.0
                 ) (iota (n*m))
    let Ap' = gauss_jordan Ap
    -- Drop the identity matrix at the front!
    in #[unsafe] ( (unflatten Ap')[0:n,n:2*n] ) :> [n][n]f32
--------------------------------------------------
--------------------------------------------------

let dotprod [n] (xs: [n]f32) (ys: [n]f32): f32 =
  reduce (+) 0.0 <| map2 (*) xs ys

let matvecmul_row [n][m] (xss: [n][m]f32) (ys: [m]f32) =
  map (dotprod ys) xss

let dotprod_filt [n] (vct: [n]f32) (xs: [n]f32) (ys: [n]f32) : f32 =
  loop (s) = (0.0) for i < n do
    s + xs[i] * ys[i] * if (f32.isnan (vct[i])) then 0.0 else 1.0

let matvecmul_row_filt [n][m] (xss: [n][m]f32) (ys: [m]f32) =
    map (\xs -> map2 (\x y -> if (f32.isnan y) then 0 else x*y) xs ys |> f32.sum) xss

let matmul_filt [n][p][m] (xss: [n][p]f32) (yss: [p][m]f32) (vct: [p]f32) : [n][m]f32 =
  map (\xs -> map (dotprod_filt vct xs) (transpose yss)) xss

----------------------------------------------------
----------------------------------------------------

-- | implementation is in this entry point
--   the outer map is distributed directly
entry main [m][N] (trend: i32) (k: i32) (n: i32) (freq: f32)
                  (hfrac: f32) (lam: f32)
                  (mappingindices : [N]i32)
                  (images : [m][N]f32) =
  ----------------------------------
  -- 1. make interpolation matrix --
  ----------------------------------
  let k2p2 = 2*k + 2
  let k2p2' = i64.i32 <| if trend > 0 then k2p2 else k2p2-1
  let mappingindices = map i64.i32 mappingindices
  let n = i64.i32 n
  let X = opaque <|
          if trend > 0
              then mkX_with_trend k2p2' freq mappingindices
          else mkX_no_trend   k2p2' freq mappingindices

  -- PERFORMANCE BUG: instead of `let Xt = copy (transpose X)`
  --   we need to write the following ugly thing to force manifestation:
  let zero = r32 <| i32.i64 <| (N*N + 2*N + 1) / (N + 1) - N - 1
  let Xt  = map (map (+zero)) (copy (transpose X))
            |> opaque

  --let BOUND = map (\q -> let t   = n+1+q
  --                       let time = #[unsafe] mappingindices[t-1]
  --                       let tmp = logplus ((r32 time) / (r32 mappingindices[N-1]))
  --                       in  lam * (f32.sqrt tmp)
  --                ) (iota (N-n))

  let Xh  =  (X[:,:n])
  let Xth =  (Xt[:n,:])
  in unzip <|
  map2 (\y ii -> if ii > 50000000 then (-1, 0.0f32) else
      let yh  = #[unsafe] y[:n]
      ----------------------------------
      -- 2. mat-mat multiplication    --
      ----------------------------------
      let Xsqr = matmul_filt Xh Xth yh

      ----------------------------------
      -- 3. matrix inversion          --
      ----------------------------------
      let Xinv = mat_inv Xsqr
      ---------------------------------------------
      -- 4. several matrix-vector multiplication --
      ---------------------------------------------
      let beta0  = matvecmul_row_filt Xh yh   -- [2k+2]

      let beta   = matvecmul_row Xinv beta0    -- [2k+2]

      let y_pred = matvecmul_row Xt   beta     -- [N]

      -- filter o redomap o redomap phase
      let (count, n', sigma0, yerr, keys) = (0, 0, 0, replicate N f32.nan, replicate N 0)
      let (N', n', sigma0, y_error, keys) =
        loop (count, n', sigma0, yerr, keys) for i < N do
          let yi = y[i] in
          if f32.isnan yi then (count, n', sigma0, yerr, keys)
          else let ye = yi - y_pred[i] in #[unsafe]
               let yerr[count] = ye
               let keys[count] = i32.i64 i
               let (n'', sigma0') = if i<n then (n'+1, sigma0+ye*ye) else (n', sigma0)
               in  (count+1, n'', sigma0', yerr, keys)
      let sigma = f32.sqrt ( sigma0 / (r32 (n' - k2p2)) )
      let h     = t32 ( (r32 n') * hfrac )
      -- last kernel
      let MO_fst =
        loop (acc) = (0.0) for i < h do
          acc + #[unsafe] y_error[i+n'-h+1]

      let (fst_break, mean, _) =
        loop (fst_break, mean, mo) = (-1,0.0,0.0)
          for i < N'-n' do
            let elm = if i==0 then MO_fst
                              else #[unsafe] (-y_error[n'-h+i] + y_error[n'+i])
            let mo = mo + elm
            let mo' = mo / (sigma * (f32.sqrt (r32 n')))
            let fst_break = if (fst_break == -1) && !(f32.isnan mo') &&
                               ((f32.abs mo') > 1.0001f32 * 1f32)
                            then i else fst_break
            in  (fst_break, mean + mo', mo)

      let fst_break' = if fst_break == -1 then -1
                       else let adj_break = adjustValInds (i32.i64 n) n' N' keys fst_break
                            in  ((adj_break-1) / 2) * 2 + 1  -- Cosmin's validation hack
      let fst_break' = if n' <=5 || N'-n' <= 5 then -2 else fst_break'

      in (fst_break', mean)

  ) images (iota m)

