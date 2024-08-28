-- Issue was memory expansion did not handle an allocation inside
-- nested SegOps in a Group SegOp.  And _this_ was because the size
-- was not hoisted all the way out, because there was an If
-- surrounding the inner SegThread SegOps.  And _this_ was because we
-- lost the information that the size was actually used as a size.
-- Tiling produced such a SegOp.  In most cases that version will then
-- be discarded, but the attributes ensure that only one version is
-- produced.

module loess_m = {
module T = f64

type t = T.t

let filterPadWithKeys [n] 't
           (p : (t -> bool))
           (dummy : t)
           (arr : [n]t) : ([n]t, [n]i64, i64) =
  let tfs = map (\a -> if p a then 1i64 else 0i64) arr
  let isT = scan (+) 0i64 tfs
  let i   = last isT
  let inds= map2 (\a iT -> if p a then iT - 1 else -1i64) arr isT
  let rs  = scatter (replicate n dummy) inds arr
  let ks  = scatter (replicate n (-1i64)) inds (iota n)
in (rs, ks, i)

--------------------------------------------------------------------------------
-- Main LOESS procedure - outer parallel version, with extra work             --
--------------------------------------------------------------------------------
let loess_outer [n] [n_m] (xx: [n]i64)
                          (yy: [n]t)
                          (q: i64)
                          (ww: [n]t)
                          (l_idx: [n_m]i64)
                          (lambda: [n_m]t)
                          (n_nn: i64) =
  let q_slice 'a (arr: [n]a) (l_idx_i: i64) (v: a) (add: a -> a -> a) (zero: a): [q]a =
    #[unsafe]
    tabulate q (\j -> if j >= n_nn then zero else add arr[l_idx_i + j] v)
  -- need the duplicate to prevent manifestation
  let q_slice' 'a (arr: [n]a) (l_idx_i: i64) (v: a) (add: a -> a -> a) (zero: a): [q]a =
    #[unsafe]
    tabulate q (\j -> if j >= n_nn then zero else add arr[l_idx_i + j] v)
  in
  -- [n_m]
  #[sequential_inner]
  map2 (\l_idx_i lambda_i ->
         -----------------------------------
         -- REDOMAP 1
         -----------------------------------
         #[unsafe]
         let xx_slice = q_slice xx l_idx_i 1 (+) 0
         let ww_slice = q_slice ww l_idx_i 0 (+) 0
         let (w, xw, x2w) =
           map2 (\xx_j ww_j ->
                   -- let x_j = (xx_j - m_fun i) |> T.i64
                   let x_j = xx_j |> T.i64
                   -- tricube
                   let r = T.abs x_j
                   let tmp1 = r / lambda_i
                   let tmp2 = 1.0 - tmp1 * tmp1 * tmp1
                   let tmp3 = tmp2 * tmp2 * tmp2
                   -- scale by user-defined weights
                   let w_j = tmp3 * ww_j
                   let xw_j = x_j * w_j
                   let x2w_j = x_j * xw_j
                   in (w_j, xw_j, x2w_j)
                ) xx_slice ww_slice |> unzip3
         -- then, compute fit and slope based on polynomial degree
         let a = T.sum w + T.epsilon
         let b = T.sum xw + T.epsilon
         let c = T.sum x2w + T.epsilon

         -- degree 1
         let det1 = 1 / (a * c - b * b)
         let a11 = c * det1
         let b11 = -b * det1

         -----------------------------------
         -- REDOMAP 2
         -----------------------------------
         let w' =
           map2 (\xx_j ww_j ->
                   let x_j = xx_j |> T.i64
                   -- tricube
                   let r = T.abs x_j
                   let tmp1 = r / lambda_i
                   let tmp2 = 1.0 - tmp1 * tmp1 * tmp1
                   let tmp3 = tmp2 * tmp2 * tmp2
                   -- scale by user-defined weights
                   in tmp3 * ww_j
                ) xx_slice ww_slice
         -- then, compute fit and slope based on polynomial degree
         let yy_slice = q_slice' yy l_idx_i 0 (+) 0
         in map3 (\w_j yy_j xw_j -> (w_j * a11 + xw_j * b11) * yy_j) w' yy_slice xw |> T.sum
       ) l_idx lambda

let loess_outer_l [m] [n] [n_m] (xx_l: [m][n]i64)
                                (yy_l: [m][n]t)
                                (q: i64)
                                (ww_l: [m][n]t)
                                (l_idx_l: [m][n_m]i64)
                                (lambda_l: [m][n_m]t)
                                (n_nn_l: [m]i64) =
  #[incremental_flattening(no_intra)]
  map5 (\xx yy ww l_idx (lambda, n_nn) ->
          loess_outer xx
                      yy
                      q
                      ww
                      l_idx
                      lambda
                      n_nn
       ) xx_l yy_l ww_l l_idx_l (zip lambda_l n_nn_l)

let l_indexes [N] (nn_idx: [N]i64)
                  (m_fun: i64 -> i64)
                  (n_m: i64)
                  (q: i64)
                  (n_nn: i64): [n_m]i64 =
  -- [n_m]
  tabulate n_m (\i ->
        let x = m_fun i
        -- use binary search to find the nearest idx
        let (init_idx, _) =
          -- O(log N)
          loop (low, high) = (0i64, N - 1) while low <= high do
            let mid = (low + high) / 2
            let mid_id = nn_idx[mid]
            let mid_idx = if mid_id < 0 then i64.highest else mid_id
            in
            if mid_idx >= x then (low, mid - 1) else (mid + 1, high)
        let (idx, _, _) =
          -- find the neighbor interval, starting at init_idx
          loop (l_idx, r_idx, span) = (init_idx, init_idx, 1) while span < q do
            -- O(q)
            let l_cand = i64.max (l_idx - 1) 0
            let r_cand = i64.min (r_idx + 1) (n_nn - 1)
            let l_dist = i64.abs (nn_idx[l_cand] - x)
            let r_dist = i64.abs (nn_idx[r_cand] - x)
            in
            if l_cand == l_idx
              then (l_idx, r_idx, q)         -- leftmost found, return
            else if l_dist < r_dist || r_cand == r_idx
              then (l_cand, r_idx, span + 1) -- expand to the left
            else (l_idx, r_cand, span + 1)   -- expand to the right
        let res_idx = i64.max (i64.min (n_nn - q) idx) 0
        in res_idx
     )

let find_lambda [n_m] (y_idx: []i64)
                        (l_idx: [n_m]i64)
                        (m_fun: i64 -> i64)
                        (q: i64)
                        (n_nn: i64) : [n_m]t=
  map2 (\l i ->
          let mv = m_fun i
          let q' = i64.min q n_nn
          let r = l + q' - 1
          let md_i = i64.max (i64.abs (y_idx[l] - mv))
                             (i64.abs (y_idx[r] - mv)) |> T.i64
          in
          md_i + T.max (((T.i64 q) - (T.i64 n_nn)) / 2) 0
       ) l_idx (iota n_m)

let loess_params [N] (q: i64)
                     (m_fun: i64 -> i64)
                     (n_m: i64)
                     (y_idx: [N]i64)
                     (n_nn: i64)
                     : ([n_m]i64, [n_m]t) =
  let y_idx_p1 = (y_idx |> map (+1))
  let q3 = i64.min q N
  -- [n_m]
  let l_idx = l_indexes y_idx_p1 (m_fun >-> (+1)) n_m q3 n_nn
  let lambda = find_lambda y_idx l_idx m_fun q n_nn
  in (l_idx, lambda)
}

entry main [m] [n] (Y: [m][n]f64) (q: i64) (jump: i64) =
  -- set up parameters for the low-pass filter smoothing
  let n_m = if jump == 1 then n else n / jump + 1
  let m_fun (x: i64): i64 = i64.min (x * jump) (n - 1)

  -- filter nans and pad non-nan indices
  let (nn_y_l, nn_idx_l, n_nn_l) =
    map (loess_m.filterPadWithKeys (\i -> !(f64.isnan i)) 0) Y |> unzip3

  -- calculate invariant arrays for the low-pass filter smoothing
  let (l_idx_l, lambda_l) =
    map2 (\nn_idx n_nn ->
            loess_m.loess_params q m_fun n_m nn_idx n_nn
         ) nn_idx_l n_nn_l |> unzip

  let weights_l = replicate (m * n) 1f64 |> unflatten
  in
  loess_m.loess_outer_l nn_idx_l
                        nn_y_l
                        q
                        weights_l
                        l_idx_l
                        lambda_l
                        n_nn_l
