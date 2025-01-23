let sum [n] (xs: [n]i64) = if n > 0 then (scan (+) 0 xs)[n-1] else 0

def length [n] 't (_: [n]t) = n

type nat64 = {i64 | (>= 0)}


let mk_flag_array 't 'a [m]
        (shape: [m]nat64)
        (zero: t)
        (xs: [m]t)
        : {[]t | \flags -> length flags == sum shape} =
  let shp_rot = map (\i -> if i==0 then 0i64 else shape[i-1]) (iota m)
  let shp_scn = scan (+) 0i64 shp_rot
  let shp_ind =
        map2 (\ shp ind ->
                if shp <= 0i64 then -1i64 else ind
             ) shape shp_scn
  let aoa_len = if m > 0 then shp_scn[m-1] + shape[m-1] else 0
  let zeros = replicate aoa_len zero
  let res = scatter zeros shp_ind xs
  in  res

def sgm_sum [n] 't
      (flags: [n]bool)
      (xs: [n]i64): [n]i64 =
  let zipped = zip flags xs
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x + y))
         (false, 0i64)
         zipped
  let (_flags, ys) = unzip flags_ys
  in ys


-- Expands a shape array to flat arrays of segment ids and flags.
let segment_ids [m]
      (shape: [m]nat64)
      : {([]i64, []bool) | \(ids, flags) ->
           length ids == sum shape
             && length flags == sum shape
        } =
  let flags1 = map (\i -> i + 1) (iota m)
  let flags = mk_flag_array shape 0i64 flags1
  let flags_sgmind = map (\f -> if f == 0 then 0 else f-1) flags
  let flags_bool = map (\f -> f > 0) flags
  in (sgm_sum flags_bool flags_sgmind, flags_bool)

-- def something (xs
--   and (map (\i -> seg_starts[seg_ids[i]] <= i) inds)

-- def permutation xs { injective , within 0 and n } =

let part2indicesL 't [m][n]
                (shape: [m]nat64)
                -- `shape` is the shape of csL
                (csL: {[n]bool | \_ -> n == sum shape})
                : {([n]i64, [m]i64, [n]i64) | \(inds, seg_starts, seg_ids) ->
                    injective inds
                      && and (map (\i -> 0 <= i && i < n) inds)
                      -- and each segment is within shape bounds
                    --   && and (map (\id -> 0 <= id && id < m) seg_ids)
                    --   && and (map (\i -> seg_starts[seg_ids[i]]) inds)
                    --   map
                    --     1.
                    --       e1[e2(i)]

                    --       fn1 <- get/create indexfn e1
                    --       bind fn1 to fresh name x
                    --       fn2 <- e2
                    --       fresh name y

                    --       . | true => x[e2(i)]

                    --    2. . | true => z[x[e2(i)]]

                    -- 3.  map body: | true => z[x[e2(i)]]
                    --    => \i in dom | true => z[x[e2(i)]]


                    -- 4. Show e2(i) in bounds of x |-> fn1.
                    --    => \i in dom | true => z[x[e2(i)]]
                    --    Do substitution of x
                    --    => \i in dom | true => z[k]
                    --    Show k in bounds of z | -> fn3
                    --    => \i in dom | true => sum shape[0:k]

                      -- ==> each seg i a perm
                      -- && segments (\seg -> injective seg) inds
                      -- && injective inds
                      -- && segments (\seg -> injective seg) inds
                      -- && segments (\seg_start seg_end -> injective seg) inds
                      -- TODO also show that each ind in segment
                      --      is Sum shp[k] <= i < Sum shp[k]
                      --      Then we have permutation at segment level.
                  } =
  let (seg_ids, flags) = segment_ids shape

  -- v SIZE HINTS FOR THE EXISTING TYPE-SYSTEM.
  let seg_ids = sized n seg_ids
  let flags = sized n flags
  -- ^ SIZE HINTS FOR THE EXISTING TYPE-SYSTEM.

  let tflgs = map (\c -> if c then 1i64 else 0i64) csL
  let fflgs = map (\b -> 1 - b) tflgs

  let indsT = sgm_sum flags tflgs
  let tmp   = sgm_sum flags fflgs
  let begs  = scan (+) 0i64 shape

  let lst   = map2 (\s b -> if s==0i64 then -1i64 else #[unsafe] indsT[b-1]
                   ) shape begs

  let indsF = map2 (\t sgmind-> t + #[unsafe] lst[sgmind]) tmp seg_ids

  let offs = map (\segi -> if segi > 0 then begs[segi-1] else 0i64) seg_ids
  let inds = map4(\c indT indF offset ->
                      if c then offset + indT - 1
                           else offset + indF - 1
                  ) csL indsT indsF offs
  in (inds, begs, seg_ids)
