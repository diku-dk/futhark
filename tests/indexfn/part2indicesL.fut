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
      (xs: [n]i64): {[n]i64 | \_ -> true} =
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

let part2indicesL 't [m][n]
      (shape: [m]nat64)
      (csL: {[n]bool | \_ -> n == sum shape})
      : {([n]i64, [m]i64, [n]i64, [m]i64) | \(inds, seg_ends, seg_ids, num_trues) ->
          -- Assuming csL is a flat representation of a segmented array of booleans
          -- with segment sizes denoted by shape, part2indicesL returns:
          --   (1) A flat array whose values are a permutation of the indices:
          --       (0, 1, ..., n - 1).
          --   (2) The indices in each row k are a permutation of
          --         sum shape[0:k-1], ..., (sum shape[0:k]) - 1.
          --   (3) The indices in each row k are partitioned by csL.
          -- 
          -- Proof.
          --   (1) A bijection from X to X is a permutation of X.
          let step1 = bijectiveRCD (0, n-1) (0, n-1) inds
          let step2 =
            map2 (\i k ->
              let seg_start = seg_ends[k] - shape[k]
              in seg_start <= i && i < seg_start + shape[k]
            ) inds seg_ids
          --   (3) Use num_trues to show partition point for each segment.
          let seg_starts = map (\k -> seg_ends[k] - shape[k]) seg_ids
          let seg_parts = map (\k -> num_trues[k]) seg_ids
          let seg_inds = map2 (\i seg_start -> i - seg_start) inds seg_starts
          let step3 = map3 (\c j p ->
              if c then j < p else j >= p
            ) csL seg_inds seg_parts
          in step1 && and step2 && and step3
        } =
  let (seg_ids, flags) = segment_ids shape

  -- v Size hints for the existing type-system.
  let seg_ids = sized n seg_ids
  let flags = sized n flags
  -- ^ Size hints for the existing type-system.

  let tflgs = map (\c -> if c then 1i64 else 0i64) csL
  let fflgs = map (\b -> 1 - b) tflgs

  let indsT = sgm_sum flags tflgs
  let tmp   = sgm_sum flags fflgs
  let ends  = scan (+) 0i64 shape

  let lst   = map2 (\s b -> if s==0i64 then -1i64 else #[unsafe] indsT[b-1]
                   ) shape ends

  let indsF = map2 (\t sgmind-> t + #[unsafe] lst[sgmind]) tmp seg_ids

  let offs = map (\segi -> if segi > 0 then ends[segi-1] else 0i64) seg_ids
  let inds = map4(\c indT indF offset ->
                      if c then offset + indT - 1
                           else offset + indF - 1
                  ) csL indsT indsF offs
  in (inds, ends, seg_ids, lst)
