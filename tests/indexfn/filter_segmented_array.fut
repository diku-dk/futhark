let sum [n] (xs: [n]i64) = if n > 0 then (scan (+) 0 xs)[n-1] else 0

def length [n] 't (_: [n]t) = n

type nat64 = {i64 | (>= 0)}


def mk_flag_array 't 'a [m]
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
def segment_ids [m]
      (shape: [m]nat64)
      -- : {([]i64, []bool) | \(ids, flags) ->
      --      length ids == sum shape
      --        && length flags == sum shape
      --        && and (map (\id -> 0 <= id && id < m) ids)
      --   } =
      : ({[]i64 | \ids ->
           length ids == sum shape
             && and (map (\id -> 0 <= id && id < m) ids)
        }, {[]bool | \flags -> length flags == sum shape}) =

  let flags1 = map (\i -> i + 1) (iota m)
  let flags = mk_flag_array shape 0i64 flags1
  let flags_sgmind = map (\f -> if f == 0 then 0 else f-1) flags
  let flags_bool = map (\f -> f > 0) flags
  in (sgm_sum flags_bool flags_sgmind, flags_bool)

def part2indices [n] (conds: [n]bool) : {(i64, [n]i64) | \_ -> true} =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs
  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] else 0
  let indsF = map (\t -> t +lst) tmp
  let inds  = map3 (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF
  in  (lst, inds)

def filter_segmented_array [m][n]
      (shape: [m]nat64)
      (pivots: [m]f32)
      (xs: {[n]f32 | \_ -> n == sum shape})
      : {[]f32 | \_ -> true} =
  -- xs is segmented by shape
  let (II, _) = segment_ids shape
  let conds = map (\i -> xs[i] < pivots[II[i]]) (iota n)
  -- XXX conds has a union iterator due to indexing into II,
  --     but has to have a non-union iterator to proceed here!
  --     In particular: conds will be summed and we cannot sum over a union conds,
  --     because the body contains k.
  let (_, perm) = part2indices conds
  in map (\i -> xs[i]) perm
