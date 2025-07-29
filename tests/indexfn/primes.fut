let sum [n] (xs: [n]i64) = if n > 0 then (scan (+) 0 xs)[n-1] else 0

def length [n] 't (_: [n]t) = n

type nat64 = {i64 | (>= 0)}

let mk_flag_array 't 'a [m]
        (shape: {[m]i64 | \x -> Range x (0,inf)})
        (zero: t)
        (xs: [m]t)
        : {([m]i64, []t) | \(_, flags) -> length flags == sum shape} =
  let shp_rot = map (\i -> if i==0 then 0i64 else shape[i-1]) (iota m)
  let shp_scn = scan (+) 0i64 shp_rot
  let shp_ind =
        map2 (\ shp ind ->
                if shp <= 0i64 then -1i64 else ind
             ) shape shp_scn
  let aoa_len = if m > 0 then shp_scn[m-1] + shape[m-1] else 0
  let zeros = replicate aoa_len zero
  let res = scatter zeros shp_ind xs
  in  (shp_scn, res)

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
      (shape: {[m]i64 | \x -> Range x (0,inf)})
      : {([]i64, [m]nat64) | \(ids, _) ->
           length ids == sum shape
        } =
  let flags1 = map (\i -> i + 1) (iota m)
  let zero = 0i64
  let (B, flags) = mk_flag_array shape zero flags1
  let flags_sgmind = map (\f -> if f == 0 then 0 else f-1) flags
  let flags_bool = map (\f -> f > 0) flags
  let II = sgm_sum flags_bool flags_sgmind
  in (II, B)

def iota_like 't [n] (_: [n]t): [n]i64 =
  iota n

def replicate_like 't 't2 [n] (_: [n]t) (x: t2): [n]t2 =
  replicate n x


def divider [nops]
    (n: {i64 | (>= 1)})
    (primes: {[nops]i64 | \ys -> Range ys (2,inf)})
    : {[nops]i64 | \ys -> Assume (Range ys (0,inf))} =
  map (\p -> n / p - 1) primes

def irregParKerII1Expl [nops]
      (n: {i64 | (>= 1)})
      (primes: {[nops]i64 | \ys -> Range ys (2,inf)})
      : {[]{i64 | (>= 1)} | \_ -> true} =
  let shp = divider n primes
  let (II1, B) = segment_ids shp
  let not_primes = map2 (\sgm_ind i ->
      let prime  = primes[sgm_ind]
      let offset = B[sgm_ind]
      let j = i - offset + 2i64
      in  j * prime
    ) II1 (iota_like II1)
  in  not_primes

def filterTrueInds [n] (bs: [n]bool) : {[]i64 | \ys -> Range ys (2,inf)} =
  let cs = map (\b -> if b then 1 else 0) bs
  let indT = scan (+) 0 cs
  let len  = if n > 0 then indT[n-1] else 0
  -- Using "f2" for indexing (proving it safe).
  let inds = map (\i ->
                    if !bs[i] then -1i64 else indT[i] - 1
                  ) (iota n)
  let vals = map (\i -> i+2) (iota n)
  in  scatter (replicate len 0i64) inds vals

def oneIter [len_sq_primes]
      (n64: {i64 | (>= 1)})
      (len: {i64 | (>= 1)})
      (sq_primes : {[len_sq_primes]i64 | \ys -> Range ys (2,inf)})
      : {([]i64, i64) | \(xs,_) -> Range xs (2,inf)} =
  -- this is "len = min n (len*len)" but without running out of i64 bounds
  -- let len = if n64 / len < len then n64 else len*len
  let not_primes = irregParKerII1Expl len sq_primes

  let false_array = map (\_ -> false) not_primes
  let mostly_true = map (\i -> i > 1) (iota (len + 1))
  let prime_flags = scatter mostly_true not_primes false_array
  -- discard first two elements which are always false
  let prime_flags = map (\i -> prime_flags[i+2]) (iota (len+1-2))
  let xs = filterTrueInds prime_flags
  in  (xs, len)
  
def primesFlat (n : {i64 | (>= 1)}) (sq_primes: {[16]i64 | \ys -> Range ys (2,inf)}) : {[]i64 | \_ -> true} =
  let len = length sq_primes
  let n64 = n
  -- This loop does not typecheck (a complication from the mix of size-types and our annotations
  -- being attached to types).
  -- Expected: ({[loop_d₁₂]i64| \(ys: [16]i64) ->   Range ys (1, inf)}, i64)
  -- Actual:   ({[16]i64| \(ys: [16]i64) ->   Range ys (1, inf)}, i64)
  -- Sizes "d₁₅" and "16" do not match.
  --
  -- let (res, _) = loop (sq_primes, len) while len < n64 do
  --       oneIter n64 len sq_primes
  --
  let (res, _) = oneIter n64 len sq_primes
  in res

-- We don't analyze entries.
-- entry primes n =
--   primesFlat n [2i64, 3i64, 5i64, 7i64, 11i64, 13i64]
