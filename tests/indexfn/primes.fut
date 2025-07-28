let sum [n] (xs: [n]i64) = if n > 0 then (scan (+) 0 xs)[n-1] else 0

def length [n] 't (_: [n]t) = n

type nat64 = {i64 | (>= 0)}

let mk_flag_array 't 'a [m]
        -- (shape: {[m]i64 | \x -> Range x (0,inf)})
        (shape: [m]nat64)
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

def mkFlagArray 't [m] 
            (aoa_shp: [m]i64) (zero: t)     -- aoa_shp=[0,3,1,0,4,2,0]
            (aoa_val: [m]t)  
          : ([m]i64, []t) =                 -- aoa_val=[1,1,1,1,1,1,1]
  let shp_rot = map (\i->if i==0 then 0     -- shp_rot=[0,0,3,1,0,4,2]
                         else aoa_shp[i-1]
                    ) (iota m)
  let shp_scn = scan (+) 0 shp_rot          -- shp_scn=[0,0,3,4,4,8,10]
  let aoa_len = if m == 0 then 0i64         --aoa_len = 10
                else shp_scn[m-1]+aoa_shp[m-1]
  let shp_ind = map2 (\shp ind ->           -- shp_ind= 
                       if shp==0 then -1i64 --  [-1,0,3,-1,4,8,-1]
                       else ind             -- scatter
                     ) aoa_shp shp_scn      --   [0,0,0,0,0,0,0,0,0,0]
  let r = scatter (replicate aoa_len zero)  --   [-1,0,3,-1,4,8,-1]
             shp_ind aoa_val                --   [1,1,1,1,1,1,1]
  in (shp_scn, r)                           -- r = [1,0,0,1,1,0,0,0,1,0]

def iota_like 't [n] (_: [n]t): [n]i64 =
  iota n

def replicate_like 't 't2 [n] (_: [n]t) (x: t2): [n]t2 =
  replicate n x


-- def infty: i64 = ???

-- type prime = {i64 | \x -> 1 <= x && x <= inf}

-- Writing / strangely to make it an uninterpreted function in our pass.
-- def div_nat64 (x: nat64) (y: nat64): {i64 | \x -> x >= 0} = Assume (\x -> x >= 0) ((i64./) x y)

def divider [nops] (n: {i64 | (>= 1)}) (primes: {[nops]i64 | \ys -> Range ys (1,inf)}): {[nops]i64 | \ys -> Assume (Range ys (0,inf))} =
    map (\p -> n / p - 1) primes

def irregParKerII1Expl (nops: nat64) (n: {i64 | (>= 1)}) (primes: {[nops]i64 | \ys -> Range ys (1,inf)}) : {[]{i64 | (>= 1)} | \_ -> true} =
      -- let shp = map (\p -> n / p - 1) primes
      let shp = divider n primes
      let is = iota nops
      let zero = 0
      let (B, flg16) = mk_flag_array shp zero is
      let flg = map (\i -> i != 0) flg16
      let II1 = sgm_sum flg flg16
      --
      let f (sgm_ind : i64) (i: i64) =
        let prime  = primes[sgm_ind]
        let offset = B[sgm_ind]
        let j = i - offset + 2i64
        in  j * prime
      --
      let not_primes = map2 f II1 (iota_like II1)
      in  not_primes

def ker = irregParKerII1Expl

def filterTrueInds [n] (bs: [n]bool) : {[]i64 | \_ -> true} =
  let cs = map (\b -> if b then 1 else 0) bs
  let indT = scan (+) 0 cs
  let len  = last indT

  let f1 (i: i64) =
    let cur_ind = indT[i] in
    if i == 0 
    then (if cur_ind == 1 then 0i64 else -1i64)
    else let prev_ind = indT[i-1] in
         if cur_ind == prev_ind
         then -1i64 
         else cur_ind-1
  -- let f2 (i: i64) = #[unsafe]
  --   if !bs[i] then -1i64
  --   else indT[i] - 1

  let inds = map f1 (iota n)  -- may also use f2
  in  scatter (replicate len 0i64) inds (iota n)

def oneIter [len_sq_primes] (n64 : i64) (len: i64)
                            (sq_primes : [len_sq_primes]i64) : {([]i64, i64) | \_ -> true} =
  -- this is "len = min n (len*len)" but without running out of i64 bounds
  let len = if n64 / len < len then n64 else len*len
  let not_primes = ker len_sq_primes len sq_primes

  let false_array = map (\_ -> false) not_primes
  let mostly_true = map (> 1) (iota (len + 1))
  let prime_flags = scatter mostly_true not_primes false_array
  let xs = filterTrueInds prime_flags
  in  (xs, len)
  
def primesFlat (n : i64) : {[]i64 | \_ -> true} =
  let sq_primes   = [2i64, 3i64, 5i64, 7i64, 11i64, 13i64]
  let len = 16i64
  let n64 = n
  let (res, _) = oneIter n64 len sq_primes
  -- let (res, _) = (loop (sq_primes, len) while len < n64 do
  --       oneIter n64 len sq_primes
  --    )
  in res
