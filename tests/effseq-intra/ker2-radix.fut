def mapIntra  f as = #[incremental_flattening(only_intra)] #[seq_factor(22)] map f as
def map3Intra f as bs cs = #[incremental_flattening(only_intra)] #[seq_factor(22)] map3 f as bs cs

-- def mapIntra  f as = map f as
-- def map3Intra f as bs cs = map3 f as bs cs


let partition2 't [n]  (dummy: t)
      (cond: t -> bool) (X: [n]t) : 
                      (i64, *[n]t) =
 let cs = map cond X
 let tfs= map (\ f->if f then 1i64
                         else 0i64) cs
 let isT= scan (+) 0 tfs
 let i  = isT[n-1]

 let ffs= map (\f->if f then 0 
                        else 1) cs
 let isF= map (+i) <| scan (+) 0 ffs
 let inds=map (\(c,iT,iF) -> 
                  if c then iT-1 
                       else iF-1
              ) (zip3 cs isT isF)
 let tmp = replicate n dummy
 in (i, scatter tmp inds X)

-- let main [n] (arr: [n]i32) : (i64,*[n]i32) =
--   partition2 0i32 (\(x:i32) -> (x & 1i32) == 0i32) arr

def getBits (bit_beg: u32) (num_bits: u32) (x: u32) : u32 =
  let mask = (1 << num_bits) - 1
  in  (x >> bit_beg) & mask

def isBitUnset1 (bit_num: u32) (x: u32) : u32 =
  let shft = x >> bit_num
  in  1 - (shft & 1)

def isBitUnset (bit_num: u32) (x: u32) : bool =
  let shft = x >> bit_num
  in  0 == (shft & 1)

def ker1Blk [n] (bit_beg: u32) (lgH: u32)
                (xs: [n]u32) : [i64.u32 (1u32 << lgH)]u16 =
  let histo_len = i64.u32 (1u32 << lgH)
  let inds = map (getBits bit_beg lgH) xs |> map i64.u32
  in  hist (+) 0u32 histo_len inds (replicate n 1u32)
      |> map u16.u32


def ker2Blk [n] (bit_beg: u32) (lgH: u32)
                (histo_loc: [i64.u32 (1u32 << lgH)]u16)
                (histo_glb: [i64.u32 (1u32 << lgH)]i64)
                (xs: [n]u32) : (*[n]u32, [n]i64) =
  let xs' =
    loop (xs) = (copy xs)
    for i < i32.u32 lgH do
      (partition2 0u32 (isBitUnset (bit_beg + u32.i32 i)) xs).1

  let histo_scn = tabulate (i64.u32 (1u32 << lgH)) (\j -> if j==0 then 0u16 else #[unsafe]histo_loc[j-1])
               |> scan (+) 0u16
  let histo = map3 (\ a b c -> a - i64.u16 (b + c)) histo_glb histo_loc histo_scn
  let f x i =
    let bin = getBits bit_beg lgH x
    in  i + (#[unsafe] histo[i32.u32 bin])
  let inds = map2 f xs' (iota n)
  in  (xs', inds)

entry radixIt (m: i64) (bq: i64) 
              (bit_beg: u32) -- (lgH: u32)
              (xs: *[m*bq]u32) : *[m*bq]u32 =
  let lgH = 8u32
  let hist16  = mapIntra (ker1Blk bit_beg lgH) (unflatten xs)
  let hist64  = transpose hist16
             |> flatten
             |> map i64.u16
             |> scan (+) 0i64
  let hist64T = unflatten hist64
             |> transpose
  let (xs', inds') = unzip <|
                map3Intra (ker2Blk bit_beg lgH) hist16 hist64T (unflatten xs)
  in  scatter xs (flatten inds') (flatten xs')
