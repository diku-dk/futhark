import "intrinsics-accs"

type etp = u32

def mapIntra f as = #[incremental_flattening(only_intra)] map f as
def map3Intra f as bs cs = #[incremental_flattening(only_intra)] map3 f as bs cs

def getBits (bit_beg: u32) (num_bits: u32) (x: u32) : u32 =
  let mask = (1 << num_bits) - 1
  in (x >> bit_beg) & mask

def isBitUnset1 (bit_num: u32) (x: u32) : u32 =
  let shft = x >> bit_num
  in 1 - (shft & 1)

def isBitUnset (bit_num: u32) (x: u32) : bool =
  let shft = x >> bit_num
  in 0 == (shft & 1)

def ker1Blk [B][Q]
            (bit_beg: u32)
            (lgH: u32)
            (xs: [B*Q]u32) : [B]u16 =
  let histo = replicate B 0u32
  let facc (histo: *acc ([B]u32)) tid : acc ([B]u32) =
      loop histo for q < Q do
        let ind = q*B + tid
        let bin = getBits bit_beg lgH (xs[ind])
        in  write histo (i64.u32 bin) 1u32
  let histo =
    reduce_by_index_stream histo (+) 0u32 facc (iota B)
  in  map u16.u32 histo

def ker2Blk [B][Q]
            (bit_beg: u32)
            (lgH: u32)
            (histo_loc: [B]u16)
            (histo_glb: [B]i64)
            (xs: [B][Q]u32) : (*[B][Q]u32, [B][Q]i64) =
  let elms = #[glb2reg_only(1)] manifest xs
  let elms =
    loop elms
    for i < i32.u32 lgH do
      let ff tid =
        loop s = 0u16 for q < Q do
          let zo = isBitUnset (bit_beg + u32.i32 i) (elms[tid,q])
          in  s + u16.bool zo
      --
      let tmp_buff = opaque <| map ff (iota B)
      let buff  = scan (+) 0u16 tmp_buff
      let split = buff[B-1]
      --
      let gg (shm: *acc ([B*Q]u32)) tid : acc ([B*Q]u32) =
        let s = if tid == 0 then 0u16 else buff[tid-1] in
        (loop (shm,s)  for q < Q do
           let elm = elms[tid, q]
           let zo  = isBitUnset (bit_beg + u32.i32 i) elm
           let s   = s + u16.bool zo
           let pos = if zo then s - 1 else (split + (u16.i64 tid * u16.i64 Q) + u16.i64 q) - s
           in  ( write shm (i64.u16 pos) elm, s ) ).0
      let shm = replicate (B*Q) 0u32  --  !!! BUG: a #[scratch] annotation would result in eranneous result !!! 
      let shm = opaque <| scatter_stream shm gg (iota B)
      --
      let freg tid =
        let f1 q = shm[q*B + tid]
        let f2 q = shm[tid*Q + q]
        in  if i == (i32.u32 lgH) - 1
            then #[sequential] map f1 (iota Q)
            else #[sequential] map f2 (iota Q)
      let elms = #[toregmem(1)] map freg (iota B)
      in  elms
  -- end repeated-partitioning LOOP
  -- compute the partial destination index from the histograms
  let hist_loc = manifest histo_loc
  let hist_loc_scan = scan (+) 0u16 hist_loc
  let hh tid = histo_glb[tid] - i64.u16 hist_loc_scan[tid]
  let histo  = map hh (iota B)
  -- compute the final index
  let finalInd tid =
    let finner q = 
      let elm = elms[tid, q]
      let bin = getBits bit_beg lgH elm
      let glb_offset = histo[i32.u32 bin]
      in  glb_offset + (q*B + tid)
    in  #[sequential] map finner (iota Q)
    -- data_keys_out[glb_pos] = elm;
  let fin_inds = #[toregmem(1)] map finalInd (iota B)
  in  (elms, fin_inds)

def radixIter [m][B][QQ]
              (bit_beg: u32)
              (lgH: u32)
              (dst:*[m * (B*QQ)]u32)
              (xs:  [m * (B*QQ)]u32) : *[m * (B*QQ)]u32 =
  #[unsafe]
  let xs' = opaque <| unflatten xs
  let hist16 = mapIntra (ker1Blk bit_beg lgH) xs'
  let hist64 =
    transpose hist16
    |> manifest
    |> flatten
    |> map i64.u16
    |> scan (+) 0i64
  let hist64T =
    unflatten hist64
    |> transpose |> manifest
  let xs' = opaque <| map unflatten <| unflatten xs
  let scat_dst = dst  --  replicate (m * (B*QQ)) 0u32 --  #[scratch] 
  let (xs', inds') =
    unzip
    <| map3Intra (ker2Blk bit_beg lgH) hist16 hist64T xs'
  in scatter scat_dst (flatten (map flatten inds')) (flatten (map flatten xs'))


-- Simple test for fusing scatter-flatten with the preceding
-- map nest that produces its indices and values
---- ==
-- entry: firstIter 
-- compiled random input { 16384i64 [92274688]u32 [92274688]u32 }

let QQ : i64 = 22i64

--entry firstIter (m: i64)
--                (xs:   [m * (256*QQ)]u32)
--                (tmp1:*[m * (256*QQ)]u32)
--              : *[m * (256*QQ)]u32 =
--  radixIter 0u32 8u32 tmp1 xs

-- ==
-- entry: radixSortU32 
-- compiled random input { 16384i64 [92274688]u32 }

-- output { true } 
--

-- compiled random input { 2i64 [92274688]u32 } 

entry radixSortU32 (m: i64)
                   (xs  : *[m * (256*QQ)]u32) =
                 -- : *[m * (256*QQ)]u32 =
  #[unsafe]
  -- let lgH = 8u32
  let tmp = replicate (m * (256*QQ)) 0u32
  let (xs_res, _) =
    loop (xs, tmp) for i < 4i32 do
      let xs' = radixIter (8 * u32.i32 i) 8u32 tmp xs
      in  (xs', xs)
  let success = 
        reduce (&&) true <|
        map (\ i -> xs_res[i] <= xs_res[i+1]) <|
        iota (m * 256 * QQ - 1) 
  in  xs_res -- success
