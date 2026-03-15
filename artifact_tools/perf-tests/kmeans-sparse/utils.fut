let scanExc 't [n] (op: t->t->t) (ne: t) (arr : [n]t) : [n]t =
    scan op ne <| map (\i -> if i>0 then arr[i-1] else ne) (iota n)


---------------------
--- SgmSumInt     ---
---------------------
-- 2. sgmSumInt on integers, i.e., sgmIncScan (+) 0

let sgmSumInt [n] (flg : [n]i32) (arr : [n]i32) : [n]i32 =
  let flgs_vals = 
    scanExc ( \ (f1, x1) (f2,x2) -> 
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2) )
         (0,0) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals
  
let sgmSumIntIncl [n] (flg : [n]i32) (arr : [n]i32) : [n]i32 =
  let flgs_vals = 
    scan ( \ (f1, x1) (f2,x2) -> 
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2) )
         (0,0) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

let sgmSumInt64 [n] (flg : [n]i64) (arr : [n]i64) : [n]i64 =
  let flgs_vals = 
    scanExc ( \ (f1, x1) (f2,x2) -> 
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2) )
         (0,0) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

let sgmSumIntIncl64 [n] (flg : [n]i64) (arr : [n]i64) : [n]i64 =
  let flgs_vals = 
    scan ( \ (f1, x1) (f2,x2) -> 
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 + x2) )
         (0,0) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

---------------------
--- MkFlags Array ---
---------------------
-- read: 4m*sizeof(i64) write: 4m*sizeof(i64)
let mkFlagArray 't [m] 
            (aoa_shp: [m]i64) (zero: t)       --aoa_shp=[0,3,1,0,4,2,0]
            (aoa_val: [m]t  ) : []t =         --aoa_val=[1,1,1,1,1,1,1]
  let shp_rot = map (\i->if i==0 then 0       --shp_rot=[0,0,3,1,0,4,2]
                         else aoa_shp[i-1]
                    ) (iota m)
  let shp_scn = scan (+) 0 shp_rot            --shp_scn=[0,0,3,4,4,8,10]
  let aoa_len = shp_scn[m-1]+aoa_shp[m-1]     --aoa_len= 10
  let shp_ind = map2 (\shp ind ->             --shp_ind= 
                       if shp==0 then -1      --  [-1,0,3,-1,4,8,-1]
                       else ind               --scatter
                     ) aoa_shp shp_scn        --   [0,0,0,0,0,0,0,0,0,0]
  in scatter(replicate aoa_len zero)--   [-1,0,3,-1,4,8,-1]
            shp_ind aoa_val     --   [1,1,1,1,1,1,1]
                                              -- res = [1,0,0,1,1,0,0,0,1,0] 


let segscan [n] 't (op: t -> t -> t) (ne: t)
                          (flags: [n]bool) (as: [n]t): [n]t =
  (unzip (scan (\(x_flag,x) (y_flag,y) ->
                (x_flag || y_flag,
                 if y_flag then y else x `op` y))
          (false, ne)
          (zip flags as))).1

-- | Segmented reduction. Given a binary associative operator ``op``
-- with neutral element ``ne``, computes the reduction of the segments
-- of ``as`` specified by the ``flags`` array, where `true` starts a
-- segment and `false` continues a segment.  One value is returned per
-- segment.
let segreduce [n] 't (op: t -> t -> t) (ne: t)
                            (flags: [n]bool) (as: [n]t) =
  -- Compute segmented scan.  Then we just have to fish out the end of
  -- each segment.
  let as' = segscan op ne flags as
  -- Find the segment ends.
  let segment_ends = rotate 1 flags
  -- Find the offset for each segment end.
  let segment_end_offsets = segment_ends |> map i64.bool |> scan (+) 0
  let num_segments = if n > 0 then last segment_end_offsets else 0
  -- Make room for the final result.  The specific value we write here
  -- does not matter; they will all be overwritten by the segment
  -- ends.
  let scratch = replicate num_segments ne
  -- Compute where to write each element of as'.  Only segment ends
  -- are written.
  let index i f = if f then i-1 else -1
  in scatter scratch (map2 index segment_end_offsets segment_ends) as'
