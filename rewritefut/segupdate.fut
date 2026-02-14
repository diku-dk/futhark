-- Flat-Parallel Segmented Update
-- ==
-- compiled input { [1i64,2i64,3i64,1i64,2i64,1i64,2i64,3i64,4i64] [3i64,2i64,4i64] [0i64,0i64,0i64,0i64,0i64] [2i64,1i64,2i64] [0i64, 1i64, 0i64] [1i64, 1i64, 2i64] } output { [0i64,0i64,3i64,1i64,0i64,0i64,2i64,0i64,4i64] }

let sgmSumI64 [n] (flg : [n]i64) (arr : [n]i64) : [n]i64 =
  let flgs_vals = 
    scan ( \ (f1, x1) (f2,x2) -> 
            let f = f1 | f2 in
            if f2 != 0 then (f, x2)
            else (f, x1 + x2) )
         (0, 0i64) (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

let mkFlagArray [m] (aoa_shp: [m]i64) (zero: i64)
                       (aoa_val: [m]i64) : []i64 =
    let shp_rot = map(\i -> if i==0i64 then 0i64 else aoa_shp[i-1]) (iota m)
    let shp_scn = scan (+) 0i64 shp_rot
    let aoa_len = shp_scn[m-1]+aoa_shp[m-1]
    let shp_ind = map2 (\shp ind -> if shp==0 then -1i64 else ind) aoa_shp shp_scn
    in scatter (replicate aoa_len zero) shp_ind aoa_val

let segUpdate [n][m][t] (xss_val : *[n]i64) (shp_xss : [t]i64)  (vss_val : [m]i64) 
                    (shp_vss : [t]i64) (bs : [t]i64) (ss : [t]i64): [n]i64 =
    let fvss = (mkFlagArray shp_vss 0 (1...t :> [t]i64)) :> [m]i64                                     
    let II1 = sgmSumI64 fvss fvss |> map (\x -> x - 1)        
    let shp_xss_rot = map(\i -> if i==0i64 then 0i64 else shp_xss[i-1]) (iota t)
    let bxss = scan (+) 0 shp_xss_rot                                                                   
    let II2 = sgmSumI64 fvss (replicate m 1) |> map (\x -> x - 1)                                       
    let iss = map (\i -> bxss[II1[i]] + bs[II1[i]] + (II2[i] * ss[II1[i]])) (iota m)                     
    in scatter xss_val iss vss_val


let main [n][m][t] (xss_val : *[n]i64) (shp_xss : [t]i64) (vss_val : [m]i64) 
                    (shp_vss : [t]i64) (bs : [t]i64) (ss : [t]i64): [n]i64 =
    segUpdate xss_val shp_xss vss_val shp_vss bs ss