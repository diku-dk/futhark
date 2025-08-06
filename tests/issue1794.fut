def sgmscan 't [n] (op: t -> t -> t) (ne: t) (flg: [n]i64) (arr: [n]t) : [n]t =
  let flgs_vals =
    scan (\(f1, x1) (f2, x2) ->
            let f = f1 | f2
            in if f2 != 0
               then (f, x2)
               else (f, op x1 x2))
         (0, ne)
         (zip flg arr)
  let (_, vals) = unzip flgs_vals
  in vals

def mkFlagArray 't [m]
                (aoa_shp: [m]i64)
                (zero: t)
                (aoa_val: [m]t) : []t =
  let shp_rot = map (\i -> if i == 0 then 0 else aoa_shp[i - 1]) (iota m)
  let shp_scn = scan (+) 0 shp_rot
  let aoa_len = shp_scn[m - 1] + aoa_shp[m - 1]
  let shp_ind =
    map2 (\shp ind -> if shp == 0 then -1 else ind)
         aoa_shp
         shp_scn
  in scatter (replicate aoa_len zero) shp_ind aoa_val

def partition2 [n] 't (conds: [n]bool) (dummy: t) (arr: [n]t) : (i64, [n]t) =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let fflgs = map (\b -> 1 - b) tflgs
  let indsT = scan (+) 0 tflgs
  let tmp = scan (+) 0 fflgs
  let lst = if n > 0 then indsT[n - 1] else -1
  let indsF = map (+ lst) tmp
  let inds = map3 (\c indT indF -> if c then indT - 1 else indF - 1) conds indsT indsF
  let fltarr = scatter (replicate n dummy) inds arr
  in (lst, fltarr)

def main [m] (bs: [m]bool) (S1_xss: [m]i64) =
  let (spl, iinds) = partition2 bs 0 (iota m)
  let F = mkFlagArray S1_xss 0 (map (+ 1) iinds)
  let II1_xss = sgmscan (+) 0 F F |> map (\x -> x - 1)
  let mask_xss = map (\sgmind -> bs[sgmind]) II1_xss
  in (spl, iinds, mask_xss)
