--
-- ==
-- compiled input { [1, 3, 1, 1, 1, 53, 2, 2, 1, 7, 8, 1,
-- 2, 1, 1, 41, 2, 2, 4, 1, 1, 37, 1, 1, 1, 2,
-- 2, 40, 1, 1, 1, 63, 1, 9, 2, 1, 2, 1, 3, 35 ]
-- }
-- output { [5i32, 15i32, 21i32, 27i32, 31i32, 39i32, 10i32, 33i32, 0i32, 1i32, 2i32, 3i32,
--           4i32, 6i32, 7i32, 8i32, 9i32, 11i32, 12i32, 13i32, 14i32, 16i32, 17i32, 18i32,
--           19i32, 20i32, 22i32, 23i32, 24i32, 25i32, 26i32, 28i32, 29i32, 30i32, 32i32,
--           34i32, 35i32, 36i32, 37i32, 38i32] }

def sgmPrefSum [n] (flags: [n]i32) (data: [n]i32) : [n]i32 =
  (unzip (scan (\(x_flag, x) (y_flag, y) ->
                  let flag = x_flag | y_flag
                  in if y_flag != 0
                     then (flag, y)
                     else (flag, x + y))
               (0, 0)
               (zip flags data))).1

def bin_packing_ffh [q] (w: i32) (all_perm: *[q]i32) (all_data0: [q]i32) =
  let all_data = scatter (replicate q 0) (map i64.i32 all_perm) all_data0
  let len = q
  let cur_shape = replicate 0 0
  let goOn = true
  let count = 0
  let (_, all_perm, _, _, _, _) =
    loop ((len, all_perm, all_data, cur_shape, goOn, count)) while goOn && count < 100 do
      let data = take (len) all_data
      let perm = take (len) all_perm
      -- 1. initial attempt by first fit heuristic
      let scan_data = scan (+) 0 data
      let ini_sgms = map (/ w) scan_data
      let num_sgms = (last ini_sgms) + 1
      -- OK
      let flags =
        map (\i ->
               if i == 0
               then 1
               else if ini_sgms[i - 1] == ini_sgms[i]
               then 0
               else 1)
            (map i32.i64 (iota len))
      let ones = replicate len 1
      let tmp = sgmPrefSum flags ones
      let (inds1, inds2, vals) =
        unzip3 (map (\i ->
                       if (i == i32.i64 len - 1) || (flags[i + 1] == 1)
                       then -- end of segment
                            (i + 1 - tmp[i], ini_sgms[i], tmp[i])
                       else (-1, -1, 0))
                    (map i32.i64 (iota len)))
      let flags = scatter (replicate len 0) (map i64.i32 inds1) vals
      let shapes = scatter (replicate (i64.i32 num_sgms) 0) (map i64.i32 inds2) vals
      -- 2. try validate: whatever does not fit move it as a first segment
      let scan_data = sgmPrefSum flags data
      let ini_sgms = scan (+) 0 (map (\x -> if x > 0 then 1 else 0) flags)
      -- map (/w) scan_data
      let moves =
        map (\i ->
               let sgm_len = flags[i]
               in if sgm_len > 0
                  then if scan_data[i + sgm_len - 1] > w
                       then 1
                       else -- this start of segment should be moved
                            0
                  else 0)
            (map i32.i64 (iota len))
      let num_moves = reduce (+) 0 moves
      -- if true
      -- then (num_moves, flags, all_data, concat shapes cur_shape, false)
      in if num_moves == 0
         then (num_moves, all_perm, all_data, concat shapes cur_shape, false, count)
         else -- reorder perm, data, and shape arrays
              let scan_moves = scan (+) 0 moves
              let (inds_s, lens, inds_v) =
                unzip3 (map (\i ->
                               let offset = scan_moves[i]
                               let (ind_s, ll) =
                                 if i > 0 && flags[i] == 0 && moves[i - 1] > 0
                                 then -- new start of segment
                                      (ini_sgms[i - 1] - 1, flags[i - 1] - 1)
                                 else (-1, 0)
                               let ind_v = if moves[i] == 0 then (num_moves - offset + i) else offset - 1
                               -- ???
                               in (ind_s, ll, ind_v))
                            (iota len))
              let shapes' = scatter shapes inds_s lens
              let cur_shape = concat shapes' cur_shape
              let all_perm = scatter (copy all_perm) inds_v perm
              let all_data = scatter (copy all_data) inds_v data
              -- in  (num_moves, all_perm, inds_v, cur_shape, false)
              in (num_moves, all_perm, all_data, cur_shape, true, count + 1)
  in all_perm

def main [arr_len] (arr: [arr_len]i32) =
  bin_packing_ffh 10 (map i32.i64 (iota arr_len)) arr
