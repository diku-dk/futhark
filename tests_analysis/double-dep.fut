def main [n] (xs: [n]i32) : [n]i32 =
  loop ys = copy xs for i < n do
    map (\p ->
      let t = i * 2 + p / 2 in
      ys[t] + ys[t / 2]
    ) (iota n)

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5382 : {
--     (arr) ys_5332 : {
--         (idx) +_lhs_5392 :
--             0 : dependencies = [ i_5331 0 seq | gtid_5383 1 par ]
--         (idx) +_rhs_5398 :
--             0 : dependencies = [ i_5331 0 seq | gtid_5383 1 par ]
--     }
-- }
