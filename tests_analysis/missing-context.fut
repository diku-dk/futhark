def main (x: i64) (y: i64) : [x]i64 =
  let b = y + 2 in
  let rs = tabulate x (\ii ->
  loop acc = 0 for i < x do
    let a = b + acc
    in acc + a + ii / i
  )
  in map2 (+) rs <| reverse rs

-- === Expected output of analysis:
-- (segmap) defunc_0_map_res_5603 : {
--     (arr) defunc_0_map_res_5575 : {
--         (idx) eta_p_5606 :
--             0 : dependencies = {gtid_5604 0 par}
--         (idx) eta_p_5607 :
--             0 : dependencies = {x_5411 0 seq, gtid_5604 0 par}
--     }
-- }
