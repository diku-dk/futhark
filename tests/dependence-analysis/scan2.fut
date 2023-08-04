def main (A: [](i32,i32)) (n: i64) =
  let r =
    loop A for _i < n do
      let (as, bs, cs) = unzip3 <| map (\(x,y) -> (x+1,y+1,x+y)) A
      let (xs, ys) = unzip <| scan (\(a,b) (x,y) -> (x+a,y+b)) (0,0) (zip as bs)
      in zip xs cs
  in map (.0) r

-- Before changes:
-- entry("main",
--       {A: opaque "arr_(i32, i32)_1d",
--        n: i64},
--       {*[]i32})
--   entry_main (d₀_5660 : i64,
--               A_5661 : [d₀_5660]i32,
--               A_5662 : [d₀_5660]i32,
--               n_5663 : i64)
--   : {*[d₀_5660]i32} = {
--   let {r_5835 : [d₀_5660]i32,
--        r_5836 : [d₀_5660]i32} =
--     loop {A_5838 : [d₀_5660]i32,
--           A_5839 : [d₀_5660]i32} = {A_5661, A_5662}
--     for _i_5837:i64 < n_5663 do {
--       let {defunc_0_scan_res_5919 : [d₀_5660]i32,
--            defunc_0_scan_res_5920 : [d₀_5660]i32,
--            defunc_0_map_res_5921 : [d₀_5660]i32} =
--         scanomap(d₀_5660,
--                  {A_5838, A_5839},
--                  {\ {eta_p_5864 : i32,
--                      eta_p_5865 : i32,
--                      eta_p_5866 : i32,
--                      eta_p_5867 : i32}
--                    : {i32,
--                       i32} ->
--                    let {tmp_5868 : i32} =
--                      add32(eta_p_5864, eta_p_5866)
--                    let {tmp_5869 : i32} =
--                      add32(eta_p_5865, eta_p_5867)
--                    in {tmp_5868, tmp_5869},
--                  {0i32, 0i32}},
--                  \ {eta_p_5908 : i32,
--                     eta_p_5909 : i32}
--                    : {i32,
--                       i32,
--                       i32} ->
--                    let {tmp_5910 : i32} =
--                      add32(1i32, eta_p_5908)
--                    let {tmp_5911 : i32} =
--                      add32(1i32, eta_p_5909)
--                    let {tmp_5912 : i32} =
--                      add32(eta_p_5908, eta_p_5909)
--                    in {tmp_5910, tmp_5911, tmp_5912})
--       in {defunc_0_scan_res_5919, defunc_0_map_res_5921}
--     }
--   let {defunc_0_map_res_5882 : [d₀_5660]i32} =
--     copy(r_5835)
--   in {defunc_0_map_res_5882}
-- }
