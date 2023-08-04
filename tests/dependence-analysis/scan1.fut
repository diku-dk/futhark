def main (A: [](i32,i32)) (n: i64) =
  let r =
    loop A for _i < n do
      scan (\(a,b) (x,y) -> (x+a,y+b)) (0,0) (map (\(x,y) -> (x+1,y+1)) A)
  in map (.0) r

-- Before changes:
-- entry("main",
--       {A: opaque "arr_(i32, i32)_1d",
--        n: i64},
--       {*[]i32})
--   entry_main (d₀_5313 : i64,
--               A_5314 : [d₀_5313]i32,
--               A_5315 : [d₀_5313]i32,
--               n_5316 : i64)
--   : {*[d₀_5313]i32} = {
--   let {r_5386 : [d₀_5313]i32,
--        r_5387 : [d₀_5313]i32} =
--     loop {A_5389 : [d₀_5313]i32,
--           A_5390 : [d₀_5313]i32} = {A_5314, A_5315}
--     for _i_5388:i64 < n_5316 do {
--       let {defunc_0_scan_res_5420 : [d₀_5313]i32,
--            defunc_0_scan_res_5421 : [d₀_5313]i32} =
--         scanomap(d₀_5313,
--                  {A_5389, A_5390},
--                  {\ {eta_p_5399 : i32,
--                      eta_p_5400 : i32,
--                      eta_p_5401 : i32,
--                      eta_p_5402 : i32}
--                    : {i32,
--                       i32} ->
--                    let {tmp_5403 : i32} =
--                      add32(eta_p_5399, eta_p_5401)
--                    let {tmp_5404 : i32} =
--                      add32(eta_p_5400, eta_p_5402)
--                    in {tmp_5403, tmp_5404},
--                  {0i32, 0i32}},
--                  \ {eta_p_5412 : i32,
--                     eta_p_5413 : i32}
--                    : {i32,
--                       i32} ->
--                    let {tmp_5414 : i32} =
--                      add32(1i32, eta_p_5412)
--                    let {tmp_5415 : i32} =
--                      add32(1i32, eta_p_5413)
--                    in {tmp_5414, tmp_5415})
--       in {defunc_0_scan_res_5420, defunc_0_scan_res_5421}
--     }
--   let {defunc_0_map_res_5407 : [d₀_5313]i32} =
--     copy(r_5386)
--   in {defunc_0_map_res_5407}
-- }
