-- ==
-- structure { Screma/BinOp 1 }

def main (X: []i32) (Y: []i32) (n: i64) =
  let free_ne = i32.i64 n * 2
  let A = (zip X Y)
  let r =
    loop A for _i < n do
      scan (\(a,b) (x,y) -> (x+a,y+b)) (free_ne,0) A
  in map (.0) r

-- Before changes:
-- entry("main",
--       {X: []i32,
--        Y: []i32,
--        n: i64},
--       {*[]i32})
--   entry_main (d₁_5271 : i64,
--               X_5272 : [d₁_5271]i32,
--               Y_5273 : [d₁_5271]i32,
--               n_5274 : i64)
--   : {*[d₁_5271]i32} = {
--   let {i64_res_5332 : i32} =
--     sext i64 n_5274 to i32
--   let {free_ne_5333 : i32} =
--     mul32(2i32, i64_res_5332)
--   let {r_5341 : [d₁_5271]i32,
--        r_5342 : [d₁_5271]i32} =
--     loop {A_5344 : [d₁_5271]i32,
--           A_5345 : [d₁_5271]i32} = {X_5272, Y_5273}
--     for _i_5343:i64 < n_5274 do {
--       let {defunc_0_scan_res_5359 : [d₁_5271]i32,
--            defunc_0_scan_res_5360 : [d₁_5271]i32} =
--         scanomap(d₁_5271,
--                  {A_5344, A_5345},
--                  {\ {eta_p_5348 : i32,
--                      eta_p_5349 : i32,
--                      eta_p_5350 : i32,
--                      eta_p_5351 : i32}
--                    : {i32,
--                       i32} ->
--                    let {tmp_5352 : i32} =
--                      add32(eta_p_5348, eta_p_5350)
--                    let {tmp_5353 : i32} =
--                      add32(eta_p_5349, eta_p_5351)
--                    in {tmp_5352, tmp_5353},
--                  {free_ne_5333, 0i32}},
--                  \ {x_5354 : i32,
--                     x_5355 : i32}
--                    : {i32,
--                       i32} ->
--                    {x_5354, x_5355})
--       in {defunc_0_scan_res_5359, defunc_0_scan_res_5360}
--     }
--   let {defunc_0_map_res_5356 : [d₁_5271]i32} =
--     copy(r_5341)
--   in {defunc_0_map_res_5356}
-- }
