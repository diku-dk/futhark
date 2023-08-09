-- ==
-- structure { Screma/BinOp 2 }

def tadd ((x1, y1): (i32, i32)) (x2, y2) = (x1+x2, y1+y2)

def main (X: []i32) (Y: []i32) (n: i64) =
  let A = (zip X Y)
  let r =
    loop x = (0,0) for _i < n do
      reduce (\(a,b) (x,y) -> (x+a,y+b)) (0,0) (map (tadd x) A)
  in r.0

-- Before changes:
-- entry("main",
--       {X: []i32,
--        Y: []i32,
--        n: i64},
--       {i32})
--   entry_main (d₁_5311 : i64,
--               X_5312 : [d₁_5311]i32,
--               Y_5313 : [d₁_5311]i32,
--               n_5314 : i64)
--   : {i32} = {
--   let {main_res_5385 : i32,
--        main_res_5386 : i32} =
--     loop {x_5388 : i32,
--           x_5389 : i32} = {0i32, 0i32}
--     for _i_5387:i64 < n_5314 do {
--       let {defunc_0_reduce_res_5416 : i32,
--            defunc_0_reduce_res_5417 : i32} =
--         redomap(d₁_5311,
--                 {X_5312, Y_5313},
--                 {\ {eta_p_5398 : i32,
--                     eta_p_5399 : i32,
--                     eta_p_5400 : i32,
--                     eta_p_5401 : i32}
--                   : {i32,
--                      i32} ->
--                   let {tmp_5402 : i32} =
--                     add32(eta_p_5398, eta_p_5400)
--                   let {tmp_5403 : i32} =
--                     add32(eta_p_5399, eta_p_5401)
--                   in {tmp_5402, tmp_5403},
--                 {0i32, 0i32}},
--                 \ {eta_p_5408 : i32,
--                    eta_p_5409 : i32}
--                   : {i32,
--                      i32} ->
--                   let {tmp_5410 : i32} =
--                     add32(x_5388, eta_p_5408)
--                   let {tmp_5411 : i32} =
--                     add32(x_5389, eta_p_5409)
--                   in {tmp_5410, tmp_5411})
--       in {defunc_0_reduce_res_5416, defunc_0_reduce_res_5417}
--     }
--   in {main_res_5385}
-- }
