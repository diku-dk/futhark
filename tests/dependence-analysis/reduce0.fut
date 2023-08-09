-- ==
-- structure { BinOp 2 }

def tadd ((x1, y1): (i32, i32)) (x2, y2) = (x1+x2, y1+y2)

def main (X: []i32) (Y: []i32) (n: i64) =
  let A = (zip X Y)
  let r =
    loop r' = (0,0) for i < n do
      reduce (\(a,b) (x,y) -> if i == 0 then (a,b) else (a+x,b+y))
             (0,0)
             (map (tadd r') A)
  in r.0

-- Before changes:
-- entry("main",
--       {X: []i32,
--        Y: []i32,
--        n: i64},
--       {i32})
--   entry_main (d₁_5328 : i64,
--               X_5329 : [d₁_5328]i32,
--               Y_5330 : [d₁_5328]i32,
--               n_5331 : i64)
--   : {i32} = {
--   let {main_res_5397 : i32,
--        main_res_5398 : i32} =
--     loop {r'_5400 : i32,
--           r'_5401 : i32} = {0i32, 0i32}
--     for i_5399:i64 < n_5331 do {
--       let {cond_5414 : bool} =
--         eq_i64(i_5399, 0i64)
--       let {defunc_0_reduce_res_5431 : i32,
--            defunc_0_reduce_res_5432 : i32} =
--         redomap(d₁_5328,
--                 {X_5329, Y_5330},
--                 {\ {eta_p_5410 : i32,
--                     eta_p_5411 : i32,
--                     eta_p_5412 : i32,
--                     eta_p_5413 : i32}
--                   : {i32,
--                      i32} ->
--                   let {lifted_lambda_res_5415 : i32,
--                        lifted_lambda_res_5416 : i32} =
--                     if cond_5414
--                     then {eta_p_5410, eta_p_5411} else {
--                       let {tmp_5417 : i32} =
--                         add32(eta_p_5410, eta_p_5412)
--                       let {tmp_5418 : i32} =
--                         add32(eta_p_5411, eta_p_5413)
--                       in {tmp_5417, tmp_5418}
--                     }
--                     : {i32,
--                        i32}
--                   in {lifted_lambda_res_5415, lifted_lambda_res_5416},
--                 {0i32, 0i32}},
--                 \ {eta_p_5423 : i32,
--                    eta_p_5424 : i32}
--                   : {i32,
--                      i32} ->
--                   let {tmp_5425 : i32} =
--                     add32(r'_5400, eta_p_5423)
--                   let {tmp_5426 : i32} =
--                     add32(r'_5401, eta_p_5424)
--                   in {tmp_5425, tmp_5426})
--       in {defunc_0_reduce_res_5431, defunc_0_reduce_res_5432}
--     }
--   in {main_res_5397}
-- }
