-- ==
-- structure { BinOp 1 }

def main (A: [](i32,i32)) (n: i64) =
  let r =
    loop A for i < n do
      scan (\(a,b) (x,y) -> if i == 0 then (a,b) else (a+x,b+y)) (0,0) A
  in map (.0) r

-- Before changes:
-- entry("main",
--       {A: opaque "arr_(i32, i32)_1d",
--        n: i64},
--       {*[]i32})
--   entry_main (d₀_5256 : i64,
--               A_5257 : [d₀_5256]i32,
--               A_5258 : [d₀_5256]i32,
--               n_5259 : i64)
--   : {*[d₀_5256]i32} = {
--   let {r_5311 : [d₀_5256]i32,
--        r_5312 : [d₀_5256]i32} =
--     loop {A_5314 : [d₀_5256]i32,
--           A_5315 : [d₀_5256]i32} = {A_5257, A_5258}
--     for i_5313:i64 < n_5259 do {
--       let {cond_5322 : bool} =
--         eq_i64(i_5313, 0i64)
--       let {defunc_0_scan_res_5332 : [d₀_5256]i32,
--            defunc_0_scan_res_5333 : [d₀_5256]i32} =
--         scanomap(d₀_5256,
--                  {A_5314, A_5315},
--                  {\ {eta_p_5318 : i32,
--                      eta_p_5319 : i32,
--                      eta_p_5320 : i32,
--                      eta_p_5321 : i32}
--                    : {i32,
--                       i32} ->
--                    let {lifted_lambda_res_5323 : i32,
--                         lifted_lambda_res_5324 : i32} =
--                      if cond_5322
--                      then {eta_p_5318, eta_p_5319} else {
--                        let {tmp_5325 : i32} =
--                          add32(eta_p_5318, eta_p_5320)
--                        let {tmp_5326 : i32} =
--                          add32(eta_p_5319, eta_p_5321)
--                        in {tmp_5325, tmp_5326}
--                      }
--                      : {i32,
--                         i32}
--                    in {lifted_lambda_res_5323, lifted_lambda_res_5324},
--                  {0i32, 0i32}},
--                  \ {x_5327 : i32,
--                     x_5328 : i32}
--                    : {i32,
--                       i32} ->
--                    {x_5327, x_5328})
--       in {defunc_0_scan_res_5332, defunc_0_scan_res_5333}
--     }
--   let {defunc_0_map_res_5329 : [d₀_5256]i32} =
--     copy(r_5311)
--   in {defunc_0_map_res_5329}
-- }
