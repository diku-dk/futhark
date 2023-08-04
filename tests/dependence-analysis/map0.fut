-- ==
-- structure { BinOp 1 }

def main (A: [](i32,i32)) (n: i64) =
  let r =
    loop A for i < n do
      map (\(a,b) -> if i == 0 then (a,b) else (a+1,b+1)) A
  in map (.0) r

-- Before changes:
-- entry("main",
--       {A: opaque "arr_(i32, i32)_1d",
--        n: i64},
--       {*[]i32})
--   entry_main (d₀_5211 : i64,
--               A_5212 : [d₀_5211]i32,
--               A_5213 : [d₀_5211]i32,
--               n_5214 : i64)
--   : {*[d₀_5211]i32} = {
--   let {r_5256 : [d₀_5211]i32,
--        r_5257 : [d₀_5211]i32} =
--     loop {A_5259 : [d₀_5211]i32,
--           A_5260 : [d₀_5211]i32} = {A_5212, A_5213}
--     for i_5258:i64 < n_5214 do {
--       let {cond_5265 : bool} =
--         eq_i64(i_5258, 0i64)
--       let {defunc_0_map_res_5273 : [d₀_5211]i32,
--            defunc_0_map_res_5274 : [d₀_5211]i32} =
--         map(d₀_5211,
--             {A_5259, A_5260},
--             \ {eta_p_5263 : i32,
--                eta_p_5264 : i32}
--               : {i32,
--                  i32} ->
--               let {lifted_lambda_res_5266 : i32,
--                    lifted_lambda_res_5267 : i32} =
--                 if cond_5265
--                 then {eta_p_5263, eta_p_5264} else {
--                   let {tmp_5268 : i32} =
--                     add32(1i32, eta_p_5263)
--                   let {tmp_5269 : i32} =
--                     add32(1i32, eta_p_5264)
--                   in {tmp_5268, tmp_5269}
--                 }
--                 : {i32,
--                    i32}
--               in {lifted_lambda_res_5266, lifted_lambda_res_5267})
--       in {defunc_0_map_res_5273, defunc_0_map_res_5274}
--     }
--   let {defunc_0_map_res_5270 : [d₀_5211]i32} =
--     copy(r_5256)
--   in {defunc_0_map_res_5270}
-- }
