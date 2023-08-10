-- TODO A scatter on two arrays is still just one scatter.
-- How to structure test?

def main [n] (A: *[n](i32,i32)) =
  let r =
    loop A for i < n do
      scatter A (iota n) (copy A)
  in map (.0) r


-- Before changes:
-- entry("main",
--       {A: *opaque "arr_(i32, i32)_1d"},
--       {*[]i32})
--   entry_main (n_5243 : i64,
--               A_5244 : *[n_5243]i32,
--               A_5245 : *[n_5243]i32)
--   : {*[n_5243]i32} = {
--   let {iota_res_5296 : [n_5243]i64} =
--     iota64(n_5243, 0i64, 1i64)
--   let {r_5286 : [n_5243]i32,
--        r_5287 : [n_5243]i32} =
--     loop {A_5289 : *[n_5243]i32,
--           A_5290 : *[n_5243]i32} = {A_5244, A_5245}
--     for i_5288:i64 < n_5243 do {
--       let {smaller_replicate_5291 : [n_5243]i32} =
--         copy(A_5289)
--       let {smaller_replicate_5292 : [n_5243]i32} =
--         copy(A_5290)
--       let {scatter_res_5305 : [n_5243]i32,
--            scatter_res_5306 : [n_5243]i32} =
--         scatter(n_5243,
--                 {iota_res_5296, smaller_replicate_5291, smaller_replicate_5292},
--                 \ {write_index_5299 : i64,
--                    write_value_5300 : i32,
--                    write_value_5301 : i32}
--                   : {i64,
--                      i64,
--                      i32,
--                      i32} ->
--                   {write_index_5299, write_index_5299, write_value_5300, write_value_5301},
--                 ([n_5243], 1, A_5289), ([n_5243], 1, A_5290))
--       in {scatter_res_5305, scatter_res_5306}
--     }
--   in {r_5286}
-- }
