-- Sum rows in a matrix
def main [n] (xss: [n][n]i32) : [n]i32 =
    map i32.sum xss

-- === Expected output of analysis: CONFIRMED
-- (segmap) defunc_0_map_res_5226 : {
--     (arr) xss_5149 : {
--         (idx) x_5234 :
--             0 : dependencies = [ gtid_5227 1 par ]
--             1 : dependencies = [ i_5269 2 seq ]
--     }
-- }
-- (segred) defunc_0_map_res_5258 : {
--     (arr) xss_5149 : {
--         (idx) x_5266 :
--             0 : dependencies = [ gtid_5259 1 par ]
--             1 : dependencies = [ gtid_5260 1 par ]
--     }
-- }
