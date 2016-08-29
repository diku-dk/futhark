-- ==
-- input {
--   [1.0f32,-4.0f32,-2.4f32]
-- }
-- output {
--   -5.4f32
--   [2.0f32, -3.0f32, -1.4f32]
--   [3.0f32, -7.0f32, -3.8f32]
-- }
-- structure {
--      Redomap 1
-- }
--

default(f32)

fun main(arr: []f32): (f32,[]f32,[]f32) =
    let x = map     (+1.0) arr
    let y = zipWith (+) x arr
    let r = reduce  (+) (0.0) arr in
    (r,x,y)

