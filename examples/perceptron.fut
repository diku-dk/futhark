-- An implementation of the venerable Perceptron algorithm.
--
-- For clarity, uses very few library functions; in practice the
-- /futlib/linalg module would probably be useful here.
--
-- ==
-- input {
--
-- [1.0f32, 1.0f32, 1.0f32]
--
-- [[1.0f32, 0.6492f32, 10.5492f32], [1.0f32, 5.0576f32, -1.9462f32],
-- [1.0f32, -5.9590f32, 7.8897f32], [1.0f32, 2.9614f32, 1.3547f32],
-- [1.0f32, 3.6815f32, 1.6019f32], [1.0f32, 5.3024f32, 3.9243f32],
-- [1.0f32, 1.9835f32, 2.3669f32], [1.0f32, -3.4360f32, 8.0828f32],
-- [1.0f32, 6.1168f32, 2.3159f32], [1.0f32, 6.2850f32, -0.4685f32],
-- [1.0f32, 4.4086f32, 1.3710f32], [1.0f32, -3.7105f32, 8.4309f32],
-- [1.0f32, -2.3741f32, 6.1648f32], [1.0f32, 0.4221f32, 8.5627f32],
-- [1.0f32, -3.5980f32, 9.2361f32], [1.0f32, -4.5349f32, 9.6428f32],
-- [1.0f32, 1.6828f32, 0.5335f32], [1.0f32, 5.3271f32, -1.5529f32],
-- [1.0f32, 3.2860f32, 3.1965f32], [1.0f32, 5.2880f32, 1.2030f32],
-- [1.0f32, -3.7126f32, 12.7188f32], [1.0f32, -2.5362f32, 6.8989f32],
-- [1.0f32, -2.0253f32, 5.1877f32], [1.0f32, 6.7019f32, 3.8357f32],
-- [1.0f32, -2.9775f32, 8.5460f32], [1.0f32, 2.4272f32, -0.4192f32],
-- [1.0f32, 3.7186f32, 4.0874f32], [1.0f32, -4.3252f32, 6.1897f32],
-- [1.0f32, -4.8112f32, 9.7657f32], [1.0f32, -3.4481f32, 10.0994]]
--
-- [-1f32, 1f32, -1f32, 1f32, 1f32, 1f32, 1f32, -1f32, 1f32, 1f32,
-- 1f32, -1f32, -1f32, -1f32, -1f32, -1f32, 1f32, 1f32, 1f32, 1f32,
-- -1f32, -1f32, -1f32, 1f32, -1f32, 1f32, 1f32, -1f32, -1f32, -1f32]
--
-- 100
--
-- 1f32
-- }
-- output {
-- 6i32
-- [2.000000f32, 8.614600f32, -4.270200f32]
-- 1.000000f32
-- }

let dotV [d] (x: [d]f32) (y: [d]f32): f32 =
  reduce (+) 0.0 (map2 (*) x y)

let addV [d] (x: [d]f32) (y: [d]f32): [d]f32 =
  map2 (+) x y

let scaleV [d] (x: [d]f32) (a: f32): [d]f32 =
  map (*a) x

let checkClass [d] (w: [d]f32) (x: [d]f32): f32 =
  if dotV x w > 0.0 then 1.0 else -1.0

let checkList [d][m] (w: [d]f32) (xs: [m][d]f32) (ys: [m]f32): bool =
  reduce (&&) true (map2 (\x y -> checkClass w x * y != -1.0) xs ys)

let accuracy [d][m] (w: [d]f32) (xs: [m][d]f32) (ys: [m]f32): f32 =
  reduce (+) 0.0 (map2 (\x y -> f32.bool (checkClass w x * y != -1.0)) xs ys)

let train [d] (w: [d]f32) (x: [d]f32) (y: f32) (eta: f32): [d]f32 =
  if checkClass w x == y then w
  else addV w (scaleV (scaleV x eta) y)

-- Returns: #iterations, final 'w', accuracy from 0-1.
let main [d][m] (w: [d]f32) (xd: [m][d]f32) (yd: [m]f32) (limit: i32) (eta: f32): (i32, [d]f32, f32) =
  let (w,i) = loop (w, i) = (w, 0) while i < limit && !(checkList w xd yd) do
    -- Find data for this iteration.
    let x = xd[i%m]
    let y = yd[i%m]
    in (train w x y eta, i+1)
  in (i, w, accuracy w xd yd / r32(m))
