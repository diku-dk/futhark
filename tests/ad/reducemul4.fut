-- ==
-- entry: fwd rev
-- input { [1f32, 2f32, 3f32, 4f32] } output { [[48f32, 12f32, 8f32, 6f32], [48f32, 48f32, 16f32, 12f32], [72f32, 36f32, 48f32, 18f32], [96f32, 48f32, 32f32, 48f32]] }

def fun [n] (as: [n]f32) =
  let x = reduce (*) 1 as 
  in map (*x) as

entry fwd [n] (as: [n]f32) =
  tabulate n (\i -> jvp fun as (replicate n 0 with [i] = 1))
  |> transpose

entry rev [n] (as: [n]f32) =
  tabulate n (\i -> vjp fun as (replicate n 0 with [i] = 1))