-- #2210
-- ==
-- error: Unknown name "n"

def f : [42]f32 =
  let [n] turtle: f32 -> [n]f32 = \(x: f32) -> replicate n x
  in turtle 42
