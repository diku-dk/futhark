-- Reads can be reduced from one iteration to the next.
-- ==
-- structure gpu {
--   /Loop/Index 0
-- }

def main [n] (A: *[n]f32) : *[n]f32 =
  let (A', _) =
    loop (A, x) = (A, 0)
    for i < n do
      let A' = map (+ x) A
      let y = A'[i]
      -- is delayed into next iteration
      let A' = A' with [i * i % n] = 42
      in (A', y)
  in A'
