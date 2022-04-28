-- Only migrate array reads when a reduction can be obtained.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/Index 3
--   /GPUBody/Apply 1
--   /Index 2
-- }

#[noinline]
def pick2 'a (x: a) (y: a) (z: a) : (a, a) =
  (x, z)

def main (arr: [3]i32) : (i32, i32) =
  let (a, b, c) = (arr[0], arr[1], arr[2])
  in pick2 a b c
