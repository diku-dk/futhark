-- Only migrate array reads when a reduction can be obtained.
-- ==
-- structure gpu {
--   /GPUBody 0
--   /Index 2
-- }

#[noinline]
def pick2 'a (x: a) (y: a) (z: a) : (a, a) =
  (x, y)

def main (arr: [3]i32) : (i32, i32) =
  let (a, b) = (arr[1], arr[2])
  in pick2 a b a
