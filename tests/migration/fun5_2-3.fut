-- Only migrate array reads when a reduction can be obtained.
-- ==
-- structure gpu {
--   GPUBody 0
--   /Index 2
-- }

#[noinline]
def id3 'a (x: a) (y: a) (z: a) : (a, a, a) =
  (x, y, z)

def main (arr: [3]i32) : (i32, i32, i32) =
  let (a, b) = (arr[0], arr[1])
  in id3 a b a
