-- Only migrate array reads when a reduction can be obtained.
-- ==
-- structure gpu {
--   GPUBody 0
--   /Index 2
-- }

#[noinline]
def id2 'a (x: a) (y: a) : (a, a) =
  (x, y)

def main (arr: [3]i32) : (i32, i32) =
  let (a, b) = (arr[0], arr[1])
  in id2 a b
