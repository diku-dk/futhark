-- Function calls that do not work with arrays can be migrated.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/Index 2
--   /GPUBody/Apply 1
--   /Index 1
-- }

#[noinline]
def id2 'a (x: a) (y: a) : (a, a) =
  (x, y)

def main (arr: [3]i32) : i32 =
  let (a, b) = (arr[0], arr[1])
  let (_, y) = id2 a b
  in y
