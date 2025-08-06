-- Only migrate array reads when a reduction can be obtained.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/Index 2
--   /GPUBody/Apply 1
--   /Index 1
-- }

#[noinline]
def hostonly (x: i32) : i32 =
  -- This function can only be run on host and thus requires
  -- its argument to be made available there.
  if x == 42
  then (opaque [x])[0]
  else 42

#[noinline]
def id2 'a (x: a) (y: a) : (a, a) =
  (x, y)

def main (arr: [3]i32) : [3]i32 =
  let (a, b) = (arr[1], arr[2])
  let (x, y) = id2 a b
  let i = hostonly x
  in map (\j -> if j == y then j / i else j) arr
