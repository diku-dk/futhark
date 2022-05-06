-- Function calls that do not work with arrays can be migrated.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /GPUBody/Index 2
--   /GPUBody/Apply 1
--   /Index 1
-- }

#[noinline]
def plus (a: i32) (b: i32) : i32 =
  a + b

def main (arr: [2]i32) : i32 =
  let (a, b) = (arr[0], arr[1])
  in plus a b
