-- Array reads are sunk to their deepest possible branch.
-- ==
-- structure gpu {
--   GPUBody 1
--   /Loop/Index 0
--   /Loop/If/True/GPUBody 1
--   /Loop/If/True/Index 1
-- }

#[noinline]
def hostonly 'a (x: a) : i32 =
  -- This function can only be run on host and thus requires
  -- its argument to be made available there.
  let arr = opaque [7]
  in arr[0]

def main (A: [5](i32, i32)) (x: i32) : (i32, i32) =
  loop (res, c) = (0, x)
  for (y, z) in A do
    if c == 3
    then (y + 1, hostonly z)
    else -- reads of y and z should be sunk here
         (res, c + 1)
