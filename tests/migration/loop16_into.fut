-- Reads can be delayed into loops given that the number of reads done by the
-- loop remains unchanged.
--
-- In this case the reads are not delayed as the number of reads per iteration
-- would increase by one.
-- ==
-- structure gpu {
--   /Index 2
--   GPUBody 0
-- }

#[noinline]
def hostonly (x: i64) : i64 =
  -- This function can only be run on host.
  if x == 42
  then (opaque [x])[0]
  else 42

def main (A: [10]i64) : i64 =
  let (a, b) =
    loop (x, y) = (A[0], A[1])
    for i < 10 do
      let z = hostonly (x + y)
      in (z % 22, z * z)
  in a + b
