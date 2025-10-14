-- Reads can be delayed into loops given that the number of reads done by the
-- loop remains unchanged.
--
-- In this case the reads are not delayed as the worst case number of reads per
-- iteration would increase by one.
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
  let x = A[0]
  let y = A[1]
  in loop z = 0
     for i < 10 do
       hostonly (if z != 0 then 42 else id (x + z) + y)
