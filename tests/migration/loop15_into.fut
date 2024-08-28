-- Reads can be delayed into loops given that the number of reads done by the
-- loop remains unchanged.
-- ==
-- structure gpu {
--   /GPUBody 1
--   /Loop/GPUBody 1
-- }

#[noinline]
def hostonly (x: i64) : i64 =
  -- This function can only be run on host.
  if x == 42 then (opaque [x])[0]
             else x

def main (A: [10]i64) : [10]i64 =
  let x = A[0]
  let (_, A') =
    loop (x, A) = (x, A) for i < 10 do
      let y = x+1
      let z = hostonly y
       in (A[i], map (+z) A)
   in A'
