-- Reshapes may not be migrated on their own as results from GPUBody constructs
-- are copied, which would change the asymptotic cost of the operation.
--
-- In general any scalar used within a type must be made available on host
-- before use.
-- ==
-- structure gpu {
--   GPUBody 0
--   Reshape 1
-- }

#[noinline]
def alter [n] (A: [n]i64) : *[]i64 =
  let l = n % 10
  in [l + 1] ++ A[:l] ++ A

#[noinline]
def modify [m] (A: [m]i64) (x: i64) : *[m]i64 =
  map (+ x) A

def main [n] (A: [n]i64) : *[]i64 =
  let A' = alter A
  let m = A'[0]
  -- must be read
  let B = A' :> [m]i64
  in modify B m
