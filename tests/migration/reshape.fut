-- Reshapes cannot be migrated to prevent dimension size variables from being
-- read.
-- ==
-- structure gpu {
--   GPUBody 0
--   Reshape 1
-- }

#[noinline]
def alter [n] (A: [n]i64) : *[]i64 =
  let l = n%10
   in [l+1] ++ A[:l] ++ A

#[noinline]
def modify [m] (A: [m]i64) (x: i64) : *[m]i64 =
  map (+x) A

def main [n] (A: [n]i64) : *[]i64 =
  let A' = alter A
  let m = A'[0] -- must be read
  let B = A' :> [m]i64
   in modify B m
