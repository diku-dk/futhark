-- Host-only operations block the migration of whole statements.
-- ==
-- structure gpu {
--   GPUBody 0
-- }

#[noinline]
def hostonly 'a (x: a) : a =
  -- This function can only be run on host and thus requires
  -- its argument to be made available there.
  let arr = opaque [x]
  in arr[0]

def main (A: [5]i64) : i64 =
  let x = if A[0] == 0 then A[1] else hostonly A[2]
   in loop y = x while y < 1000 do
        hostonly (y * A[y%5])
