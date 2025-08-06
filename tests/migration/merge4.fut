-- Migrated statements are moved into GPUBody statements that are combined.
--
-- Can merge non-adjacent GPUBody statements but only if the latter does not
-- depend upon the prior through a non-GPUBody statement.
-- ==
-- structure gpu {
--   GPUBody 2
-- }

#[noinline]
def hostonly 'a (x: a) : i32 =
  -- This function can only be run on host and thus requires
  -- its argument to be made available there.
  let arr = opaque [42]
  in arr[0]

#[noinline]
def id 'a (x: a) : a = x

def main (A: *[3]i32) : *[3]i32 =
  let (x, y) = id (A[0], A[1])
  -- gpu 1
  let z = hostonly x
  -- host
  in A with [0] = y + z

-- gpu 2
