-- Migrated statements are moved into GPUBody statements that are combined.
--
-- Can merge multiple non-adjacent GPUBody statements.
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

def main (A: *[4]i32) : *[4]i32 =
  let (a1, a2) = id (A[0], A[1])
  -- gpu 1
  let b = hostonly a1
  -- host
  let c = b * a2
  -- gpu 2
  let (d1, d2) = id (a2 * 2, A[2])
  -- gpu 1
  let e = hostonly d1
  -- host
  let f = e * d2
  -- gpu 2
  in A with [2] = c + f
