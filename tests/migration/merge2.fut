-- Migrated statements are moved into GPUBody statements that are combined.
--
-- Can merge multiple non-adjacent GPUBody statements.
-- ==
-- structure gpu {
--   GPUBody 1
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
  let (a, b) = id (A[0], A[1])
  -- gpu
  let c = hostonly a
  -- host
  let (x, y) = id (A[2], A[3])
  -- gpu
  let z = hostonly y
  -- host
  let B = A with [0] = b + x
  -- gpu
  in map (+ c + z) B
