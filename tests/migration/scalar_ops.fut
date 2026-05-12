-- This test verifies that a host-only usage can be found through all major
-- scalar expression types, indicating that migration analysis covers all of
-- them.
-- ==
-- structure gpu {
--   GPUBody 0
-- }

#[noinline]
def hostonly 'a (x: a) : bool =
  -- This function can only be run on host and thus requires
  -- its argument to be made available there.
  let arr = opaque [true]
  in arr[0]

#[noinline]
def join 'a (x: a) (y: a) : a =
  x

#[noinline]
def join3 'a (x: a) (y: a) (z: a) : a =
  x

-- 'SubExp' cannot be tested due to elimination by the simplifier.

entry opaque (A: [2]i32) : i32 =
  let (a, b) = (A[0], A[1])
  let x = opaque b
  in if hostonly x
     then join a b
     else 0

entry unOp (A: [2]i32) : i32 =
  let (a, b) = (A[0], A[1])
  let x = i32.abs b
  in if hostonly x
     then join a b
     else 0

entry binOp (A: [3]i32) : i32 =
  let (a, b, c) = (A[0], A[1], A[2])
  let x = a + 4
  let y = 2 + b
  in if hostonly x || hostonly y
     then join3 a b c
     else 0

entry cmpOp (A: [3]i32) : i32 =
  let (a, b, c) = (A[0], A[1], A[2])
  let x = a == 4
  let y = 2 == b
  in if hostonly x || hostonly y
     then join3 a b c
     else 0

entry convOp (A: [2]i32) : i32 =
  let (a, b) = (A[0], A[1])
  let x = i32.to_i64 b
  in if hostonly x
     then join a b
     else 0
