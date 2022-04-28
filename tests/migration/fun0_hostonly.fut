-- Do not migrate the application of functions that work with arrays.
-- These are subject to compiler limitations.
-- ==
-- structure gpu {
--   GPUBody 0
--   /Index 3
-- }

#[noinline]
def fun'' (a: i32) (b: i32) : i32 =
  let xs = scan (+) 0 (replicate 10 a)
  in xs[b % 10]

#[noinline]
def fun' (a: i32) (b: i32) : i32 =
  fun'' a b

#[noinline]
def fun (a: i32) (b: i32) : i32 =
  fun' a b

def main (arr: [2]i32) : i32 =
  let (a, b) = (arr[0], arr[1])
  in fun a b
