-- A local function whose closure refers to an array whose size is
-- *not* used inside the local function.
-- ==
-- input { 2i64 0 } output { 1i64 }

def main (n: i64) (x: i32) =
  let a = map (1 +) (iota n)
  let f (i: i32) = #[unsafe] a[i]
  -- 'unsafe' to prevent an assertion
  -- that uses the array length.
  in f x
