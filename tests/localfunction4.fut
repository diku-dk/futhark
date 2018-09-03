-- A local function whose closure refers to an array whose size is
-- *not* used inside the local function.
-- ==
-- input { 2 0 } output { 1 }

let main(n: i32) (x: i32) =
  let a = map (1+) (iota n)
  let f (i: i32) = unsafe a[i] -- 'unsafe' to prevent an assertion
                               -- that uses the array length.
  in f x
