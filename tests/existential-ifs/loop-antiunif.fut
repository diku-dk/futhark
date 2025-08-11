-- Another simple test for index-function anti-unification across an if-then-else
-- This one returns the same memory block, only the offset is existentialized.
-- ==
-- input  { [5, 3, 2, 1, 5] }
-- output { 8 }
--
-- input  { [1, 2, 3, 4, 5, 6, 7] }
-- output { 22 }
def main [n] (arr: *[n]i32) : i32 =
  let xs =
    loop arr for _i < n / 2 do
      arr[1:]
  in reduce (+) 0 xs
