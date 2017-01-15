-- If the reduction function accumulator type is unique, consume the
-- initial value, but only as much as is actually unique!
-- ==
-- input {
--   [0,0,0,0,0,0,0,0,0,0]
--   [1,1,1,1,1,1,1,1,1,1]
-- }
-- output {
--   [1i32, 11i32, 21i32, 31i32, 41i32, 51i32, 61i32, 71i32, 81i32, 91i32]
-- }

fun main(a: *[]int,b: []int): []int =
  let (x,y) =
    reduce (\(acc: (*[]int, []int)) (arr: ([]int, []int)): (*[]int, []int)  ->
             let (a1,b1) = acc
             let (a2,b2) = arr
             in (map (+) a1 a2,
                 map (*) b1 b2))
           (a,b) (zip (copy(replicate 10 (iota 10))) (replicate 10 (iota 10)))
  in map (+) b x -- Should be OK, because only a has been consumed.
