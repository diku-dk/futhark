-- ==
-- input {
--   [1,2,3,4,5,6,7,8,9]
--   [1,2,3,4,5,6,7,8,9]
-- }
-- output {
--   [1, 2, 3, 4, 5, 6, 7, 8, 9]
--   [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- }
fun main(a: []int, b: []int): ([]int,[]int) =
    let arr = zip(a,b)
    in unzip(map (fn (x: (int,int)): (int,int)  => x) arr)
