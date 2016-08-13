-- ==
-- input {
--   [1,2,3,4,5,6,7,8,9]
--   [1,2,3,4,5,6,7,8,9]
-- }
-- output {
--   [1, 2, 3, 4, 5, 6, 7, 8, 9]
--   [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- }
fun ([]int,[]int) main([]int a, []int b) =
    let arr = zip(a,b)
    in unzip(map(fn (int,int) ((int,int) x) => x, arr))
