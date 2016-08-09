-- ==
-- input {
-- }
-- output {
--   [1, 2, 3, 4, 5, 6, 7, 8, 9]
--   [[2, 4, 6], [8, 10, 12], [14, 16, 18]]
-- }
fun ([]int,[][]int) main() =
  let n = 9 in
  let a = map(+1,iota(n)) in
  let b = reshape((3,3),a) in
  let c = map (fn []int ([]int row) =>
                    map (fn int (int x) => x*2, row)
              , b)
  in (a,c)
