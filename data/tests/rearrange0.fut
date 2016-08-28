-- ==
-- input {
-- }
-- output {
--   [[[0, 2, 4, 6], [8, 10, 12, 14]], [[1, 3, 5, 7], [9, 11, 13, 15]]]
-- }
fun main (): [][][]int =
  let xss = iota(16) in
  let tmp = reshape (2,4,2) xss in
  rearrange (2,0,1) tmp
