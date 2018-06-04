-- ==
-- input {
-- }
-- output {
--   [[[0, 2, 4, 6], [8, 10, 12, 14]], [[1, 3, 5, 7], [9, 11, 13, 15]]]
-- }
let main (): [][][]i32 =
  let xss = iota(16)
  let tmp = unflatten_3d 2 4 2 xss in
  tmp |> map transpose |> transpose
