-- ==
-- input {
--   [[1,2,3], [4,5,6]]
--   [[2,1,3], [4,6,5]]
-- }
-- output {
--   [[3, 3, 6], [8, 11, 11]]
-- }
fun [[int]] main([[int]] a1, [[int]] a2) =
  let b = map(fn [int] (([int],[int]) row) =>
                let (x,y) = row in
                map(+ , zip(x,y)),
              zip(a1,a2)) in
  b
