-- ==
-- input {
-- }
-- output {
--   [[0, 9, 18], [27, 36, 45], [54, 63, 72]]
-- }
fun [[int]] main() =
  let n = 9 in
  let a = map(fn [int] (int i) =>
                replicate(n,i),
              iota(n)) in
  let b = reshape((3,3,9),a) in
  map (fn [int] ([[int]] row) =>
         map (fn int ([int] x) => reduce(+,0,x), row),
       b)
