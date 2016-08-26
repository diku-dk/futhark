-- ==
-- input {
-- }
-- output {
--   0
-- }
fun main(): int =
  let n = 10 in
  let a = iota(n) in
  let b = iota(n) in
  let c = (a,b) in
  let (a_,unused_b) = (a,b) in
  let a[0] = 0 in -- Only a_ and a are consumed.
  let (unused_a,b_) = (a,b) in
  let b[0] = 1 in -- Only b_ and b are consumed.
  0
