-- ==
-- input {
-- }
-- output {
--   3
-- }
fun int main() =
    let n = 10 in
    let (a, b) = (copy(replicate(n,0)), copy(replicate(n,0))) in
    let a[0] = 1 in
    let b[0] = 2 in
    a[0] + b[0]
