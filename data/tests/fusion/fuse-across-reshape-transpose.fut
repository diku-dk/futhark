fun [[int]] main() =
  let n = 9 in
  let a = map(+1,iota(n)) in
  let b = reshape((3,3),a) in
  let c = transpose(b) in
  map (fn [int] ([int] row) =>
         map (*2, row),
       c)
