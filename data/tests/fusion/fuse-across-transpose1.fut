// --
fun [[int]] main([[int]] a) =
  let b = map(fn [int] ([int] x1) => map(+1, x1), a) in
  let c = map(fn [int] ([int] z1) => map(*3, z1), transpose(b)) in
  c
