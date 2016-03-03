-- ==
-- structure { Map 2 }
fun [[int]] main([[int,n]] a) =
  let b = map(fn [int,n] ([int] x1) => map(+1, x1), a) in
  let c = map(fn [int,n] ([int] z1) => map(*3, z1), transpose(b)) in
  c
