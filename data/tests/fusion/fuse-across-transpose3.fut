// --
// structure { Redomap 2 }
fun int main([[int,m],n] a) =
  let b = map(fn [int,m] ([int] z1) =>
                map(*3, z1),
              a) in
  let ravgs = map(fn int ([int] r) =>
                    let n = size(0, r) in
                    reduce(+, 0, r) / n,
                  transpose(b)) in
  let res = reduce(+, 0, ravgs) in
  res
