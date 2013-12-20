fun int main([[int]] a) =
  let b = map(fn [int] ([int] z1) =>
                map(op*(3), z1),
              a) in
  let ravgs = map(fn int ([int] r) =>
                    let n = size(0, r) in
                    reduce(op+, 0, r) / n,
                  transpose(b)) in
  let res = reduce(op+, 0, ravgs) in
  res
