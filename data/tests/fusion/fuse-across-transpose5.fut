fun [[int]] main([[int]] a) =
  let n = size(0,a) in
  let m = size(1,a) in
  let foo = replicate(m, iota(n)) in
  let bar = replicate(m, iota(n)) in
  let b = replicate(n, iota(m)) in
  let c = map(fn [int] ([int] xs, [int] ys,[int] zs) =>
                map(fn int (int x, int y, int z) => x+y*z, zip(xs,ys,zs)),
              zip(foo,bar,transpose(b))) in
  c
