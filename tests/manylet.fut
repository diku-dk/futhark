-- Parser test.  'in' is optional except at the end of a chain of
-- let-bindings.

def main [n] (a: *[n]i32, x: i32) : [n]i32 =
  let y = x + 2
  let z = y + 3 + x
  let (a, _) =
    loop ((a, z)) for i < n do
      let tmp = z * z
      let a[i] = tmp
      let x = [a[i] - 1]
      let b = scatter a [i] x
      in (b, tmp + 2)
  in a
