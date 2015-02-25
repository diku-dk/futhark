fun [[int]] main(*[[int]] a, [int] b, int i) =
  let {br, _} = split(size(1,a), b) in
  let a[i] = br in
  a
