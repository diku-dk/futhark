fun [int] main([int] a, [int] b) =
  let {c,d} = unzip(filter(fn bool (int x, int y) => x+y < 0, zip(a,b))) in
  filter(0<, d)
