fun bool div2(int x) = x % 2 = 0

fun bool div3(int x) = x % 3 = 0

fun [int] main([int] a, [int] b) =
  let {c1,c2} = unzip(filter(fn bool (int x, int y) =>
                               div2(x) || div3(y),
                             zip(a,b))) in
  filter(div2, c2)
