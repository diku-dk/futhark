fun [int] reverse([int] a) =
  map (fn int (int i) =>
         a[size(0,a)-i-1],
       iota(size(0,a)))

fun [int] main([int] a, [int] b) =
  concatMap(reverse, a, b)
