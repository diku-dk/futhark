// Nasty loop whose size cannot be predicted in advance.

fun [int] main([int] xs, int n) =
  loop (xs) = for i < n do
    concat(xs,xs)
  in xs
