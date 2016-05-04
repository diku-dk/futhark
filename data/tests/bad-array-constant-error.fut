-- This array constant is not regular, and internalisation or type
-- checking should fail sensibly.
-- ==
-- error:
fun [([int],[int])] main () =
  [ ([1,2], [3,4,5]),
    ([4], [1,2,3,4])
  ]
