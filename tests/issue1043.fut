-- ==
-- error: aliased to "xs"

let f 'a 'b (f: a -> b) (xs: a) =
  f xs

let main (xs: []i32) : *[]i32 =
  (`f`xs) id
