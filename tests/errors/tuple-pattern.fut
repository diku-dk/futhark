-- Tuple patterns must match the number of elements in their rhs.
--
-- ==
-- error: match

let main(): i32 =
  let (x,y) = (1,2,3)
  in x+y
