-- The merge parameter in a loop cannot have function type.
-- ==
-- error: The type of the merge expression in a do-loop must be of order 0.*

let id 'a (x : a) : a = x

let main (n : i32) =
  loop f = id for i < n do
    \(y:i32) -> f y
