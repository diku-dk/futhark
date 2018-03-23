-- The merge parameter in a loop cannot have function type.
-- ==
-- error: used as loop variable

let id 'a (x : a) : a = x

let main (n : i32) =
  loop f = id for i < n do
    \(y:i32) -> f y
