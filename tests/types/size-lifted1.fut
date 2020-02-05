-- Size-lifted types can be returned from loop.
--
-- Pretty ridiculous that we have to write it this way!
-- ==
-- input { 1 [1] } output { [1,1] }

let iterate '~a (n: i32) (f: (() -> a) -> a) (x: () -> a) =
  loop x = x () for _i < n do f (\() -> x)

let main n (xs: []i32) = iterate n (\(p : () -> []i32) -> p () ++ p ()) (\() -> xs)
