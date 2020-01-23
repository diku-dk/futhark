-- Size-lifted types can be returned from loop.
-- FIXME: the test below should work.
-- ==

-- input { 1 [1] } output { [1,1] }

let iterate '~a (n: i32) (f: a -> a) (x: a) =
  loop x for _i < n do f x

let main n (xs: []i32) = iterate n (\p -> p++p) xs
