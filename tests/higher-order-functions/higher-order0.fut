-- id id id ...
-- ==
-- input { 378 } output { 378 }

let id '^a (x : a) : a = x

let main (x : i32) = id id id id x
