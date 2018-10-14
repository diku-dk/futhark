-- Being more polymorphic inside a tuple is OK.
-- ==
-- input { 1 2 } output { 1 2 }

module type has_pair = { val fs 'a: (a -> a, a -> a) }

module with_pair: has_pair = { let fs = (\x -> x, \y -> y) }

let main (x: i32) (y: i32) = (with_pair.fs.1 x, with_pair.fs.1 y)
