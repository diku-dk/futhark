-- Applying a functor whose parameter signature contains a polymorphic 'val'
-- spec used to crash the type checker ("Is a type param").
-- ==
-- input { 42i32 } output { 42i32 }

module type vec = {val id 'a : a -> a}
module unit : vec = {def id 'a (x: a) : a = x}
module f (X: vec) : vec = X
module two = f unit

entry main (x: i32) = two.id x
