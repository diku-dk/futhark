-- Lifted type parameters allows for the definition of general polymorphic
-- function composition. Without them, we are limited in which functions can be
-- composed.
-- ==
-- error: Cannot instantiate the value type variable c with the functional type

let compose 'a 'b 'c (f : b -> c) (g : a -> b) : a -> c =
  \(x : a) -> f (g x)

let add (x : i32) (y : i32) : i32 = x+y
let double (x : i32) : i32 = x+x

let main (x : i32) (y : i32) : i32 =
  compose add double 3 5
