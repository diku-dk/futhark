-- A functor with a closure
-- ==
-- input {}
-- output { 26 }

module A = { type t = int val x : t = 3 }
module F (X : { val b : int }) : { type t = int val c : t } =
   { type t = int val c = A.x + X.b }

module C = {
  module A = { type t = f32 }
  module B = F( { val b = 23 } )
}

fun main() : int = C.B.c