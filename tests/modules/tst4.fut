-- A functor with a closure
-- ==
-- input {}
-- output { 26 }

module A = { type t = i32 def x : t = 3 }
module F (X : { val b : i32 }) : { type t = i32 val c : t } =
   { type t = i32 def c = A.x + X.b }

module C = {
  module A = { type t = f32 }
  module B = F( { def b = 23 } )
}

def main : i32 = C.B.c
