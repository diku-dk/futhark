-- Opaque signature ascription must hide equality of type.
-- ==
-- error: type

module type S = {type t val a : t val f : t -> i32}
module B : S = {type t = i32 def a : t = 3 def f (a: t) : t = a}
module C : S = B
def main () : i32 = C.f B.a
