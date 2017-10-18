-- Opaque signature ascription must hide equality of type.
-- ==
-- error: type

module type S = { type t val a : t val f : t -> i32 }
module B : S = { type t = i32 let a:t = 3 let f (a:t):t = a }
module C : S = B
let main() : i32 = C.f B.a
