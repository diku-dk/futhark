-- Incompatible types check
-- ==
-- error: Type s specified as t

module type T1 = { type t type s = t val a : s val f : s -> i32 }
module X : T1 = { type t = f32 type s = i32 let a : s = 3 let f (x:s) : i32 = x }    -- err
let main () : i32 = X.f X.a
