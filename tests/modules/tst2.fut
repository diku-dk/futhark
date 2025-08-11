-- Incompatible types check
-- ==
-- error: i32.*f32

module type T1 = {type t type s = t val a : s val f : s -> i32}
module X : T1 = {type t = f32 type s = i32 def a : s = 3 def f (x: s) : i32 = x}

-- err
def main () : i32 = X.f X.a
