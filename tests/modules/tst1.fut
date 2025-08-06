module type T1 = {type t type s = t val a : s val f : s -> i32}
module X : T1 = {type t = i32 type s = i32 def a : s = 3 def f (x: s) : i32 = x}

-- ok
def main () : i32 = X.f X.a
