-- x with [i].f = v should update field f inside array element i.
-- ==
-- input {}
-- output { 42 }

def main () : i32 =
let xs = [{f=1i32}, {f=2i32}]
let ys = xs with [0].f = 42
in ys[0].f