-- ==
-- input { 42 } output { 42 }

def main (x: i32) : i32 =
let xs = [{f=1i32}, {f=2i32}]
let ys = xs with [0].f = x
in ys[0].f