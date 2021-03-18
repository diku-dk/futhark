type s = i32

entry a (x: s) : s = x + 2

type t1 = {x:[1]i32}
type t2 = t1

entry b (x: i32) : t1 = {x=[x + 3]}

entry c : t1 -> t2 = id

entry d ({x}: t2) : i32 = x[0]
