-- The result of a function that may return a global array cannot be
-- consumed, even when the function is applied through "|>".
-- ==
-- error: "tl", which is not consumable

def global : [10]i32 = map i32.i64 (iota 10)

def tl (_: i32) : []i32 = global[1:]

def main (x: i32) = (x |> tl) with [0] = 1
