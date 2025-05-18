-- Inspired by #2271. No constructing impossible types just by making them
-- phantoms!
-- ==
-- error: "x"

def f (x: i64, _: bool) : [0][x]i32 = []
