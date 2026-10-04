-- A function may return a global array, but then its result cannot be
-- consumed.
-- ==
-- error: "f", which is not consumable

def global : []i32 = [1, 2, 3]

def f (b: bool) = if b then global else []

def main (b: bool) = f b with [0] = 0
