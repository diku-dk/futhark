-- A local function may return a global array, but then the result of
-- applying it cannot be consumed.
-- ==
-- error: "f", which is not consumable

def global : []i32 = [1, 2, 3]

def f =
  let g (b: bool) = if b then global else []
  in g

def main (b: bool) = f b with [0] = 0
