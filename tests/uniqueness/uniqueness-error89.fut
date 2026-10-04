-- A component of a polymorphic value whose type does not mention the type
-- parameter may be a global array, so it cannot be consumed.
-- ==
-- error: "pv", which is not consumable

def global : [3]i32 = [1, 2, 3]

def pv 'a : ([]i32, []a) = (global, [])

def main (_: i32) =
  let (p, _) = pv : ([]i32, []f32)
  in p with [0] = 1
