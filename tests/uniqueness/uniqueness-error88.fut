-- A global array with size parameters may be (a slice of) another global
-- array, so it cannot be consumed.
-- ==
-- error: "zs", which is not consumable

def global : [10]i32 = map i32.i64 (iota 10)

def zs [n] : [n]i32 = global[:n]

def main (k: i64) =
  let a = zs : [k]i32
  let a[0] = 42
  in (a, global[0])
