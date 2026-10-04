-- A lambda declared to return a fresh value may not return an outer local.
-- ==
-- error: not consumable

def main (x: *[]i32) : i32 =
  let f = \(_: i32) : *[]i32 -> x
  let r = f 0
  let r[0] = 10
  in x[0]
