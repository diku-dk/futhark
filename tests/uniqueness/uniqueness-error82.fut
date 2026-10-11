-- A lambda declared to return a fresh value may not return a global.
-- ==
-- error: not consumable

def glob : []i32 = [1, 2, 3]

def main (n: i32) : i32 =
  let f = \(_: i32) : *[]i32 -> glob
  let r = f n
  let r[0] = 10
  in glob[0]
