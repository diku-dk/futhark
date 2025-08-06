-- A lambda whose free variable has been consumed.
-- ==
-- error: "QUUX".*consumed

def main (y: i32, QUUX: *[]i32) =
  let f = \x -> x + QUUX[0]
  let QUUX[1] = 2
  in f y
