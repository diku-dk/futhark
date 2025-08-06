-- Track aliases properly in arrays in sum types.
-- ==
-- error: "x".*consumed

type t = #some ([2]i32) | #none

def consume (x: *t) : *[2]i32 =
  match x
  case #some arr -> arr with [0] = 1
  case _ -> [0, 0]

def main (x: *t) =
  let a = consume x
  let b = consume x
  in (a, b)
