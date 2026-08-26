-- Consuming the result of a pipeline through a closure-carrying
-- function must still be an error, as the code generator conservatively
-- assumes the result aliases the closure.  This should be a proper type
-- error, not an internal compiler error.
-- ==
-- error: not consumable

def glob : [3]i64 = [10, 20, 30]

def addto (a: [3]i64) (b: [3]i64) : *[3]i64 = map2 (+) a b

def main (k: i64) =
  let a = [k, k, k] |> addto glob
  in a with [0] = 999
