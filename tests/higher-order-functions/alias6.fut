-- Consuming the result of a pipeline through a closure-carrying function used
-- to be an error, because the lifted function was declared to return a nonfresh
-- value and the core language then assumed its result aliased the closure.
-- Parametricity now tells the type checker that the result is that of applying
-- "addto glob", which is fresh, and the instantiated type of "|>" says so.
-- ==
-- input { 5i64 }
-- output { [999i64, 25i64, 35i64] }

def glob : [3]i64 = [10, 20, 30]

def addto (a: [3]i64) (b: [3]i64) : *[3]i64 = map2 (+) a b

def main (k: i64) : [3]i64 =
  let a = [k, k, k] |> addto glob
  in a with [0] = 999
