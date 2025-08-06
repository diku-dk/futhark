-- Tests some nasty code generation/simplification details about
-- removing existential contexts.
--
-- ==
--
-- input { true 3i64 }
-- output { [0i64,1i64,2i64] }
-- input { false 3i64 }
-- output { [1337i64,1337i64,1337i64] }

def f [n] (a: [n]i64) : []i64 = a

def g (n: i64) : []i64 = replicate n 1337

def main (b: bool) (n: i64) : []i64 =
  let a = iota (n)
  in if b then f (a) else g (n)
