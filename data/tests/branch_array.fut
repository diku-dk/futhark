-- Tests some nasty code generation/simplification details about
-- removing existential contexts.
--
-- ==
--
-- input { True 3 }
-- output { [0,1,2] }
-- input { False 3 }
-- output { [1337,1337,1337] }

fun f(a: [n]int): []int = a

fun g(n: int): []int = replicate(n, 1337)

fun main(b: bool, n: int): []int =
  let a = iota(n) in
  if b then f(a) else g(n)
