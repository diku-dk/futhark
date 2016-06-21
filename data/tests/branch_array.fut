-- Tests some nasty code generation/simplification details about
-- removing existential contexts.
--
-- ==
--
-- input { True 3 }
-- output { [0,1,2] }
-- input { False 3 }
-- output { [1337,1337,1337] }

fun []int f([n]int a) = a

fun []int g(int n) = replicate(n, 1337)

fun []int main(bool b, int n) =
  let a = iota(n) in
  if b then f(a) else g(n)
