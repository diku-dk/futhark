--
--
-- ==
-- tags { no_python }
-- input { 100 1000 } output { 870104 }
-- compiled input { 400 1000} output { 985824 }
-- compiled input { 100000 100} output { 15799424 }
--
fun main(n: int, m: int): int =
  let a = map (\(i: int): [m]int  ->
                map (+i) (iota(m))) (
              iota(n))
  let b = map  (\(a_r: [m]int): [m]int  ->
                 scan (+) 0 (a_r)) a in
  reduce (^) 0 (reshape (n*m) b)
