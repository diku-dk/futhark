--
--
-- ==
-- tags { no_python }
-- compiled input { 100000 100} output { 15799424 }
--
fun main(n: int, m: int): int =
  let a = map(fn (i: int): [m]int  =>
                map((+i), iota(m)),
              iota(n)) in
  let b = map (fn (a_r: [m]int): [m]int  =>
                 scan((+), 0, a_r),
               a) in
  reduce((^), 0, reshape (n*m) b)
