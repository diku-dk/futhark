-- A simple streamRed that does not permit interchange of the
-- reduction operator.
-- ==
-- input { [[1,2,3], [4,5,6], [6,7,8]] }
-- output { [11i32, 14i32, 17i32] }

fun vecadd(xs: *[m]int, ys: [m]int): *[m]int =
  loop (xs) = for i < m do
    let xs[i] = xs[i] + ys[i]
    in xs
  in xs

fun main(xss: [n][m]int): [m]int =
  streamRedPer(vecadd,
               fn (chunk_sz: int, acc: *[m]int, chunk: [][m]int): *[m]int  =>
                 loop (acc) = for i < chunk_sz do
                   vecadd(acc, chunk[i])
                 in acc,
               replicate(m, 0), xss)
