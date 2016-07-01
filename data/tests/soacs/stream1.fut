-- A simple streamRed that does not permit interchange of the
-- reduction operator.
-- ==
-- input { [[1,2,3], [4,5,6], [6,7,8]] }
-- output { [11i32, 14i32, 17i32] }

fun *[m]int vecadd(*[m]int xs, [m]int ys) =
  loop (xs) = for i < m do
    let xs[i] = xs[i] + ys[i]
    in xs
  in xs

fun [m]int main([n][m]int xss) =
  streamRedPer(vecadd,
               fn *[m]int (int chunk_sz, *[m]int acc, [][m]int chunk) =>
                 loop (acc) = for i < chunk_sz do
                   vecadd(acc, chunk[i])
                 in acc,
               replicate(m, 0), xss)
