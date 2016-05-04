-- ==
-- input {
--   [[1,2,3],[4,5,6]]
--   [[6,5,4],[3,2,1]]
-- }
-- output {
--   [[7, 7, 7], [7, 7, 7]]
--   [[-5, -3, -1], [1, 3, 5]]
-- }
fun [[(int,int)]] inner([[(int,int)]] a) =
  map(fn [(int,int)] ([(int,int)] row) =>
        map(fn (int,int) (int x, int y) =>
              (x+y,x-y),
            row),
        a)

fun ([[int]], [[int]]) main([[int]] a1, [[int]] a2) =
  let a = map(fn [(int,int)] (([int],[int]) p) =>
                let (p1,p2) = p in
                zip(p1,p2),
              zip(a1,a2)) in
  unzip(map(fn ([int], [int]) ([(int,int)] r) =>
              unzip(r),
            inner(a)))
