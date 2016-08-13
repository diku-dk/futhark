-- ==
-- structure { Map 2 }
fun [][]int main([n][m]int a_1, [n][m]int a_2) =
  let a = zipWith(fn [m](int,int) ([m]int a_1r, [m]int a_2r) => zip(a_1r, a_2r),
                  a_1, a_2)
  let b = map(fn [m](int,int) ([](int,int) row) =>
                map(fn (int,int) (int x, int y) =>
                      (x+y,x-y),
                    row),
                a)
  let c = map(fn [n]int ([](int,int) row) =>
                map(+ , row),
              transpose(b))
  in c
