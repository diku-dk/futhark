-- ==
-- structure { Map 2 }
fun [][]int main([n][m](int,int) a) =
  let b = map(fn [m](int,int) ([](int,int) row) =>
                map(fn (int,int) (int x, int y) =>
                      (x+y,x-y),
                    row),
                a) in
  let c = map(fn [n]int ([](int,int) row) =>
                map(+ , row),
              transpose(b)) in
  c
