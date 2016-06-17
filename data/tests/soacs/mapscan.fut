--
--
-- ==
-- tags { no_python }
-- compiled input { 100000 100} output { 15799424 }
--
fun int main(int n, int m) =
  let a = map(fn [m]int (int i) =>
                map(+i, iota(m)),
              iota(n)) in
  let b = map (fn [m]int ([m]int a_r) =>
                 scan(+, 0, a_r),
               a) in
  reduce(^, 0, reshape((n*m), b))
