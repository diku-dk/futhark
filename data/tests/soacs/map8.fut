-- Test whether the internaliser can properly handle very complex map
-- return types.
-- ==

fun int main([]int a1, []int a2) =
  let b = zipWith(fn ([](int,int),[](int,int)) (int x, int y) =>
                    (zip(iota(x),iota(y)),zip(iota(x),iota(y))),
                  a1, a2) in
  0
