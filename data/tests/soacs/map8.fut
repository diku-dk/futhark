-- Test whether the internaliser can properly handle very complex map
-- return types.
-- ==

fun main(a1: []int, a2: []int): int =
  let b = zipWith (fn (x: int) (y: int): ([](int,int),[](int,int))  =>
                    (zip (iota(x)) (iota(y)),zip (iota(x)) (iota(y)))) a1 a2 in
  0
