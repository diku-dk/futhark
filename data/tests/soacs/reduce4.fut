// Reduction where the accumulator is an array.
// --
// input { [[1,2],[3,4],[5,6]] }
// output { [9, 12] }
fun [int] main([[int]] as) =
  reduce(fn [int] ([int] acc, [int] r) =>
           zipWith(+, acc, r),
         replicate(size(1,as), 0),
         as)
