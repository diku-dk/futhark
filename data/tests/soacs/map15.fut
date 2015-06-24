// Test that a map not using its parameters can be turned into a
// replicate.
//
// --
// input { 2 [1,2,3] }
// output { [4, 4, 4] }
// structure { Map 0 Replicate 1 }

fun [int] main(int x, [int] a) =
  map(fn int (int y) =>
        x + 2,
      a)
