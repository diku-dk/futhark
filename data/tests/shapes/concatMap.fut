// Test that shape declarations work even in concatMap.
// --
// input {
//   [4,3,2,1]
//   [8,7,6,5]
// }
// output {
//   [1, 2, 3, 4, 5, 6, 7, 8]
// }
fun [int,!n] reverse([int,n] a) =
  map (fn int (int i) =>
         a[size(0,a)-i-1],
       iota(size(0,a)))

fun [int] main([int] a, [int] b) =
  concatMap(reverse, a, b)
