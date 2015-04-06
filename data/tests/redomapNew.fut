// --
// input {
//   [1,2,3,4,5]
// }
// output {
//   { [0.0, 30.0, 60.0]
//   , [[[0.0, 0.0, 0.0, 0.0, 0.0],
//       [2.0, 2.0, 2.0, 2.0, 2.0],
//       [4.0, 4.0, 4.0, 4.0, 4.0]],
//      [[0.0, 0.0, 0.0, 0.0, 0.0],
//       [4.0, 4.0, 4.0, 4.0, 4.0],
//       [8.0, 8.0, 8.0, 8.0, 8.0]],
//      [[0.0, 0.0, 0.0, 0.0, 0.0],
//       [6.0, 6.0, 6.0, 6.0, 6.0],
//       [12.0, 12.0, 12.0, 12.0, 12.0]],
//      [[0.0, 0.0, 0.0, 0.0, 0.0],
//       [8.0, 8.0, 8.0, 8.0, 8.0],
//       [16.0, 16.0, 16.0, 16.0, 16.0]],
//      [[0.0, 0.0, 0.0, 0.0, 0.0],
//       [10.0, 10.0, 10.0, 10.0, 10.0],
//       [20.0, 20.0, 20.0, 20.0, 20.0]]] }
//
//
// }
fun {[real],[[[real]]]} main([int] arr) =
    redomap( // why parse error if I write: zipWith(op +) ???
             fn [real] ([real] a, [real] b) =>
                 zipWith(+, a, b)
           , fn {[real],[[real]]} ([real] acc, int a) =>
                 let r = map( fn real (int x) => toReal(2*x*a)
                            , iota(3) )
                 in  { zipWith(+, acc, r), transpose(replicate(5,r)) }
           , replicate(3,0.0), arr )

fun real main0([int] arr) =
    redomap( +
           , fn real (real acc, int a) =>
                 let r = toReal(2*a) in
                 acc+r
           , 0.0, arr )

//fun real main1([int] arr) =
//    let acc = 0.0 in
//    loop (acc) = for i < size(0,arr) do
//        acc + toReal(2*arr[i])
//
//    in acc


//fun {real,[real]} main2([int] arr) =
//    redomap( op +
//           , fn {real,real} (real acc, int a) =>
//                 let r = toReal(2*a) in
//                 { acc+r, r }
//           , 0.0, arr )

////futhark -s -fe --in-place-lowering -a -e --compile-sequential InterestCalib.fut > InterestCalib.c
