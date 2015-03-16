fun {[real],[[real]]} main([int] arr) =
    redomap( // why parse error if I write: zipWith(op +) ???
             fn [real] ([real] a, [real] b) =>
                 zipWith(op +, a, b)
           , fn {[real],[real]} ([real] acc, int a) =>
                 let r = map( fn real (int x) => toReal(2*x*a)
                            , iota(3) ) 
                 in  { zipWith(op +, acc, r), r }
           , replicate(3,0.0), arr ) 

fun real main0([int] arr) =
    redomap( op +
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
