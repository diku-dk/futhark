fun bool testBit(int n, int ind) =
    let t = (1 << ind) in (n & t) == t

fun int xorInds(int bits_num, int n, [int] dir_vs ) =
    let bits    = iota   ( bits_num )                   in
    let indices = filter ( testBit(grayCode(n)), bits ) in
    reduce( op ^, 0, map( index(dir_vs), indices ) )

fun [int] sobolIndI ( int bits_num, [[int]] dir_vs, int n ) =
    map( xorInds(bits_num, n), dir_vs )

fun [real] sobolIndR( int bits_num, [[int]] dir_vs, int n ) =
    let divisor = 2.0 pow toReal (bits_num)        in
    let arri    = sobolIndI( bits_num, dir_vs, n ) in
        map( fn real (int x) => toReal(x) / divisor, arri )

fun real main( int num_mc_it, int num_bits, [[int]] dir_vs ) =
    let sobol_mat = map ( sobolIndR(num_bits, dir_vs), 
                          map(fn int (int x) => x + 1, iota(num_mc_it)) )
    in  sobol_mat

