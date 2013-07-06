////////////////////////////////////////////
//subroutine tridag(a,b,c,d,nn)
//
//    dimension a(nn),b(nn),c(nn),d(nn)
//
//    if(nn .eq. 1) then
//        d(1)=d(1)/b(1)
//        return
//    end if
//
//    do 10 k = 2,nn
//        xm = a(k)/b(k-1)
//        b(k) = b(k) - xm*c(k-1)
//        d(k) = d(k) - xm*d(k-1)
//10  continue
//
//    d(nn) = d(nn)/b(nn)
//    k = nn
//    do 20 i = 2,nn
//        k = nn + 1 - i
//        d(k) = (d(k) - c(k)*d(k+1))/b(k)
//20  continue
//    return
//end
/////////////////////////////////////////////


fun {[real],[real]} tridag(  int  nn,
                            *[real] b, *[real] d,
                            [real] a, [real] c ) =
    if (nn = 1)
    //then ( b, zipWith(fn real (real x, real y) => x / y, d, b) )
    then {b, [d[0]/b[0]]}
    else
        loop({b, d}) = for i < (nn-1) do
            let xm     = a[i+1] / b[i]    in
            let b[i+1] = b[i+1] - xm*c[i] in
            let d[i+1] = d[i+1] - xm*d[i] in
            {b, d}
        in
        let d[nn-1] = d[nn-1] / b[nn-1]   in

        loop(d)    = for i < (nn-1) do
            let k = nn - 2 - i                       in
            let d[k] = ( d[k] - c[k]*d[k+1] ) / b[k] in
            d
        in
            {b, d}


fun {[real],[real]} main() =
    let nn = reduce(op +, 0, [1,2,3,4]) in
    let a = replicate(nn, 3.33) in
    let b = map(fn real (int x) => toReal(x) + 1.0, iota(nn)) in
    let c = map(fn real (int x) => 1.11*toReal(x) + 0.5, iota(nn)) in
    let d = map(fn real (int x) => 1.01*toReal(x) + 0.25, iota(nn)) in
        tridag(nn, b, d, a, c)
