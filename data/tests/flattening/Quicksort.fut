fun [int] quicksort ([int] xs) =
    let len = size(0,xs) in
    if len < 2
    then xs
    else let pivot = xs[0] in
         let {arr, ltIndex, eqIndex} = filter( fn bool (int x) => x < pivot
                                             , fn bool (int x) => x == pivot
                                             , xs) in
         let {lt, eq, gt} = split(arr, ltIndex, eqIndex) in
         let {lt', gt'} = tmap(quicksort, {lt,gt}) in
             concat(lt', eq, gt')


// This is how the lifted code could look like
//
// fun [[int]] quicksort^ ([[int]] xss) =
//     let lens = map (fn int ([int] xs) => size(0,xs), xss) in
//     let cnds = map(fn bool (int len) => len < 2, lens) in
//
//     let (xs_t, xs_f) = partition(cnds, xss) in
//
//     let xs_t' = xs_t in
//
//     let xs_f' = let pivots = map(fn int ([int] xs) => xs[0], xss) in
//                 let {arrs, ltIndexs, eqIndexs} = filter^ (<^ pivot, ==^ pivot, xss) in
//                 let {lts, eqs, gts} = split^ (arrs, ltIndexs, eqIndexs) in
//                 let {lts', gts'} = tmap(quicksort^, {lts, gts}) in
//                     concat^(lts', eqs, gts')
//                 in
//
//     merge(cnds, xs_t', xs_f')

fun [[int]] main ([[int]] tosort) =
    map(quicksort, tosort)
