fun bool isSorted ([int] xs) =
    if size(0,xs) < 2 then True
    else let bs = map (fn bool (int i) => xs[i] <= xs[i+1], iota( size(0,xs) - 1) ) in
             reduce(op&&, True, bs)

fun [int] quicksort ([int] xs) =
    if isSorted(xs)
    then xs
    else let pivot = xs[0] in
         let lt = filter( fn bool (int x) => x < pivot , xs ) in
         let eq = filter( fn bool (int x) => x == pivot, xs ) in
         let gt = filter( fn bool (int x) => pivot < x , xs ) in
         let {lt', eq', gt'} = {quicksort(lt), quicksort(eq), quicksort(gt)} in
             concat(lt', concat(eq, gt'))

// fun [[int]] quicksort^ ([[int]] xss)

// but isn't that going to generate a lot of assertions checking that
// input arrays must be regular ?

// Should it be this instead ?
// fun [ { SegDescp , [int] } ] quicksort^ ( [ { SegDescp , [int] } ] xs )

// But won't arrays of tuples be turned into tuple of arrays, meaning it doesn't matter?

// This is how the lifted version of the old code could look like
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
