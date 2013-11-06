// This benchmark tests whether aliasing is tracked even deep inside
// loops.

fun [[int]] floydSbsImp(int N, *[[int]] D) =
    let DT = transpose(D) in // DT aliases D.
    loop (D) = for i < N do
        loop (D) = for j < N do
            let D[i,j] = DT[j,i] in D // Consume D and DT, but bad, because DT is used.
        in D
    in D
