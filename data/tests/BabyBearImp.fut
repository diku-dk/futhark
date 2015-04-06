// --
// input {
// }
// output {
//   [[2,4,5],[1,5,3],[3,7,1]]
// }

//////////////////////////////////////////////////
// SAC VERSION
//////////////////////////////////////////////////
//inline int[.,.] floydSbs1(int[.,.] D ) [
//    DT = transpose(D);
//    res = with
//        (. <= [i,j] <= .) :
//            min( D[i,j], minval( D[i] + DT[j]));
//        : modarray(D);
//    return( res);
//]



//////////////////////////////////////////////////
// C VERSION
//////////////////////////////////////////////////
//inline int* floydSbs1( int N, int* D ) [
//    do k = 1, N
//      do i = 1, N
//        do j = 1, N
//          D[i,j] = MIN(D[i,j], D[i,k] + D[k,j])
//        enddo
//      enddo
//    enddo

//////////////////////////////////////////////////
// C VERSION
//////////////////////////////////////////////////
//inline int* floydSbs1( int N, int* D ) [
//    do i = 1, N
//      do j = 1, N
//        minrow = 0;
//        do k = 1, N
//          minrow = MIN(minrow, D[i,k] + D[k,j])
//        enddo
//        D[i,j] = MIN(D[i,j], minrow)
//      enddo
//    enddo

fun int MIN(int a, int b) = if(a<b) then a else b

fun [[int]] floydSbsImp(int N, *[[int]] D) =
    let DT = copy(transpose(D)) in
    loop (D) = for i < N do
        loop (D) = for j < N do
            let sumrow = map(+, zip(D[i], DT[j])) in
            let minrow = reduce (MIN, 1200, sumrow)    in
            let minrow = MIN(D[i,j], minrow)        in
            let D[i,j] = minrow in D
        in D
    in D

fun [[int]] main() =
    let arr = [[2,4,5], [1,1000,3], [3,7,1]] in
    floydSbsImp(3, copy(arr))
