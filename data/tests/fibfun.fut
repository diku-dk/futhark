-- ==
-- input {
--   10
-- }
-- output {
--    [ 0 , 1 , 1 , 2 , 3 , 5 , 8 , 13 , 21 , 34  ]
-- }
fun *[]int computefibs(*[n]int arr) =
    let arr[0] = 0 in
    let arr[1] = 1 in
    loop (arr) = for i < n-2 do
                   let x = arr[i] in
                   let y = arr[i+1] in
                   let arr[i+2] = x + y
                   in arr
    in arr

fun *[][]int fibs([]int arr, int n) =
    map(fn *[]int (int i) => computefibs(copy(arr)), iota(n))

-- Read an integer from the user, then compute that number of fibonacci numbers.
fun []int main(int n) =
    let res = fibs(iota(n), n) in
    res[0]
