// Compute Something.
fun [int] main(int n) =
    let arr = iota(n) in
    loop (arr) = for i < n-2 do
                   let arr[i+2] = arr[i] + arr[i+1]
                   in reshape((n), arr)  // transpose(arr)
    in arr
