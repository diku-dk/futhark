// Compute Something.
fun [[int]] main(int n) =
    let arr = replicate(n, iota(n)) in
    loop (arr) = for i < n-2 do
                   let arr[0,i+2] = arr[0,i] + arr[0,i+1]
                   in reshape((n,n), arr)  // transpose(arr)
    in arr
