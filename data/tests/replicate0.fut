// Simple test to see whether we can properly replicate arrays.
fun [[int]] main(int n) =
    let x  = iota(n)     in
    let y  = replicate(n,   x) in
    y
