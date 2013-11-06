// This test inspired by code often created by
// arrays-of-tuples-to-tuple-of-arrays transformation.

fun {[[real]],[[real]]} main() =
  let n = 10 in
  let arrtup = {replicate(n, [0.0]), replicate(n, [0.0])} in
  loop (outarr = copy(arrtup)) = for i < n  do
    let {a, b}  = outarr in
    let a[i] = [0.0] in
    let b[i] = [0.0] in
    {a, b}
  in outarr
