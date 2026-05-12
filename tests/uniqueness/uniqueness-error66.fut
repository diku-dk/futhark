-- From #2300
-- ==
-- error: "a".*consumed

entry sort [n] (array: [n]i32) : []i32 =
  let a =
    loop a = copy array
    for i in 1..<n do
      let ai = array[i]
      let (a2, j) =
        loop (a2, j) = (a, i)
        while j > 0 && a[j - 1] > ai do
          (a2 with [j] = a2[j - 1], j - 1)
      in a2 with [j] = ai
  in a
