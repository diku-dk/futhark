-- Distribution should not choke on a map that consumes its input.

fun [[int]] main(*[[int,n]] a, [int,n] is, [int,n] js) =
  map(fn [int] (*[int] a_r) =>
        let double = map(*2, a_r) in
        let triple = map(*3, a_r) in
        loop (a_r) = for i < n do
          let a_r[i] = unsafe double[is[i]] * unsafe triple[js[i]] in
          a_r in
        a_r
     , a)
