-- Distribution should not choke on a map that consumes its input.

fun [[int]] main(*[[int]] a, [int] is, [int] js) =
  map(fn [int] (*[int] a_r) =>
        let double = map(*2, a_r) in
        let triple = map(*3, a_r) in
        loop (a_r) = for i < size(0, a_r) do
          let a_r[i] = double[is[i]] * triple[js[i]] in
          a_r in
        a_r
     , a)
