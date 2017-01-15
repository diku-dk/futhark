-- Distribution should not choke on a map that consumes its input.

fun main(a: *[][n]int, is: [n]int, js: [n]int): [][]int =
  map (\(a_r: *[]int): []int  ->
        let double = map (*2) (a_r)
        let triple = map (*3) (a_r) in
        loop (a_r) = for i < n do
          let a_r[i] = unsafe double[is[i]] * unsafe triple[js[i]] in
          a_r in
        a_r
     ) a
