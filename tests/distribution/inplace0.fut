-- Distribution should not choke on a map that consumes its input.

let main [n] (a: *[][n]i32, is: [n]i32, js: [n]i32): [][]i32 =
  map (\(a_r: *[]i32): []i32  ->
        let double = map (*2) (a_r)
        let triple = map (*3) (a_r) in
        loop (a_r) for i < n do
          let a_r[i] = unsafe double[is[i]] * unsafe triple[js[i]] in
          a_r
     ) a
