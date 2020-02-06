-- ==
-- tags { no_python }
-- input { 100 1000 } output { 870104 }
-- compiled input { 400 1000} output { 985824 }
-- compiled input { 100000 100} output { 15799424 }
--
let main (n: i32) (m: i32): i32 =
  let a = map (\(i: i32): [m]i32  ->
                 map (+i) (iota(m)))
              (iota(n))
  let b = map  (\(a_r: [m]i32): [m]i32  ->
                 scan (+) 0 (a_r)) a in
  reduce (^) 0 (flatten b)
