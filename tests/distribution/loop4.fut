-- Distribution with maps consuming their input.
--
-- ==
--
-- structure distributed { Map/Loop 0 }

let main(m: i32, a: [n][k]i32): [n][k]i32 =
  map (\(a_r: [k]i32): [k]i32  ->
        let a_r_copy = copy(a_r) in
        loop(acc = a_r_copy) = for i < m do
          let acc' = copy(map (+) acc (a_r))
          let acc'[0] = 0 in
          acc' in
        acc
     ) a
