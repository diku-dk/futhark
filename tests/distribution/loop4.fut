-- Distribution with maps consuming their input.
--
-- ==
--
-- structure distributed { Map/Loop 0 }

import "/futlib/array"

let main [n][k] (m: i32, a: [n][k]i32): [n][k]i32 =
  map (\[k] (a_r: [k]i32): [k]i32  ->
        let a_r_copy = copy(a_r) in
        loop acc = a_r_copy for i < m do
          let acc' = copy(map2 (+) acc (a_r))
          let acc'[0] = 0 in
          acc'
     ) a
