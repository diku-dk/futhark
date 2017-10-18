-- Simplified variant of loop2.fut with lower-rank arrays.
--
-- ==
--
-- structure distributed { Map/Loop 0 }

let main [n][k] (m: i32, a: [n][k]i32): [n][k]i32 =
  map (\[k] (a_r: [k]i32): [k]i32  ->
        let acc = a_r in
        loop(acc) for i < m do
          map (+) acc (a_r)
     ) a
