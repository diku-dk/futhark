-- Simplified variant of loop2.fut with lower-rank arrays.
--
-- ==
--
-- structure distributed { Map/Loop 0 }

let main(m: i32, a: [#n][#k]i32): [n][k]i32 =
  map (\(a_r: [#k]i32): [k]i32  ->
        let acc = a_r in
        loop(acc) for i < m do
          map (+) acc (a_r)
     ) a
