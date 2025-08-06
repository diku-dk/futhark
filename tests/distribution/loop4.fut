-- Distribution with maps consuming their input.
--
-- ==
--
-- structure gpu { Map/Loop 0 }

def main [n] [k] (m: i32) (a: [n][k]i32) : [n][k]i32 =
  map (\a_r ->
         let a_r_copy = copy (a_r)
         in loop acc = a_r_copy
            for i < m do
              let acc' = copy (map2 (+) acc (a_r))
              let acc'[0] = 0
              in acc')
      a
