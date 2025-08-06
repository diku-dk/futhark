-- More distribution with maps consuming their input.
--
-- ==
--
-- structure gpu { Map/Loop 0 }

def main [n] [m] [k] (a: *[n][m][k]i32) : [n][m][k]i32 =
  map (\(a_r: [m][k]i32) : [m][k]i32 ->
         loop (a_r) for i < m do
           map (\(a_r_r: [k]i32) : *[k]i32 ->
                  loop a_r_r = copy a_r_r
                  for i < k - 2 do
                    let a_r_r[i + 2] = a_r_r[i + 2] + a_r_r[i] - a_r_r[i + 1]
                    in a_r_r)
               (a_r))
      a
