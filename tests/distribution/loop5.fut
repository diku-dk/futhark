-- More distribution with maps consuming their input.
--
-- ==
--
-- structure distributed { Map/Loop 0 }

fun main(a: *[n][m][k]int): [n][m][k]int =
  map (\(a_r: *[m][k]int): [m][k]int  ->
        loop(a_r) = for i < m do
          map (\(a_r_r: *[k]int): *[k]int  ->
                loop(a_r_r) = for i < k-2 do
                  let a_r_r[i+2] =
                    a_r_r[i+2] + a_r_r[i] - a_r_r[i+1] in
                  a_r_r in
                a_r_r) (
              a_r) in
        a_r
     ) a
