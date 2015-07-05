// More distribution with maps consuming their input.
//
// --
//
// structure distributed { Map/Loop 0 }

fun [[[int,k],m],n] main(*[[[int,k],m],n] a) =
  map(fn [[int,k],m] (*[[int,k],m] a_r) =>
        loop(a_r) = for i < m do
          map(fn *[int,k] (*[int,k] a_r_r) =>
                loop(a_r_r) = for i < k-2 do
                  let a_r_r[i+2] =
                    a_r_r[i+2] + a_r_r[i] - a_r_r[i+1] in
                  a_r_r in
                a_r_r,
              a_r) in
        a_r
     , a)
