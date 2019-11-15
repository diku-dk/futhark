-- Simple rank-1 two-dimensional stencil computation.  Eventually
-- smooths out all differences.
-- ==
-- input {
-- 0
-- [[1.0,2.0,3.0],
--  [4.0,5.0,6.0],
--  [7.0,8.0,9.0]]
-- }
-- output {
-- [[1.0,2.0,3.0],
--  [4.0,5.0,6.0],
--  [7.0,8.0,9.0]]
-- }
-- input {
-- 1
-- [[1.0,2.0,3.0],
--  [4.0,5.0,6.0],
--  [7.0,8.0,9.0]]
-- }
-- output {
-- [[1.8, 2.6000000000000005, 3.4000000000000004],
-- [4.2, 5.0, 5.800000000000001],
-- [6.6000000000000005, 7.4, 8.2]]
-- }
-- input {
-- 2
-- [[1.0,2.0,3.0],
--  [4.0,5.0,6.0],
--  [7.0,8.0,9.0]]
-- }
-- output {
-- [[2.44, 3.0800000000000005, 3.7200000000000006],
--  [4.36, 5.0, 5.640000000000001],
--  [6.280000000000001, 6.920000000000001, 7.56]]
-- }

let main [n][m] (num_iterations: i32) (a: [n][m]f64): [][]f64 =
  loop (a) for i < num_iterations do
    map (\(i: i32) ->
          map (\(j: i32) ->
                let center = unsafe a[i,j]
                let north = if i == 0 then center else unsafe a[i-1,j]
                let east = if j == m-1 then center else unsafe a[i,j+1]
                let south = if i == n-1 then center else unsafe a[i+1,j]
                let west = if j == 0 then center else unsafe a[i,j-1]
                let factor = 1.0/5.0 in
                factor*center +
                factor*north +
                factor*east +
                factor*south +
                factor*west
             ) (iota(m))
       ) (iota(n))
