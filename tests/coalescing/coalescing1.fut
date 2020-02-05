-- ==
-- input { [[1,2],[3,4],[5,6]] }
-- output { [4i32, 10i32, 16i32] }

let main [n][m] (a: [n][m]i32): []i32 =
  map (\(r: []i32): i32  ->
        loop x = 0 for i < m do
          x * 2 + r[i]) a
