-- This failed at one point during type-checking because the k was not
-- visible in the map return type.

def main [n] [m] [k] (a: [n][m][k]i32) : [n][k]i32 =
  let acc_expanded = replicate n (replicate k 0)
  in loop (acc_expanded) for i < m do
       map2 (\(acc: [k]i32) (a_r: [m][k]i32) : [k]i32 ->
               map2 (+) acc (a_r[i]))
            (acc_expanded)
            a
