-- Will compute the sum of the inner arrays, and sum these together. We do
-- this for a 3D dimensional array, so end up with an array of results

def main [l] [m] [n] (xsss: [l][m][n]i32) : [l]i32 =
  map (\(xss: [m][n]i32) : i32 ->
         let xss_sums: []i32 = map (\xs -> reduce (+) 0 xs) xss
         in reduce (+) 0 xss_sums)
      xsss
