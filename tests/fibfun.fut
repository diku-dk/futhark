-- ==
-- input {
--   10
-- }
-- output {
--    [ 0 , 1 , 1 , 2 , 3 , 5 , 8 , 13 , 21 , 34  ]
-- }

def computefibs [n] (arr: *[n]i32) : *[n]i32 =
  let arr[0] = 0
  let arr[1] = 1
  in loop (arr) for i < n - 2 do
       let x = arr[i]
       let y = arr[i + 1]
       let arr[i + 2] = x + y
       in arr

def fibs (arr: []i32, n: i32) : *[][]i32 =
  map (\_ -> computefibs (copy (arr))) (0..1..<n)

-- Read an integer from the user, then compute that number of fibonacci numbers.
def main (n: i32) : []i32 =
  let res = fibs (0..1..<n, n)
  in res[0]
