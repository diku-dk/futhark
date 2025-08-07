-- A redomap using a real non-commutative reduction
--
-- Some of the other examples uses `+`, and make sure the compiler doesn't
-- realize it is actually commutative

def foo (x1: i32, x2: i32) (y1: i32, y2: i32) : (i32, i32) =
  if x1 > 0
  then (x1, x2)
  else (y1, x2 + y2)

def main [m] [n] (xss: [m][n]i32) : ([m]i32, [m]i32) =
  unzip (map (\xs ->
                let ys = map (\x -> (x, x)) xs
                in reduce_comm foo (0, 0) ys)
             xss)
