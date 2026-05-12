-- ==
def main (arr: *[]f64) : f64 =
  let x = map (+ 1.0) arr
  let arr[1] = 3.33
  let y = map (* 2.0) x
  in y[0] + arr[1]
