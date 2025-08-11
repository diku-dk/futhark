-- An array type alias can be unique.

type matrix [n] [m] = [n][m]i32

def main (m: *matrix [] []) : matrix [] [] =
  let m[0, 0] = 0 in m
