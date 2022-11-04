def main (xss: [][]i32) vs =
  map (\xs -> map (\v -> copy xs with [0] = v) vs) xss
