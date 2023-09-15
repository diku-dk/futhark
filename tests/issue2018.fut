def main (i: i64) (j: i64) (xss: *[][]i32) =
  let xs = xss[i]
  let xss[j] = copy (opaque (opaque xs))
  in xss
