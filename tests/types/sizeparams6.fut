-- Arrays of tuples work, too.
-- ==
-- input { 2i64 3 } output { [3,3,3,3] }

type pairvec [m] = [m](i32, i32)

def main (n: i64) (e: i32) : []i32 =
  let a: pairvec [] = replicate (2 * n) (e, e)
  in (unzip a).0
