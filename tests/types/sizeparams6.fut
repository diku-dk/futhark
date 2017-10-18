-- Arrays of tuples work, too.
-- ==
-- input { 2 3 } output { [3,3,3,3] }

type pairvec [m] = [m](i32,i32)

let main (n:i32) (e: i32): []i32 =
  let a: pairvec [] = replicate (2*n) (e,e)
  in #1 (unzip a)
