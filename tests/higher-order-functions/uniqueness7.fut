-- ==
-- input { 1 } output { [0,1,1] }

def zero (xs: *[]i32) (i: i32) =
  xs with [i] = 0

def uniq (x: i32) : *[]i32 = [x, x, x]

def main (x: i32) =
  let f = zero (uniq x)
  in f 0
