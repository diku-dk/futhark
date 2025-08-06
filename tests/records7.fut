-- Even a large tuple works as a record.
--
-- ==
-- input { 0 } output { 5 }

def main (x: i32) =
  let (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
    { 0 = x + 1
    , 1 = x + 2
    , 2 = x + 3
    , 3 = x + 4
    , 4 = x + 5
    , 5 = x + 6
    , 6 = x + 7
    , 7 = x + 8
    , 8 = x + 9
    , 9 = x + 10
    , 10 = x + 11
    , 11 = x + 12
    , 12 = x + 13
    , 13 = x + 14
    }
  in e
