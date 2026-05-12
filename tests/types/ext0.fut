-- ==
-- input { 0i64 }
-- output { [[true,true],[true,true]] }

def main x : ?[n].[n][n]bool =
  let n = x + 2
  in replicate n (replicate n true)
