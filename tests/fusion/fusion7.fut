-- ==
-- structure { Screma 1 }

def main (is: []i32) (xs: []i32) =
  let foo = (map (+ 1) xs)[0]
  let bar = (map (+ 2) xs)[0]
  in (foo, bar)
