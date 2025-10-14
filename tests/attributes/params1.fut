-- ==
-- input { 10 } output { 20 }

def main (x: i32) =
  loop (#[maxval(1337)] acc: i32) = x for i < 10 do x + x
