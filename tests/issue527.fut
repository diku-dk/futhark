-- ==
-- input { 2 } output { 2 }

def id 'a (x: a) : a = x

def main (x: i32) =
  let r = {id}
  in r.id x
