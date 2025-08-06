-- A location function with some shape stuff.  Crashed the
-- defunctionaliser once.

def getneighbors (_: i32) : []f64 = []

def main (x: i32) =
  let objxy = getneighbors x
  let flikelihood (_: i32): []i64 =
    let ind = map i64.f64 objxy
    in ind
  let res = flikelihood x
  in res
