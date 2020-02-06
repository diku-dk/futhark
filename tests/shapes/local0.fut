-- A location function with some shape stuff.  Crashed the
-- defunctionaliser once.

let getneighbors (_: i32): []f64 = []

let main (x: i32) =
  let objxy = getneighbors x

  let flikelihood (_: i32) : []i32 =
    let ind = map t64 objxy
    in ind

  let res = flikelihood x

  in res
