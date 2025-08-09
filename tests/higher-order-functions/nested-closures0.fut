-- A local function with a free variable that itself is a function that closes
-- over its own local variables, i.e., a closure inside a closure environment.
-- ==
-- input { 12 } output { 17 }

def main (x: i32) =
  let f =
    let b = 2
    let g =
      let a = 1
      let h = \(x: i32) -> x + a
      in \(z: i32) -> h b + z
    in \(y: i32) -> g y + b
  in f x
