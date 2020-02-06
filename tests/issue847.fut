-- Tiling bug.

let main (acc: []i32) (c: i32) (n:i32) =
  let is = map (+c) (iota n)
  let fs = map (\i -> reduce (+) 0 (map (+(i+c)) acc)) (iota n)
  in (fs, is)
