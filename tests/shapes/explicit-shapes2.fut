-- Explicit shape quantification in let-binding.
-- ==
-- input { [1,2,3] } output { 3 }

let main (x: []i32) =
  let [n] _y: [n]i32 = x
  in n
