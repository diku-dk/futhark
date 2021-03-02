-- ==
-- input { [1,2,3] }
-- output { [[1i32, 1i32, 2i32], [1i32, 2i32, 3i32], [2i32, 3i32, 3i32]] }

let main (xs: []i32) =
  let f () xs = xs
  in stencil_1d [-1,0,1] f (map (const ()) xs) xs
