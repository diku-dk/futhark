-- Testing static index input on a non-associative non-commutative function
-- ==
--
-- input  { [-3f32,3f32,6f32,8f32,10f32] }
-- output { [2.0f32,3.5f32,-2.25f32,-5.2f32,-7.0f32] }
-- compiled random input { [257]f32 } auto output

let main (ys :[]f32) =
  let f _ xs = xs[1] / xs[2] - xs[0]
  in stencil_1d [-1,0,1] f (map (const ()) ys) ys
