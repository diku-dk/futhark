-- Like loop16, but the loop condition depends on the size of A_bak.
-- A_bak's new size is A's old size, so in the internalised condition
-- body the shape parameters must be rebound simultaneously.
-- Rebinding them one at a time overwrites the size of A before it is
-- copied into the size of A_bak, so the loop stopped one iteration
-- early and returned [4,5].
-- ==
-- input { [1.0f32,2.0f32,3.0f32] [4.0f32,5.0f32,6.0f32] }
-- output { [1.0f32] }

def f [n] (A: *[n]f32) (A_bak: *[n]f32) : []f32 =
  let (A, _) =
    loop (A, A_bak) while length A_bak > 2 do
      let m = length A - 1
      let A' = A_bak[:m]
      in (A', A)
  in A

entry main = f
