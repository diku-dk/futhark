-- Testing for static stencil index with tuples
-- ==
-- input {
--  [1u64,2u64,3u64,4u64,5u64]
--  [51i32,52i32,53i32,54i32,55i32]
--  [27u8,28u8,29u8,30u8,31u8]
-- }
-- output { [238u64, 239u64, 233u64, 251u64, 250u64] }

let xor_add (xs: [](u64,i32,u8)) =
  let f () xss =
    let (xs, ys, zs) = unzip3 xss
    let rx : u64 = ((((xs[0] + xs[1]) ^ xs[2]) + xs[3]) ^ xs[4])
    let ry : i32 = ((((ys[0] + ys[1]) ^ ys[2]) + ys[3]) ^ ys[4])
    let rz : u8 = ((((zs[0] + zs[1]) ^ zs[2]) + zs[3]) ^ zs[4])
    in (rx^(u64.i32 ry),rz)
  in stencil_1d ([-2,-1,0,1,2]) f (map (const ()) xs) xs

let main [n] (xs : [n]u64) (ys : [n]i32) (zs : [n]u8) =
  let summed = xor_add (zip3 xs ys zs)
   in map (\(fl, bt) -> fl ^ u64.u8 bt) summed
