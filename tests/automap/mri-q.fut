-- ==
-- entry: main
-- random input { [12]f32 [12]f32 [12]f32 [10]f32 [10]f32 [10]f32 [12]f32 [12]f32 }
-- output { true }

def main_orig [numK][numX]
         (kx: [numK]f32) (ky: [numK]f32) (kz: [numK]f32)
         (x: [numX]f32) (y: [numX]f32) (z: [numX]f32)
         (phiR: [numK]f32) (phiI: [numK]f32)
       : ([numX]f32, [numX]f32) =
  let phiMag = map2 (\r i -> r*r + i*i) phiR phiI
  let expArgs = map3 (\x_e y_e z_e ->
                          map (2.0f32*f32.pi*)
                              (map3 (\kx_e ky_e kz_e ->
                                        kx_e * x_e + ky_e * y_e + kz_e * z_e)
                                    kx ky kz))
                     x y z
  let qr = map1 (map f32.cos >-> map2 (*) phiMag >-> f32.sum) expArgs
  let qi = map1 (map f32.sin >-> map2 (*) phiMag >-> f32.sum) expArgs
  in (qr, qi)
     
def main_am [numK][numX]
         (kx: [numK]f32) (ky: [numK]f32) (kz: [numK]f32)
         (x: [numX]f32) (y: [numX]f32) (z: [numX]f32)
         (phiR: [numK]f32) (phiI: [numK]f32)
       : ([numK]f32, [numX][numK]f32) =
  let (phiMag : [numK]f32) = phiR * phiR + phiI * phiI
  let (expArgs : [numX][numK]f32) = map3 (\(x_e : f32) (y_e : f32) (z_e : f32) ->
                          2.0*f32.pi*(kx*x_e + ky*y_e + kz*z_e))
                     x y z
  in (phiMag, expArgs)
  --let (qr : [numX]f32) = f32.sum (f32.cos expArgs * phiMag) -- [numx]f32
  --let (qi : [numX]f32) = f32.sum (f32.sin expArgs * phiMag) -- let (qi_10408: artificial₁₁₄_10524 ~ [M113_10523]f32) 
  --in (qr, qi)
     
--entry main [numK][numX]
--         (kx: [numK]f32) (ky: [numK]f32) (kz: [numK]f32)
--         (x: [numX]f32) (y: [numX]f32) (z: [numX]f32)
--         (phiR: [numK]f32) (phiI: [numK]f32) =
--  main_orig kx ky kz  x y z phiR phiI == main_am kx ky kz  x y z phiR phiI 
