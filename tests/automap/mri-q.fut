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
       : ([numX]f32, [numX]f32) =
  let phiMag = phiR * phiR + phiI * phiI
  let expArgs = map3 (\x_e y_e z_e ->
                          2.0*f32.pi*(kx*x_e + ky*y_e + kz*z_e))
                     x y z
  let qr = f32.sum (f32.cos expArgs * phiMag)
  let qi = f32.sum (f32.sin expArgs * phiMag)
  in (qr, qi)

entry main [numK][numX]
         (kx: [numK]f32) (ky: [numK]f32) (kz: [numK]f32)
         (x: [numX]f32) (y: [numX]f32) (z: [numX]f32)
         (phiR: [numK]f32) (phiI: [numK]f32) =
  let (qr, qi) = main_orig kx ky kz  x y z phiR phiI
  let (qr_am, qi_am) = main_am kx ky kz  x y z phiR phiI
  in and (map2 (==) qr qr_am && map2 (==) qi qi_am)
