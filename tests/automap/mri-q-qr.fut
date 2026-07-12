def qr [numX][numK] (expArgs : [numX][numK]f32) (phiMag : [numK]f32) : [numX]f32 = 
      f32.sum (f32.cos expArgs * phiMag)
