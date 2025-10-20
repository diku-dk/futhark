-- Source: https://raw.githubusercontent.com/diku-dk/futhark-benchmarks/adfc91c77ed2e3d132f079cf58ddd9ec59a24997/rodinia/srad/srad.fut
-- Many of these names are taken directly from srad_v1 in the Rodinia
-- suite.  It looks like some Fortran remnant (sorry, I mean
-- "FORTRAN").
--
-- Some comments are also from original source code.
--
-- One annoying difference is that the original program assumes
-- column-major storage, whereas we use row-major storage here.
--
-- The original program rounds the final result to integers (as is
-- proper for an image).  This creates problems with getting the same
-- results on GPU and CPU, as differences in floating-point accuracy
-- leads to slight changes that just might push the rounded integer up
-- or down, thus exaggerating the difference.  For simplicity, this
-- mplementation returns floats instead.  This should have no
-- measurable impact on performance, as we still perform the scaling
-- to (0,255).
--
-- ==
-- compiled input @ data/image.in.gz
-- output @ data/image.out.gz

def indexN(_rows: i64, i: i64): i64 =
  if i == 0 then i else i - 1

def indexS(rows: i64, i: i64): i64 =
  if i == rows-1 then i else i + 1

def indexW(_cols: i64, j: i64): i64 =
  if j == 0 then j else j - 1

def indexE(cols: i64, j: i64): i64 =
  if j == cols-1 then j else j + 1

def sumf32 [n] (xs: [n]f32): f32 =
  if n > 0 then (scan (\x y -> x + y) 0 xs)[n-1] else 0

def loop_body [rows][cols] (lambda: f32, image: [rows][cols]f32) (neROI: {i64 | \x -> Range x (0,inf) && Range rows (1,inf) && Range cols (1,inf)}): {[rows][cols]f32 | \_ -> true} =
    -- ROI statistics for entire ROI (single number for ROI)
    let flatten_image = flatten image
    let sum = sumf32 flatten_image
    let sum2 = sumf32 (map (\x -> x*x) flatten_image)
    -- get mean (average) value of element in ROI
    let meanROI = sum / f32.i64 neROI
    -- gets variance of ROI
    let varROI = (sum2 / f32.i64 neROI) - meanROI*meanROI
    -- gets standard deviation of ROI
    let q0sqr = varROI / (meanROI*meanROI)

    let (dN, dS, dW, dE, c) =
      unzip5 (map2 (\i row ->
                unzip5 (map2 (\j jc ->
                        -- let i = i32.i64 i_64
                        -- let j = i32.i64 j_64
                        let dN_k = #[unsafe] image[indexN(rows,i),j] - jc
                        let dS_k = #[unsafe] image[indexS(rows,i),j] - jc
                        let dW_k = #[unsafe] image[i, indexW(cols,j)] - jc
                        let dE_k = #[unsafe] image[i, indexE(cols,j)] - jc
                        let g2 = (dN_k*dN_k + dS_k*dS_k +
                                  dW_k*dW_k + dE_k*dE_k) / (jc*jc)
                        let l = (dN_k + dS_k + dW_k + dE_k) / jc
                        let num = (0.5*g2) - ((1.0/16.0)*(l*l))
                        let den = 1.0 + 0.25*l
                        let qsqr = num / (den*den)
                        let den = (qsqr-q0sqr) / (q0sqr * (1.0+q0sqr))
                        let c_k = 1.0 / (1.0+den)
                        let c_k = if c_k < 0.0
                                  then 0.0
                                  else if c_k > 1.0
                                       then 1.0 else c_k
                        in (dN_k, dS_k, dW_k, dE_k, c_k))
                      (iota cols) row))
             (iota rows) image)

    let image =
      map4 (\i image_row c_row (dN_row, dS_row, dW_row, dE_row) ->
                map4 (\j pixel c_k (dN_k, dS_k, dW_k, dE_k)  ->
                          -- let i = i32.i64 i_64
                          -- let j = i32.i64 j_64
                          let cN = c_k
                          let cS = #[unsafe] c[indexS(rows, i), j]
                          let cW = c_k
                          let cE = #[unsafe] c[i, indexE(cols,j)]
                          let d = cN*dN_k + cS*dS_k + cW*dW_k + cE*dE_k
                          in pixel + 0.25 * lambda * d)
                     (iota cols) image_row c_row (zip4 dN_row dS_row dW_row dE_row))
           (iota rows) image c (zip4 dN dS dW dE)
    in image
  
def do_srad [rows][cols] (niter: i32, lambda: f32, image: [rows][cols]u8): {[rows][cols]f32 | \_ -> true} =
  let r1 = 0
  let r2 = rows - 1
  let c1 = 0
  let c2 = cols - 1

  -- ROI image size
  let neROI = (r2-r1+1)*(c2-c1+1)

  -- SCALE IMAGE DOWN FROM 0-255 TO 0-1 AND EXTRACT
  let image = map (\pixels -> map1 (\pixel -> f32.exp(f32.u8(pixel)/255)) pixels) image
  let image = loop image for _i < niter do
    loop_body (lambda, image) neROI

  -- SCALE IMAGE UP FROM 0-1 TO 0-255 AND COMPRESS
  let image = map (map1 (\pixel  ->
                          -- Take logarithm of image (log compress).
                          -- This is where the original implementation
                          -- would round to i32.
                         f32.log(pixel)*255.0)) image
  in image

-- def main [rows][cols] (image: [rows][cols]u8): [rows][cols]f32 =
--   let niter = 100
--   let lambda = 0.5
--   in do_srad(niter, lambda, image)

-- Entry point for interactive demo.  Here we can return an RGBA image.
-- entry srad [rows][cols] (niter: i32) (lambda: f32) (image: [rows][cols]u8): [rows][cols]i32 =
--   map (map1 (\p -> (i32.f32 (p) << 16) | (i32.f32 (p) << 8) | (i32.f32 (p))))
--       (do_srad(niter, lambda, image))
