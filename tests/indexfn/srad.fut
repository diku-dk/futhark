def indexN
    (rows: {i64 |\v -> Range v (1,inf)})
    (i: {i64 | \v -> Range v (0, rows+1)})
    : {i64 | \v -> Range v (0, rows)} =
  if i == 0 then i else i - 1

def indexS
    (rows: {i64 |\v -> Range v (1,inf)})
    (i: {i64 | \v -> Range v (0, rows)})
    : {i64 | \v -> Range v (0, rows)} =
  if i == rows - 1 then i else i + 1

def indexW
    (cols: {i64 |\v -> Range v (1,inf)})
    (j: {i64 | \v -> Range v (0, cols)})
    : {i64 | \v -> Range v (0, cols)} =
  if j == 0 then j else j - 1

def indexE
    (cols: {i64 |\v -> Range v (1,inf)})
    (j: {i64 | \v -> Range v (0, cols)})
    : {i64 | \v -> Range v (0, cols)} =
  if j == cols - 1 then j else j + 1

def calc_stats [rows] [cols]
    (image: [rows][cols]f32)
    (neROI: i64)
    : {f32 | \_ -> true} =
  let flat_image = flatten image
  let sum = f32.sum flat_image
  let flat_sq = map (\x -> x*x) flat_image
  let sum2 = f32.sum flat_sq
  -- get mean (average) value of element in ROI
  let neROI_f32 = f32.i64 neROI
  let meanROI = sum / neROI_f32
  -- gets variance of ROI
  let mean_sq = meanROI * meanROI
  let varROI = (sum2 / neROI_f32) - mean_sq
  -- gets standard deviation of ROI
  let q0sqr = varROI / mean_sq
  in q0sqr

def scale_up [rows] [cols]
    (image: [rows][cols]f32)
    : [rows][cols]f32 =
  map (\row ->
         map (\pixel ->
                -- Take logarithm of image (log compress).
                let log_p = f32.log pixel
                in log_p * 255)
             row)
      image

def calc_c
    (dN_k: f32)
    (dS_k: f32)
    (dW_k: f32)
    (dE_k: f32)
    (jc: f32)
    (q0sqr: f32)
    : {f32 | \_ -> true} =
  let g2 =
    (dN_k * dN_k + dS_k * dS_k
     + dW_k * dW_k
     + dE_k * dE_k)
    / (jc * jc)
  let l = (dN_k + dS_k + dW_k + dE_k) / jc
  let num = (0.5 * g2) - ((1.0 / 16.0) * (l * l))
  let den = 1.0 + 0.25 * l
  let qsqr = num / (den * den)
  let den = (qsqr - q0sqr) / (q0sqr * (1.0 + q0sqr))
  let c_k = 1.0 / (1.0 + den)
  let c_k =
    if c_k < 0.0
    then 0.0
    else if c_k > 1.0
    then 1.0
    else c_k
  in c_k

def calc_d
    (cN: f32)
    (dN_k: f32)
    (cS: f32)
    (dS_k: f32)
    (cW: f32)
    (dW_k: f32)
    (cE: f32)
    (dE_k: f32)
    : {f32 | \_ -> true} =
  -- The identitity function is a little trick to make the index function
  -- for this uninterpreted.
  (\x -> x) (cN * dN_k + cS * dS_k + cW * dW_k + cE * dE_k)


def srad_iter [rows] [cols]
    (image: [rows][cols]f32)
    (lambda: f32)
    (neROI: i64)
    : {[rows][cols]f32 | \_ -> true} =
  -- ROI statistics for entire ROI (single number for ROI)
  let q0sqr = calc_stats image neROI
  let (dN, dS, dW, dE, c) =
    unzip5 (map2 (\i row ->
                    unzip5 (map2 (\j jc ->
                                    -- ANF calls to indexers
                                    let idx_n = indexN rows i
                                    let idx_s = indexS rows i
                                    let idx_w = indexW cols j
                                    let idx_e = indexE cols j
                                    let dN_k = image[idx_n, j] - jc
                                    let dS_k = image[idx_s, j] - jc
                                    let dW_k = image[i, idx_w] - jc
                                    let dE_k = image[i, idx_e] - jc
                                    let c_k = calc_c dN_k dS_k dW_k dE_k jc q0sqr
                                    in (dN_k, dS_k, dW_k, dE_k, c_k))
                                  (iota cols)
                                  row))
                  (iota rows)
                  image)
  let image =
    map4 (\i image_row c_row (dN_row, dS_row, dW_row, dE_row) ->
            map4 (\j pixel c_k (dN_k, dS_k, dW_k, dE_k) ->
                    let idx_s = indexS rows i
                    let idx_e = indexE cols j
                    let cS = c[idx_s, j]
                    let cE = c[i, idx_e]
                    let cN = c_k
                    let cW = c_k
                    let d = calc_d cN dN_k cS dS_k cW dW_k cE dE_k
                    in pixel + lambda / 4 * d)
                  (iota cols)
                  image_row
                  c_row
                  (zip4 dN_row dS_row dW_row dE_row))
          (iota rows)
          image
          c
          (zip4 dN dS dW dE)
  in image

def do_srad [rows] [cols]
    (niter: i64)
    (lambda: f32)
    (image: [rows][cols]u8)
    : [rows][cols]f32 =
  let r1 = 0
  let r2 = rows - 1
  let c1 = 0
  let c2 = cols - 1
  -- ROI image size
  let neROI = (r2 - r1 + 1) * (c2 - c1 + 1)
  -- SCALE IMAGE DOWN FROM 0-255 TO 0-1 AND EXTRACT
  let image = map (\row -> map1 (\pixel -> f32.exp (f32.u8 (pixel) / 255)) row) image
  let image =
    loop image for _i < niter do
      srad_iter image lambda neROI
  -- SCALE IMAGE UP FROM 0-1 TO 0-255 AND COMPRESS
  let image = scale_up image
  in image

def main [rows] [cols]
    (image: [rows][cols]u8)
    : {[rows][cols]f32 | \_ -> true} =
  let niter = 100
  let lambda = 0.5
  in do_srad niter lambda image
