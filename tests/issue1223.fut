-- ==
-- tags { no_opencl no_cuda no_hip no_pyopencl }
-- structure gpu { SegMap 3 }

def foo [h] [w] (seam_energy: [h][w]i64) : [h]i64 =
  loop res = replicate h 0
  for i < h do
    res with [i] = i64.min (w - 1) seam_energy[i, w - 1]

def remove_minimum_seam [h] [w] (image: [h][w]u32) : [h][]u32 =
  let minimum_seam =
    foo <| map (map i64.u32) image
  in map2 (\row seam_idx -> sized (w - 1) (row[:seam_idx] ++ row[seam_idx + 1:]))
          image
          minimum_seam

def helper [h] [w] (n: i64) (image: [h][w]u32) : [h][w]u32 =
  loop image = copy image
  for i < n do
    let w' = n - i
    in image with [:, :n - i - 1] = remove_minimum_seam <| copy image[:, :w']

def main [m] [h] [w] (n: i64) (images: *[m][h][w]u32) : [m][h][]u32 =
  let w' = w - n
  let res =
    #[incremental_flattening(only_intra)]
    map (helper n) images
  in res[:, :, :w']
