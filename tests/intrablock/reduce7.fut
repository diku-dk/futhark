-- Intra-block reduction with two reduction operators at once, over an index
-- space that must be virtualised.  Each operator must then be handed its own
-- carry when the result of one chunk is combined with the next.
-- ==
-- entry: single_segment
-- random input { [2][500]i32 } auto output
-- compiled random input { [2][1000]i32 } auto output
-- compiled random input { [2][257]i32 } auto output

-- ==
-- entry: segmented
-- random input { [2][50][50]i32 } auto output
-- compiled random input { [2][300][7]i32 } auto output
-- compiled random input { [1][7][300]i32 } auto output

def twoOps [n] (xs: [n]i32) =
  i32.sum xs * 31 + i32.maximum xs

entry single_segment xss =
  #[flattening(only_intra)]
  map twoOps xss

entry segmented xsss =
  #[flattening(only_intra)]
  map (\xss -> map twoOps xss) xsss
