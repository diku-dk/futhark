-- Segmented intra-block reduction of a virtualised index space, where the
-- segment size does not divide the thread block size.  The chunks handled by
-- the virtualisation then straddle segment boundaries.
-- ==
-- random input { [2][50][50]i32 } auto output
-- compiled random input { [2][300][7]i32 } auto output
-- compiled random input { [1][7][300]i32 } auto output
-- structure gpu { SegMap/SegRed 1 SegMap 1 SegRed 1 }

def main xsss =
  #[flattening(only_intra)]
  map (\xss -> #[intrablock_virtualise] map i32.sum xss) xsss
