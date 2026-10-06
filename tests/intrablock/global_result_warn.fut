-- The intrablock_result(global) attribute warns when it cannot be
-- applied, so that the fallback is not silent.  This is decided during
-- GPU code generation, so the test is only meaningful on the GPU
-- backends.
-- ==
-- tags { no_c no_multicore no_ispc no_python no_wasm }
-- warning: intrablock_result.*no effect

entry main [m] [p] (a: [m][p]f32) =
  #[flattening(only_intra)]
  #[intrablock_result(global)]
  map (\row -> map (\x -> replicate 16 x) row) a
