def main [l][n][m] (xsss: [l][n][m]i64) : [l][m]i64 =
  map (\xss -> 
    #[unsafe]
    loop _=xss[0] for i < n do
      map (\x -> x*2) xss[i]
  ) xsss

-- === Expected output of analysis:
-- entry_main
--   xsss_transformed_5310 [[σ xsss_5223 | ν | par], [τ 0i64 | ν | seq], [σ xsss_5223 | ν | par]]
--   fits_5331 []
--   intra_suff_and_fits_5332 []
--   nest_size_5409 []
--   segmap_usable_groups_5411 []
-- WARNING: This output is DEFINITELY wrong!!