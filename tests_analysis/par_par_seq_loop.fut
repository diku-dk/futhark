def main [l][n][m] (xsss: [l][n][m]i64) : [l][n]i64 =
  map (\xss -> 
    map (\xs ->
      #[unsafe]
      loop res=xs[0] for i < n do 
        res + xs[i]
    ) xss
  ) xsss

-- === Expected output of analysis:
-- entry_main
--   nest_size_5387

--   segmap_usable_groups_5389

--   tmp_5395
--     xsss_5241 [[σ gtid_5391 | ν | par], [σ gtid_5392 | ν | par], [σ i_5397 | ν | seq ]]