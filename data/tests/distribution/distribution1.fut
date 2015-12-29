-- Expected distributed/sequentialised structure:
--
-- map
--   map
--     map
--
-- map
--   map
--     map
-- map
--   map
--     scan
--
-- ==
-- tags { no_opencl }
-- structure distributed { MapKernel 3 }

fun [real] combineVs([real] n_row, [real] vol_row, [real] dr_row) =
    map(+, zip(dr_row, map(*, zip(n_row, vol_row ) )))

fun [[real,num_und],num_dates]
  mkPrices([real,num_und] md_starts, [[real,num_und],num_dates] md_vols,
	   [[real,num_und],num_dates] md_drifts, [[real,num_und],num_dates] noises) =
  let e_rows = map( fn [real] ([real] x) =>
                      map(exp, x)
                  , map(combineVs, zip(noises, md_vols, md_drifts)))
  in  scan( fn [real] ([real] x, [real] y) =>
              map(*, zip(x, y))
          , md_starts, e_rows )

--[num_dates, num_paths]
fun [[[real]]] main([[real]] md_vols,
                  [[real]] md_drifts,
                  [real]  md_starts,
                  [[[real]]] noises_mat) =
  map (fn [[real]] ([[real]] noises) =>
         mkPrices(md_starts, md_vols, md_drifts, noises),
       noises_mat)
