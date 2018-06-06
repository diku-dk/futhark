-- ==
-- structure { Scatter 1 Screma 1 }

let main [n_indices]
        (scan_num_edges: [n_indices]i32,
         write_inds: [n_indices]i32,
         active_starts: [n_indices]i32) =

  let flat_len       = scan_num_edges[n_indices-1]
  let (tmp1, tmp2, tmp3) = unzip (replicate flat_len (false, 0i32, 1i32))
  let active_flags   = scatter tmp1 write_inds (replicate n_indices true)
  let track_nodes_tmp= scatter tmp2 write_inds (iota n_indices)
  let track_index_tmp= scatter tmp3 write_inds active_starts

  in scan (\(x,a,b) (y,c,d) -> (x || y, a+c,b+d))
      (false,0,0)
      (zip active_flags track_nodes_tmp track_index_tmp)