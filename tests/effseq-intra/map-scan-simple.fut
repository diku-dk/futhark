let fun [n] [m] (a : [n][m]i32) =
  #[incremental_flattening(only_intra)]
  #[seq_factor(8)]
  map (\ a_row ->
     scan (+) 0 a_row
  ) a


-- ==
-- entry: test1
-- compiled random input {[100000][1024]i32} auto output
-- compiled random input {[200000][1024]i32} auto output
-- compiled random input {[300000][1024]i32} auto output
-- compiled random input {[400000][1024]i32} auto output
-- compiled random input {[500000][1024]i32} auto output
-- compiled random input {[600000][1024]i32} auto output
entry test1 [n] [m] (a: [n][m]i32) = fun a

-- ==
-- entry: test2
-- compiled random input { [65536][2048]i32     }
-- compiled random input { [131072][1024]i32    }
-- compiled random input { [262144][512]i32     }
-- compiled random input { [524288][256]i32     }
-- compiled random input { [1048576][128]i32    }
entry test2 [n] [m] (a: [n][m]i32) = fun a
