import "utils"

let closest_point (p1: (i64,f32)) (p2: (i64,f32)): (i64,f32) =
  if p1.1 < p2.1 then p1 else p2

let initCenters [nnz][np1]
                (k: i64) (columns: i64)
                (pointers: [np1]i64) (row_indices: [nnz]i64)
                (values: [nnz]f32) (indices_data: [nnz]i64) :
                [k][columns]f32 =

  -- initialize the cluster centers (in dense rep) with the first k (sparse) elements
  let first_k_total_nz = pointers[k+1] -- reduce (+) 0 (take k vector_nnz)
  let dense_k_inds = map2 (\row col -> row*columns + col)
                          (take first_k_total_nz row_indices)
                          (take first_k_total_nz indices_data)

  let cluster_centers_flat =
        scatter (replicate (k*columns) 0)
                dense_k_inds
                (take first_k_total_nz values)

  let cluster_centers =
        unflatten cluster_centers_flat

  in  cluster_centers

let kmeans_seq_rows [nnz][np1]
        (_fix_iter: bool) (threshold: f32) (num_iterations: i64) (k: i64)
        (values: [nnz]f32) (indices_data: [nnz]i64) 
        (pointers: [np1]i64) =

    let n = np1 - 1

    let columns = 1 + reduce (i64.max) (-1) indices_data
    
    let shape = map (\i -> pointers[i+1] - pointers[i]) (iota n) -- this is shape
    let flags = mkFlagArray shape false (replicate n true)  :> [nnz]bool
    let row_indices =
         map2 (\f i -> if f && (i!=0) then 1i64 else 0i64) flags (indices flags)
      |> scan (+) 0i64

    let cluster_centers =
      initCenters k columns pointers row_indices values indices_data

    let (_new_membership,cluster_centers,delta,i) =
        -- ALWAYS EXECUTE num_iterations
        loop (membership, cluster_centers, _delta, i) =
             (map (%k) (iota n), cluster_centers, threshold + 1, 0)
        while i < num_iterations do
--        while (if fix_iter then true else delta > threshold) && i < num_iterations do
            -- For each point, find the cluster with the closest centroid.
            -- prepare sum of squares because we'll only have slight deviationsssss
            -- we assume that most differences are 0 and correct the mistakes as we go

            let cluster_squared_sum = map (\c -> f32.sum (map (\x -> x*x) c)) cluster_centers
            -- assume that every row has a thing thing: uses marked with *

            let diffs =
              map (\row -> 
                    map (\cluster->
                          let index_start = pointers[row]
                          let nnz = pointers[row+1] - index_start 
                          let j = 0
                          let correction = 0  
                          let (correction, _) = 
                            loop (correction, j) while j < nnz do
                                let element_value = values[index_start+j]
                                let column = indices_data[index_start+j]
                                let cluster_value = cluster[column]
                                let value = (element_value - 2 * cluster_value)*element_value
                                in (correction+value, j+1)
                          in correction
                        ) cluster_centers
                  ) (iota n)
            -- let diffs = map (\cluster -> map f32.i64 (iota k)) (iota n)
            
            let new_membership =
              map (\diff ->
                      let (i, _) =
                        foldl (\acc (i, diff) ->
                                -- is it better to add the square as a map before?
                                closest_point acc (i, cluster_squared_sum[i] + diff)
                              )
                              (0, f32.inf)
                              (zip (iota k) diff)
                      in i
                  ) diffs
            
            -- Then, find the new centres of the clusters.
            let new_centers =
                  reduce_by_index
                      (replicate (k*columns) 0) (+) 0
                      (map2 (\row col -> columns*new_membership[row] + col) row_indices indices_data)
                      values


            let center_counts =
                  reduce_by_index
                      (replicate k 0) (+) 0
                      new_membership
                      (replicate n 1)

            let new_centers = unflatten new_centers

            -- need to divide by number of elements 
            let new_centers = 
                  map2(\center count -> 
                          map (\x -> x / (f32.i64 (if count == 0 then 1 else count)))
                              center
                      ) new_centers center_counts
        
            let new_delta =
                  map2 (==) membership new_membership |>
                  map (\b -> if b then 0 else 1) |>
                  i64.sum
            let new_delta = (f32.i64 new_delta) / (f32.i64 n)
                  
            in (new_membership, new_centers, new_delta, i+1)

    in (delta, i, cluster_centers)
