-- See issue 1989.
-- ==
-- tags { autodiff }
-- structure { UpdateAcc 3 }

def gather1D 't [m] (arr1D: [m]t) (inds: [m]i32) : *[m]t =
  map (\ind -> arr1D[ind]) inds

def gather2D 't [m] [d] (arr2D: [m][d]t) (inds: [m]i32) : *[m][d]t =
  map (\ind -> map (\j -> arr2D[ind, j]) (iota d)) inds

def sumSqrsSeq [d] (xs: [d]f32) (ys: [d]f32) : f32 =
  loop (res) = (0.0f32)
  for (x, y) in (zip xs ys) do
    let z = x - y in res + z * z

def log2 x = (loop (y, c) = (x, 0i32) while y > 1i32 do (y >> 1, c + 1)).1

def isLeaf (h: i32) (node_index: i32) =
  node_index >= ((1 << (h + 1)) - 1)

def findLeaf [q] [d]
             (median_dims: [q]i32)
             (median_vals: [q]f32)
             (height: i32)
             (query: [d]f32) =
  let leaf =
    loop (node_index) = (0)
    while !(isLeaf height node_index) do
      if query[median_dims[node_index]] <= median_vals[node_index]
      then (node_index + 1) * 2 - 1
      else (node_index + 1) * 2
  in leaf - i32.i64 q

def traverseOnce [q] [d]
                 (radius: f32)
                 (height: i32)
                 (kd_tree: [q](i32, f32, i32))
                 (query: [d]f32)
                 (last_leaf: i32, stack: i32, dist: f32) : (i32, i32, f32) =
  let (median_dims, median_vals, clanc_eqdim) = unzip3 kd_tree
  let last_leaf = last_leaf + i32.i64 q
  let no_leaf = 2 * q + 1
  let getPackedInd (stk: i32) (ind: i32): bool = let b = stk & (1 << ind) in b != 0
  let setPackedInd (stk: i32) (ind: i32) (v: bool) =
    let fst = stk & ((1 << ind) - 1)
    let snd = (stk >> (ind + 1)) << (ind + 1)
    let mid = if v then (1 << ind) else 0
    in ((fst | snd) | mid)
  let getLevel (node_idx: i32): i32 = log2 (node_idx + 1)
  let getAncSameDimContrib (q_m_i: f32) (node_stack: i32) (node: i32): f32 =
    (loop (idx, res) = (node, 0.0f32)
     while (idx >= 0) do
       let anc = clanc_eqdim[idx]
       in if anc == (-1i32)
          then (-1i32, 0.0f32)
          else let anc_lev = getLevel anc
               let is_anc_visited = getPackedInd node_stack anc_lev
               in if !is_anc_visited
                  then (anc, res)
                  else (-1i32, median_vals[anc] - q_m_i)).1
  let (parent_rec, stack, count, dist, rec_node) =
    loop (node_index, stack, count, dist, rec_node) =
           (last_leaf, stack, height, dist, -1)
    for _i2 < height + 1 do
      if (node_index != 0) && (rec_node < 0)
      then let parent = (node_index - 1) / 2
           let scnd_visited = getPackedInd stack count
           --stack[count]

           let q_m_d = query[median_dims[parent]]
           let cur_med_dst = median_vals[parent] - q_m_d
           let cur_med_sqr = cur_med_dst * cur_med_dst
           let prv_med_dst = getAncSameDimContrib q_m_d stack parent
           let prv_med_sqr = prv_med_dst * prv_med_dst
           let dist_minu = f32.abs (dist - cur_med_sqr + prv_med_sqr)
           let dist_plus = f32.abs (dist - prv_med_sqr + cur_med_sqr)
           in if scnd_visited
              then -- continue backing-up towards the root
                   (parent, stack, count - 1, dist_minu, -1)
              else -- the node_index is actually the `first` child of parent,
                   let to_visit = dist_plus <= radius
                   in if !to_visit
                      then (parent, stack, count - 1, dist, -1)
                      else -- update the stack
                           let fst_node = node_index
                           let snd_node = if (fst_node % 2) == 0 then fst_node - 1 else fst_node + 1
                           let stack = setPackedInd stack count true
                           in (parent, stack, count, dist_plus, snd_node)
      else (node_index, stack, count, dist, rec_node)
  let (new_leaf, new_stack, _) =
    if parent_rec == 0 && rec_node == -1
    then -- we are done, we are at the root node
         (i32.i64 no_leaf, stack, 0)
    else -- now traverse downwards by computing `first`
         loop (node_index, stack, count) =
                (rec_node, stack, count)
         for _i3 < height + 1 do
           if isLeaf height node_index
           then (node_index, stack, count)
           else let count = count + 1
                let stack = setPackedInd stack count false
                let node_index =
                  if query[median_dims[node_index]] <= median_vals[node_index]
                  then (node_index + 1) * 2 - 1
                  else (node_index + 1) * 2
                in (node_index, stack, count)
  in (new_leaf - i32.i64 q, new_stack, dist)

def sortQueriesByLeavesRadix [n] (leaves: [n]i32) : ([n]i32, [n]i32) =
  (leaves, map i32.i64 (iota n))

def bruteForce [m] [d]
               (radius: f32)
               (query: [d]f32)
               (query_w: f32)
               (leaf_refs: [m][d]f32)
               (leaf_ws: [m]f32) : f32 =
  map2 (\ref i ->
          let dist = sumSqrsSeq query ref
          in if dist <= radius then query_w * leaf_ws[i] else 0.0f32)
       leaf_refs
       (iota m)
  |> reduce (+) 0.0f32

def iterationSorted [q] [n] [d] [num_leaves] [ppl]
                    (radius: f32)
                    (h: i32)
                    (kd_tree: [q](i32, f32, i32))
                    (leaves: [num_leaves][ppl][d]f32)
                    (ws: [num_leaves][ppl]f32)
                    (queries: [n][d]f32)
                    (query_ws: [n]f32)
                    (qleaves: [n]i32)
                    (stacks: [n]i32)
                    (dists: [n]f32)
                    (query_inds: [n]i32)
                    (res: f32) : ([n]i32, [n]i32, [n]f32, [n]i32, f32) =
  let queries_sorted = gather2D queries query_inds
  let query_ws_sorted = gather1D query_ws query_inds
  let new_res =
    map3 (\query query_w leaf_ind ->
            if leaf_ind >= i32.i64 num_leaves
            then 0.0f32
            else bruteForce radius query query_w (leaves[leaf_ind]) (ws[leaf_ind]))
         queries_sorted
         query_ws_sorted
         qleaves
    |> reduce (+) 0.0f32
    |> opaque
  let (new_leaves, new_stacks, new_dists) =
    unzip3
    <| map4 (\query leaf_ind stack dist ->
               if leaf_ind >= i32.i64 num_leaves
               then (leaf_ind, stack, dist)
               else traverseOnce radius
                                 h
                                 kd_tree
                                 query
                                 (leaf_ind, stack, dist))
            queries_sorted
            qleaves
            stacks
            dists
    |> opaque
  let (qleaves', sort_inds) = sortQueriesByLeavesRadix new_leaves
  let stacks' = gather1D new_stacks sort_inds
  let dists' = gather1D new_dists sort_inds
  let query_inds' = gather1D query_inds sort_inds
  in (qleaves', stacks', dists', query_inds', res + new_res)

def propagate [m1] [m] [q] [d] [n]
              (radius: f32)
              (ref_pts: [m][d]f32)
              (indir: [m]i32)
              (kd_tree: [q](i32, f32, i32))
              (queries: [n][d]f32)
              (query_ws: [n]f32, ref_ws_orig: [m1]f32) : f32 =
  let kd_weights =
    map i64.i32 indir
    |> map (\ind -> if ind >= m1 then 1.0f32 else ref_ws_orig[ind])
  let (median_dims, median_vals, _) = unzip3 kd_tree
  let num_nodes = q
  -- trace q
  let num_leaves = num_nodes + 1
  let h = (log2 (i32.i64 num_leaves)) - 1
  let ppl = m / num_leaves
  let leaves = unflatten (sized (num_leaves * ppl) ref_pts)
  let kd_ws_sort = unflatten (sized (num_leaves * ppl) kd_weights)
  let query_leaves = map (findLeaf median_dims median_vals h) queries
  let (qleaves, query_inds) = sortQueriesByLeavesRadix query_leaves
  let dists = replicate n 0.0f32
  let stacks = replicate n 0i32
  let res_ws = 0f32
  let (_qleaves', _stacks', _dists', _query_inds', res_ws') =
    loop (qleaves: [n]i32, stacks: [n]i32, dists: [n]f32, query_inds: [n]i32, res_ws: f32)
    for _i < 8 do
      iterationSorted radius h kd_tree leaves kd_ws_sort queries query_ws qleaves stacks dists query_inds res_ws
  in res_ws'

def rev_prop [m1] [m] [q] [d] [n]
             (radius: f32)
             (ref_pts: [m][d]f32)
             (indir: [m]i32)
             (kd_tree: [q](i32, f32, i32))
             (queries: [n][d]f32)
             (query_ws: [n]f32, ref_ws_orig: [m1]f32) : (f32, ([n]f32, [m1]f32)) =
  let f = propagate radius ref_pts indir kd_tree queries
  in vjp2 f (query_ws, ref_ws_orig) 1.0f32

def main [d] [n] [m] [m'] [q]
         (sq_radius: f32)
         (queries: [n][d]f32)
         (query_ws: [n]f32)
         (ref_ws: [m]f32)
         (refs_pts: [m'][d]f32)
         (indir: [m']i32)
         (median_dims: [q]i32)
         (median_vals: [q]f32)
         (clanc_eqdim: [q]i32) =
  let (res, (query_ws_adj, ref_ws_adj)) =
    rev_prop sq_radius refs_pts indir (zip3 median_dims median_vals clanc_eqdim) queries (query_ws, ref_ws)
  in (res, query_ws_adj, ref_ws_adj)
