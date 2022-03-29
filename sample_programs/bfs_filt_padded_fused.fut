-- A decently optimized start point, but which uses padding aggressively.
-- It gives the best runtimes, except for skewed datasets, where it is
-- very poor indeed (because it does not respect the work asymptotic).
-- ==
--
-- tags { }
-- input @ data/4096nodes.in
-- output @ data/4096nodes.out
-- input @ data/512nodes_high_edge_variance.in.gz
-- output @ data/512nodes_high_edge_variance.out
-- input @ data/graph1MW_6.in.gz
-- output @ data/graph1MW_6.out.gz
-- input @ data/64kn_32e-var-1-256-skew.in.gz
-- output @ data/64kn_32e-var-1-256-skew.out

-- Paper's tests!
-- input @ data/bin-6kn_2ke-ct.in
-- output @ data/bin-6kn_2ke-ct.out
-- input @ data/bin-6kn_2ke-var.in
-- output @ data/bin-6kn_2ke-var.out
-- input @ data/bin-400kn_30e-ct.in
-- output @ data/bin-400kn_30e-ct.out
-- input @ data/bin-20kn_600e-var.in
-- output @ data/bin-20kn_600e-var.out
-- input @ data/graph1MW_6.in.gz
-- output @ data/graph1MW_6.out.gz
-- input @ data/64kn_32e-var-1-256-skew.in
-- output @ data/64kn_32e-var-1-256-skew.out

def step [n][e]
        (cost: *[n]i32)
        (nodes_start_index: [n]i32)
        (nodes_n_edges: [n]i32)
        (edges_dest: [e]i32)
        (graph_visited: [n]bool)
        (graph_mask: *[n]bool)
        (updating_graph_mask: *[n]bool) : (*[n]i32, *[n]bool, *[n]bool) =
  let [n_indices] (active_indices : [n_indices]i64, _) = unzip (filter (.1) (zip (iota n) graph_mask))

  let graph_mask' =
    scatter graph_mask active_indices (map (const false) active_indices)

  -- We calculate the maximum number of edges for a node.  This is necessary,
  -- since the number of edges are irregular, and since we want to construct a
  -- nested array.
  let e_max = i32.maximum nodes_n_edges
  let active_costs = map (\tid -> #[unsafe] cost[tid]) active_indices

  -- let start_indices = map (\tid -> #[unsafe] nodes_start_index[tid]) active_indices
  -- let act_num_edges = map (\tid -> #[unsafe] nodes_n_edges[tid]    ) active_indices
  -- let active_costs  = map (\tid -> #[unsafe] cost[tid]+1           ) active_indices
  -- let e_max = i32.maximum act_num_edges

  let flat_len = i64.i32 e_max * n_indices
  let changes = map (\ii -> let row = ii / e_max
                            let col = ii % e_max
                            -- let n_edges     = #[unsafe] act_num_edges[row]
                            let tid     = #[unsafe] active_indices[row]
                            let n_edges = #[unsafe] nodes_n_edges[tid]
                            in  #[unsafe]
                                if col < n_edges
                                then -- let start_index = #[unsafe] start_indices[row]
                                     let start_index = #[unsafe] nodes_start_index[tid]
                                     let edge_index  = col+start_index
                                     let node_id = #[unsafe] edges_dest[edge_index]
                                     in  if !(#[unsafe] graph_visited[node_id])
                                         then (i64.i32 node_id, active_costs[row]+1)
                                         -- then (node_id, #[unsafe] cost[tid] + 1)
                                         else (-1, -1)
                                else (-1, -1)
                    ) (map i32.i64 (iota flat_len))

  let (changes_node_ids, changes_costs) = unzip(changes)

  let cost' =
      scatter cost changes_node_ids changes_costs

  let updating_graph_mask' =
      scatter updating_graph_mask changes_node_ids (replicate flat_len true)

  in (cost', graph_mask', updating_graph_mask')

import "common"

def main = common_main step
