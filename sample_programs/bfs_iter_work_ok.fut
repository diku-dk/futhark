-- This uses an iterartive partitioning of the active nodes
-- so that the number of edges are "uniform" in each partition.
-- Respects the work asymptotic.
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
-- input @ data/64kn_32e-var-1-256-skew.in.gz
-- output @ data/64kn_32e-var-1-256-skew.out

def core_step [n] [e]
        ( cost: *[n]i32,
          nodes_start_index: [n]i32,
          nodes_n_edges: [n]i32,
          edges_dest: [e]i32,
          graph_visited: [n]bool,
          updating_graph_mask: *[n]bool,
          e_max: i32,
          active_indices: []i32) : (*[n]i32, *[n]bool) =

  let costs_now = map (\tid -> #[unsafe] cost[tid]) active_indices
  let flat_len = i64.i32 e_max * length active_indices

  let changes = map (\ii ->
                        let row = ii / e_max
                        let col = ii % e_max
                        -- let n_edges     = #[unsafe] act_num_edges[row]
                        let tid     = #[unsafe] active_indices[row]
                        let n_edges = #[unsafe] nodes_n_edges[tid]
                        in  #[unsafe]
                            if col < n_edges
                            then
                                let start_index = #[unsafe] nodes_start_index[tid]
                                let edge_index  = col+start_index
                                let node_id = #[unsafe] edges_dest[edge_index]
                                in  if !(#[unsafe] graph_visited[node_id])
                                    then (i64.i32 node_id, costs_now[row]+1)
                                    -- then (node_id, #[unsafe] cost[tid] + 1)
                                    else (-1, -1)
                            else (-1, -1)
                    ) (map i32.i64 (iota flat_len))

  let (changes_node_ids, changes_costs) = unzip(changes)
  let cost' = scatter cost changes_node_ids changes_costs
  let updating_graph_mask' =
              scatter updating_graph_mask changes_node_ids
                      (replicate flat_len true)
  in  (cost', updating_graph_mask')


def step [n][e]
        (cost: *[n]i32)
        (nodes_start_index: [n]i32)
        (nodes_n_edges: [n]i32)
        (edges_dest: [e]i32)
        (graph_visited: [n]bool)
        (graph_mask: *[n]bool)
        (updating_graph_mask: *[n]bool) : (*[n]i32, *[n]bool, *[n]bool) =

  let (active_indices, _) = unzip (filter (.1) (zip (iota n) graph_mask))

  let graph_mask_res =
    scatter graph_mask active_indices (map (const false) active_indices)

  let active_indices = map i32.i64 active_indices
  let continue = true
  let (cost_res, updating_graph_mask_res, _, _) =
    loop (cost, updating_graph_mask, active_indices, continue)
      while continue do
          let act_num_edges = map (\tid -> #[unsafe] nodes_n_edges[tid]) active_indices
          let max_num_edges = i32.maximum act_num_edges
          let tot_num_edges = i32.sum     act_num_edges
          let e_max = 3 * ( tot_num_edges / i32.i64 (length active_indices) + 1)
          let continue' = max_num_edges > e_max

          in if continue'
             then let (do_inds_now, do_inds_later) =
                    partition (\tid -> #[unsafe] nodes_n_edges[tid] <= e_max)
                              active_indices
                  let (cost', updating_graph_mask') =
                      core_step ( cost, nodes_start_index, nodes_n_edges, edges_dest,
                                  graph_visited, updating_graph_mask, e_max, do_inds_now )
                  in  (cost', updating_graph_mask', do_inds_later, continue')

             else let (cost', updating_graph_mask') =
                      core_step ( cost, nodes_start_index, nodes_n_edges, edges_dest,
                                  graph_visited, updating_graph_mask, e_max,
                                  active_indices )
                  in  (cost', updating_graph_mask', active_indices, continue')
  in (cost_res, graph_mask_res, updating_graph_mask_res)

import "common"

def main = common_main step
