-- This program exposed an obscure problem in the type-checking of
-- loops with existential sizes (before inlining).

def step [n]
         ( cost: *[n]i32
         , updating_graph_mask: *[n]bool
         ) : (*[n]i32, *[n]bool) =
  (cost, updating_graph_mask)

def main (n: i32, cost: *[]i32, updating_graph_mask: *[]bool) =
  loop (cost, updating_graph_mask) = (cost, updating_graph_mask)
  while updating_graph_mask[0] do
    let (cost', updating_graph_mask') = step (cost, updating_graph_mask)
    in (cost', updating_graph_mask')
