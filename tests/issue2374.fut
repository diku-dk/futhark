-- Reproducer for an internal compiler error in lift allocations.
-- ==

entry error [n] (succ: [n]i64) : [n]bool =
  let set = rep false
  let is = iota n
  let succ = copy succ
  let (set, _, _) =
    loop (set, succ, is)
    while 1 < length succ do
      let small_set = map (const true) succ
      let small_set[0] = false
      let ns =
        map (\i ->
               if small_set[i]
               then succ[i]
               else -1)
            (indices succ)
      let rs = map (\i -> if small_set[i] then 0 else 1) (indices succ)
      let set = scatter set (map (\j -> is[j]) rs) (rep true)
      let small_set = scatter small_set ns (rep true)
      let keep = filter (\i -> small_set[i]) (indices succ)
      let succ = map (\a -> succ[a]) keep
      in (set, succ, is)
  in set
