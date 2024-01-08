-- ==
-- error: "value" is not in scope

def f [n] (dmax: i64) (depth: [n]i64) (value: [n]i32) (parent: [n]i64) : []i32 =
  loop value for d in dmax..dmax-1...1 do
    reduce_by_index
    (copy value)
    (+)
    0i32
    (map (\i -> if depth[i] == d then parent[i] else -1) (iota (length value)))
    value
