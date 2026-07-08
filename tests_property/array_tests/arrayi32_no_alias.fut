-- ==
-- property: prop_record_sums_succ
-- property: prop_record_sums_fail

def all_equal_1 (x: i32) : bool =
  x == 1i32

def prop_all_ones (xs: []i32) : bool =
  map all_equal_1 xs |> reduce (&&) true

-- Succeeding generator: all 1s
entry gen_record_sums (size: i64) (_: u64) : []i32 =
  let n = if size < 0 then 0 else size
  in replicate n 1i32

#[prop(gen(gen_record_sums))]
entry prop_record_sums_succ (input: []i32) : bool =
  prop_all_ones input

-- Failing generator: first element 1, rest 0 (if size>1)
entry gen_record_sums_fail (size: i64) (_: u64) : []i32 =
  let n = if size < 0 then 0 else size
  let idx: []i64 = iota n
  in map (\i -> if i == 0i64 then 1i32 else 0i32) idx

-- New-protocol shrinker for []i32 (SWAPPED order)
entry shrink_arr (xs: []i32) (random: u64) : []i32 =
  let tactic = random % 4
  let n = length xs
  let t = if tactic < 0 then 0 else tactic
  -- ----- Phase 1: remove one element (t in [0..n-1]) -----
  in if t < u64.i64 n
     then if n <= 0
          then xs
          else if n == 1
          then -- removing would make it empty; allow it (often helps)
               []
          else let i = i64.u64 t
               let pre = take i xs
               let post = drop (i + 1) xs
               in pre ++ post
     else -- ----- Phase 2: shrink scalars toward 1 (t in [n..2n-1]) -----
     if t < u64.i64 (2 * n)
     then if n == 0
          then xs
          else let ki: i64 = i64.u64 t - n
               -- set xs[k] to 1 if it isn't already
               let old = xs[ki]
               in if old == 1
                  then xs
                  else tabulate n (\i -> if i == ki then 1 else xs[i])
     else xs

#[prop(gen(gen_record_sums_fail),shrink(shrink_arr))]
entry prop_record_sums_fail (input: []i32) : bool =
  prop_all_ones input
