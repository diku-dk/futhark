-- ==
-- property: prop_record_sums_succ
-- property: prop_record_sums_fail

type~ arr = []i32

def all_equal_1 (x: i32) : bool =
  x == 1

def prop_all_ones (xs: arr) : bool =
  map all_equal_1 xs |> reduce (&&) true

-- Succeeding generator: all 1s
entry gen_record_sums (size: i64) (_: u64) : arr =
  let n = if size < 0 then 0 else size
  in replicate n 1

#[prop(gen(gen_record_sums))]
entry prop_record_sums_succ (input: arr) : bool =
  prop_all_ones input

-- Failing generator: first element 1, rest 0 (if size>1)
entry gen_record_sums_fail (size: i64) (_: u64) : arr =
  let n = if size < 0 then 0 else size
  let idx = iota n |> map (\i -> i32.i64 i)
  in map (\i -> if i == 0 then 1 else 0) idx

entry shrink_arr (xs: []i32) (random: u64) : []i32 =
  let tactic = random % 4
  let n = length xs
  let t = if tactic < 0 then 0 else tactic
  in if t < u64.i64 n
     then if n <= 0
          then xs
          else if n == 1
          then 
               []
          else let i = i64.u64 t
               let pre = take i xs
               let post = drop (i + 1) xs
               in pre ++ post
     else
     if t < u64.i64 (2 * n)
     then if n == 0
          then xs
          else let ki: i64 = i64.u64 t - n
               let old = xs[ki]
               in if old == 1
                  then xs
                  else tabulate n (\i -> if i == ki then 1 else xs[i])
     else xs

#[prop(gen(gen_record_sums_fail), shrink(shrink_arr))]
entry prop_record_sums_fail (input: arr) : bool =
  prop_all_ones input
