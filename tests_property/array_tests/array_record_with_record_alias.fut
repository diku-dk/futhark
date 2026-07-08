-- ==
-- property: prop_record_sums_succ

-- ==
-- property: prop_record_sums_fail

-- File 2: ONLY record is a type alias (array is []record)
type record = {s: i32, a: i32}

def all_equal (r: record) : bool =
  r.s == r.a

def prop_all_equal (xs: []record) : bool =
  map all_equal xs |> reduce (&&) true

entry gen_record_sums (size: i64) (_: u64) : []record =
  let n = if size < 0 then 0 else size
  in replicate n {s = 1i32, a = 1i32}

#[prop(gen(gen_record_sums))]
entry prop_record_sums_succ (input: []record) : bool =
  prop_all_equal input

entry gen_record_sums_fail (size: i64) (_: u64) : []record =
  let n = if size < 0 then 0 else size
  in tabulate n (\i ->
                   if i == 0i64
                   then {s = 1i32, a = 1i32}
                   else {s = 2i32, a = 1i32})

def shrink_i32 (v: i32) : i32 =
  if v == 0i32
  then 0i32
  else if i32.abs v <= 1i32
  then 0i32
  else v / 2i32

def update_at_n [n] (kk: i64) (xs: [n]record) : ([n]record, bool) =
  let r = xs[kk]
  let s' = shrink_i32 r.s
  let a' = shrink_i32 r.a
  let r' =
    if s' != r.s
    then {s = s', a = r.a}
    else {s = r.s, a = a'}
  let changed = r' != r
  let xs' = tabulate n (\i -> if i == kk then r' else xs[i])
  in (xs', changed)

entry shrink_arr_record (xs: []record) (random: u64) : []record =

  let n = length xs
  let r = i64.abs (i64.u64 random)
  let t = r % (2 * n)
  in if t < n
      then let xsN: [n]record = xs :> [n]record
          let (xsN', changed) = update_at_n t xsN
          let out: []record = xsN' :> []record
          in if changed
              then out
              else
              if n == 1
              then []
              else take t xs ++ drop (t + 1) xs
      else
          let i = t - n
          in if n == 1
              then []
              else take i xs ++ drop (i + 1) xs

#[prop(gen(gen_record_sums_fail),shrink(shrink_arr_record))]
entry prop_record_sums_fail (input: []record) : bool =
  prop_all_equal input
