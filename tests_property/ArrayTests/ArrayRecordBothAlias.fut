-- ==
-- property: prop_record_sums_succ

-- ==
-- property: prop_record_sums_fail

import "../libraries/toString/toString"

type record = {s: i32, a: i32}
type~ arr = []record

def all_equal (r: record) : bool =
  r.s == r.a

def prop_all_equal (xs: arr) : bool =
  map all_equal xs |> reduce (&&) true

entry gen_record_sums (size: i64) (_: u64) : arr =
  let n = if size < 0 then 0 else size
  in replicate n {s = 1i32, a = 1i32}

#[prop(gen(gen_record_sums))]
entry prop_record_sums_succ (input: arr) : bool =
  prop_all_equal input

entry gen_record_sums_fail (size: i64) (_: u64) : arr =
  let n = if size < 0 then 0 else size
  in tabulate n (\i ->
                   if i == 0i64
                   then {s = 1i32, a = 1i32}
                   else {s = 2i32, a = 1i32})

def shrink_i32 (x: i32) : i32 =
  if x == 0i32
  then 0i32
  else if i32.abs x <= 1i32
  then 0i32
  else x / 2i32

def shrink_record_field (r: record) (field: u64) : (record, bool) =
  if field == 0
  then let s' = shrink_i32 r.s
       let r' = {s = s', a = r.a}
       in (r', r' != r)
  else let a' = shrink_i32 r.a
       let r' = {s = r.s, a = a'}
       in (r', r' != r)

def replace_at [n] (xs: [n]record) (idx: i64) (v: record) : [n]record =
  tabulate n (\i -> if i == idx then v else xs[i])

entry shrink_arr_record (xs: arr) (random: u64) : arr =
  let n: i64 = length xs
  let tactic = random % u64.i64 n
  let field_tactics: i32 = 2i32 * i32.i64 n
  let total_tactics: i32 =
    if n <= 1
    then field_tactics
    else 2i32 + field_tactics
  let t = if tactic < 0 then 0 else tactic
  in if t >= u64.i32 total_tactics
     then xs
     else if n > 1 && t == 0
     then let xs' = take (n - 1) xs
          in xs'
     else if n > 1 && t == 1
     then let xs' = drop 1 xs
          in xs'
     else let loc = if n > 1 then t - 2 else t
          let idx: i64 = i64.u64 (loc / 2)
          let field = loc % 2
          let xsn: [n]record = xs :> [n]record
          let old = xsn[idx]
          let (new, changed) = shrink_record_field old field
          let ysn = replace_at xsn idx new
          let ys: arr = ysn :> arr
          in if changed then ys else xs

--#[prop(gen(gen_record_sums_fail), shrink(shrink_arr_record))]

#[prop(gen(gen_record_sums_fail),shrink(shrink_arr_record),pprint(pp_arrRecord))]
entry prop_record_sums_fail (input: arr) : bool =
  prop_all_equal input

def pp_record (r: record) : []u8 =
  "{"
  ++ "s: "
  ++ i32_to_string r.s
  ++ ", a: "
  ++ i32_to_string r.a
  ++ "}"

entry pp_arrRecord (input: arr) : []u8 =
  let n = length input
  let body =
    loop acc = ""
    for i < n do
      let sep = if i == 0 then "" else ", "
      in acc ++ sep ++ pp_record input[i]
  in "[" ++ body ++ "]"

entry pp_arrRecord_fail (input: arr) : []u32 =
  [7]
