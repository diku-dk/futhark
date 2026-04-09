-- ==
-- property: prop_record_sums_succ

-- ==
-- property: prop_record_sums_fail

import "../../libraries/toString/toString"

type record = { s: i32, a: i32 }
type~ arr = []record

let all_equal (r: record) : bool =
  r.s == r.a

let prop_all_equal (xs: arr) : bool =
  map all_equal xs |> reduce (&&) true

entry gen_record_sums (size: i64) (_: i32) : arr =
  let n = if size < 0 then 0 else size
  in replicate n {s=1i32, a=1i32}

#[prop(gen(gen_record_sums))]
entry prop_record_sums_succ (input: arr) : bool =
  prop_all_equal input

entry gen_record_sums_fail (size: i64) (_: i32) : arr =
  let n = if size < 0 then 0 else size
  in tabulate n (\i ->
       if i == 0i64 then {s=1i32, a=1i32}
       else {s=2i32, a=1i32})

let shrink_i32 (x: i32) : i32 =
  if x == 0i32 then 0i32
  else if i32.abs x <= 1i32 then 0i32
  else x / 2i32

let shrink_record_field (r: record) (field: i32) : (record, bool) =
  if field == 0i32 then
    let s' = shrink_i32 r.s
    let r' = {s = s', a = r.a}
    in (r', r' != r)
  else
    let a' = shrink_i32 r.a
    let r' = {s = r.s, a = a'}
    in (r', r' != r)

let replace_at [n] (xs: [n]record) (idx: i64) (v: record) : [n]record =
  tabulate n (\i -> if i == idx then v else xs[i])

entry shrink_arr_record (xs: arr) (tactic: i32) : (arr, i8) =
  let ok  : i8 = i8.i32 0
  let nop : i8 = i8.i32 1
  let end : i8 = i8.i32 2

  let n : i64 = length xs
  let field_tactics : i32 = 2i32 * i32.i64 n
  let total_tactics : i32 =
    if n <= 1 then field_tactics
    else 2i32 + field_tactics

  let t : i32 = if tactic < 0i32 then 0i32 else tactic
  in if t >= total_tactics then
       (xs, end)
     else if n > 1 && t == 0i32 then
       let xs' = take (n - 1) xs
       in (xs', ok)
     else if n > 1 && t == 1i32 then
       let xs' = drop 1 xs
       in (xs', ok)
     else
       let loc : i32 = if n > 1 then t - 2i32 else t
       let idx : i64 = i64.i32 (loc / 2i32)
       let field : i32 = loc % 2i32
       let xsn : [n]record = xs :> [n]record
       let old = xsn[idx]
       let (new, changed) = shrink_record_field old field
       let ysn = replace_at xsn idx new
       let ys : arr = ysn :> arr
       in if changed then (ys, ok) else (xs, nop)

#[prop(gen(gen_record_sums_fail), shrink(shrink_arr_record), pprint(pp_arrRecord))]
entry prop_record_sums_fail (input: arr) : bool =
  prop_all_equal input

def pp_record (r: record) : []u8 =
  "{" ++
  "s: " ++ i32_to_string r.s ++
  ", a: " ++ i32_to_string r.a ++
  "}"

entry pp_arrRecord (input: arr) : []u8 =
  let n = length input
  let body =
    loop acc = ""
    for i < n do
      let sep = if i == 0 then "" else ", "
      in acc ++ sep ++ pp_record input[i]
  in "[" ++ body ++ "]"