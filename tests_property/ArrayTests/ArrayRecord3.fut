-- ==
-- property: prop_record_sums_succ

-- ==
-- property: prop_record_sums_fail

-- File 3: ONLY array is a type alias (record is INLINE, arr = []{...})
import "../libraries/toString/toString"

type~ arr = []{ s: i32, a: i32 }

let all_equal (r: { s: i32, a: i32 }) : bool =
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

let shrink_i32 (v: i32) : i32 =
  if v == 0i32 then 0i32
  else if i32.abs v <= 1i32 then 0i32
  else v / 2i32

let update_at_n [n] (kk: i64) (xs: [n]{ s: i32, a: i32 }) : ([n]{ s: i32, a: i32 }, bool) =
  let r = xs[kk]
  let s' = shrink_i32 r.s
  let a' = shrink_i32 r.a
  let r' =
    if s' != r.s then {s = s', a = r.a}
    else if a' != r.a then {s = r.s, a = a'}
    else r
  let changed = r' != r
  let xs' = tabulate n (\i -> if i == kk then r' else xs[i])
  in (xs', changed)

entry shrink_arr_record (xs: arr) (random: i32) : arr =
  let n : i64 = length xs
  let tactic = random % i32.i64 n
  let t : i32 = if tactic < 0i32 then 0i32 else tactic
  let n_i32 : i32 = i32.i64 n
  in if n == 0 then
       xs
     else if t < n_i32 then
       let kk =
         let k64 = i64.i32 t
         in if k64 < 0 then 0
            else if k64 >= n then n-1
            else k64
       let xsN : [n]{ s: i32, a: i32 } = xs :> [n]{ s: i32, a: i32 }
       let (xsN', changed) = update_at_n kk xsN
       let out : arr = xsN' :> arr
       in if changed then out else xs
     else
       xs

#[prop(gen(gen_record_sums_fail), shrink(shrink_arr_record), pprint(pp_arrRecord))]
entry prop_record_sums_fail (input: arr) : bool =
  prop_all_equal input

def pp_record (r: { s: i32, a: i32 }) : []u8 =
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