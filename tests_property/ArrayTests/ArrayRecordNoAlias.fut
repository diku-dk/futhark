-- ==
-- property: prop_record_sums_succ

-- ==
-- property: prop_record_sums_fail

-- File 4: NO type aliases (record inline, array is []{...})
import "../libraries/toString/toString"

def all_equal (r: {s: i32, a: i32}) : bool =
  r.s == r.a

def prop_all_equal (xs: []{s: i32, a: i32}) : bool =
  map all_equal xs |> reduce (&&) true

entry gen_record_sums (size: i64) (_: i32) : []{s: i32, a: i32} =
  let n = if size < 0 then 0 else size
  in replicate n {s = 1i32, a = 1i32}

#[prop(gen(gen_record_sums))]
entry prop_record_sums_succ (input: []{s: i32, a: i32}) : bool =
  prop_all_equal input

entry gen_record_sums_fail (size: i64) (_: i32) : []{s: i32, a: i32} =
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

def shrink_record_field (r: {s: i32, a: i32}) (field: i32) : ({s: i32, a: i32}, bool) =
  if field == 0i32
  then let s' = shrink_i32 r.s
       let r' = {s = s', a = r.a}
       in (r', r' != r)
  else let a' = shrink_i32 r.a
       let r' = {s = r.s, a = a'}
       in (r', r' != r)

def replace_at [n] (xs: [n]{s: i32, a: i32}) (idx: i64) (v: {s: i32, a: i32}) : [n]{s: i32, a: i32} =
  tabulate n (\i -> if i == idx then v else xs[i])

entry shrink_arr_record (xs: []{s: i32, a: i32}) (random: i32) : []{s: i32, a: i32} =
  let n = length xs
  in if n == 0
     then xs
     else let r0 = i64.i32 random
          let r = if r0 < 0 then -r0 else r0
          let total_tactics = n + n
          let t = r % total_tactics
          in if t < n
             then -- Drop one element. If n == 1, this gives [], which passes.
                  take t xs ++ drop (t + 1) xs
             else let i = t - n
                  let xsn: [n]{s: i32, a: i32} = xs :> [n]{s: i32, a: i32}
                  let old = xsn[i]
                  let new =
                    if old.s == old.a
                    then old
                    else if old.s != 1i32 || old.a != 0i32
                    then {s = 1i32, a = 0i32}
                    else old
                  in if new == old
                     then -- No progress possible. Return a passing candidate so the
                          -- runner rejects it and advances the failed-shrink counter.
                          []
                     else let ysn = replace_at xsn i new
                          in ysn :> []{s: i32, a: i32}

#[prop(gen(gen_record_sums_fail),shrink(shrink_arr_record),pprint(pp_arrRecord))]
entry prop_record_sums_fail (input: []{s: i32, a: i32}) : bool =
  prop_all_equal input

def pp_record (r: {s: i32, a: i32}) : []u8 =
  "{"
  ++ "s: "
  ++ i32_to_string r.s
  ++ ", a: "
  ++ i32_to_string r.a
  ++ "}"

entry pp_arrRecord (input: []{s: i32, a: i32}) : []u8 =
  let n = length input
  let body =
    loop acc = ""
    for i < n do
      let sep = if i == 0 then "" else ", "
      in acc ++ sep ++ pp_record input[i]
  in "[" ++ body ++ "]"
