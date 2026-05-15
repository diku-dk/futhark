-- ==
-- property: prop_record_sums_succ

-- ==
-- property: prop_record_sums_fail

-- File 2: ONLY record is a type alias (array is []record)
import "../libraries/toString/toString"

type record = {s: i32, a: i32}

def all_equal (r: record) : bool =
  r.s == r.a

def prop_all_equal (xs: []record) : bool =
  map all_equal xs |> reduce (&&) true

entry gen_record_sums (size: i64) (_: i32) : []record =
  let n = if size < 0 then 0 else size
  in replicate n {s = 1i32, a = 1i32}

#[prop(gen(gen_record_sums))]
entry prop_record_sums_succ (input: []record) : bool =
  prop_all_equal input

entry gen_record_sums_fail (size: i64) (_: i32) : []record =
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
    else if a' != r.a
    then {s = r.s, a = a'}
    else r
  let changed = r' != r
  let xs' = tabulate n (\i -> if i == kk then r' else xs[i])
  in (xs', changed)

entry shrink_arr_record (xs: []record) (random: i32) : []record =
  let n: i64 = length xs
  in if n == 0
     then xs
     else let r0 = i64.i32 random
          let r = if r0 < 0 then -r0 else r0
          let t = r % (2 * n)
          -- Phase 1: try to shrink one record field.
          in if t < n
             then let xsN: [n]record = xs :> [n]record
                  let (xsN', changed) = update_at_n t xsN
                  let out: []record = xsN' :> []record
                  in if changed
                     then out
                     else -- Avoid returning the same failing candidate.
                     if n == 1
                     then []
                     else take t xs ++ drop (t + 1) xs
             else -- Phase 2: drop one record.

                  let i = t - n
                  in if n == 1
                     then []
                     else take i xs ++ drop (i + 1) xs

--#[prop(gen(gen_record_sums_fail), shrink(shrink_arr_record), pprint(pp_arrRecord))]

#[prop(gen(gen_record_sums_fail),shrink(shrink_arr_record))]
entry prop_record_sums_fail (input: []record) : bool =
  prop_all_equal input

def pp_record (r: record) : []u8 =
  "{"
  ++ "s: "
  ++ i32_to_string r.s
  ++ ", a: "
  ++ i32_to_string r.a
  ++ "}"

entry pp_arrRecord (input: []record) : []u8 =
  let n = length input
  let body =
    loop acc = ""
    for i < n do
      let sep = if i == 0 then "" else ", "
      in acc ++ sep ++ pp_record input[i]
  in "[" ++ body ++ "]"
