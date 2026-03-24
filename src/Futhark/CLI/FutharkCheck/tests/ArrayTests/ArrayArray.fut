-- Property target:
-- For a rectangular matrix, compute the sum of each row.
-- Check these row-sums are nondecreasing: sums[0] <= sums[1] <= ... <= sums[r-1].

let row_sums (xss: [][]i32) : []i32 =
  map (\xs -> reduce (+) 0i32 xs) xss

let nondecreasing (xs: []i32) : bool =
  let n = length xs
  in if n < 2 then true
     else
       map2 (<=) ((take (n-1) xs):>[n-1]i32) ((drop 1 xs):>[n-1]i32)
       |> reduce (&&) true

-- Succeeding generator: same row length everywhere, and row sums increase with row index.
entry gen_matrix_sums (size: i64) (seed: i32) : [][]i32 =
  let s = if size < 0 then 0 else size
  let r_i32 = if s == 0 then 0 else 1 + (i32.abs seed) % i32.i64 (s + 1)
  let c_i32 = if s == 0 then 0 else 1 + (i32.abs (seed * 17 + 3)) % i32.i64 (s + 1)
  let r = i64.i32 r_i32
  let c = i64.i32 c_i32

  let base_row : [c]i32 =
    iota c |> map (\j -> i32.i64 j)

  let make_row (ri: i64) : [c]i32 =
    let offset = (i32.i64 ri) * 10i32
    in map (\x -> x + offset) base_row

  in tabulate r make_row

#[prop(gen(gen_matrix_sums), shrink(shrink_matrix_sums))]
entry prop_matrix_sums_succ (input: [][]i32) : bool =
  nondecreasing (row_sums input)

-- New protocol shrinker:
-- (xss, tactic) -> (xss', status)
--
-- tactic 0: try dropping the last row
-- tactic 1: try dropping the first row
-- tactic >= 2: stop shrinking
--
-- status i8:
--   0 = produced candidate (normal)
--   1 = produced candidate but "advance tactic" hint (runner rule: if FAIL and status=1 then tactic=0 anyway, but we keep the channel)
--   2 = stop
entry shrink_matrix_sums (xss: [][]i32) (tactic: i32) : ([][]i32, i8) =
  let r = length xss
  in if r <= 1 then
       (xss, 2i8)
     else if tactic == 0 then
       -- drop last row
       let xss' = take (r-1) xss
       in (xss', 0i8)
     else if tactic == 1 then
       -- drop first row
       let xss' = drop 1 xss
       in (xss', 0i8)
     else
       (xss, 2i8)


entry gen_matrix_sums_bad (size: i64) (seed: i32) : [][]i32 =
  let xss = gen_matrix_sums size seed
  let r = length xss
  in if r < 2 then
       xss
     else
       -- swap first two rows
       let a = xss[0]
       let b = xss[1]
       in [b] ++ [a] ++ drop 2 xss

-- Helper: clamp index to [0..n-1]
let clamp_index (n: i64) (i: i32) : i64 =
  let ii = i64.i32 i
  in if n <= 0 then 0
     else if ii < 0 then 0
     else if ii >= n then n-1
     else ii

-- Remove element at index i from a 1D array (keeps order).
let remove_at (i: i32) (xs: []i32) : []i32 =
  let n = length xs
  in if n == 0 then xs
     else
       let ii = clamp_index n i
       let pre  = take ii xs
       let post = drop (ii+1) xs
       in pre ++ post

-- Remove row i from a 2D array.
let remove_row (i: i32) (xss: [][]i32) : [][]i32 =
  let r = length xss
  in if r == 0 then xss
     else
       let ii = clamp_index r i
       let pre  = take ii xss
       let post = drop (ii+1) xss
       in pre ++ post

let remove_col (j: i32) (xss: [][]i32) : [][]i32 =
  let r = length xss
  in if r == 0 then xss
     else
       let c = length xss[0]
       in if c <= 1 then xss
          else
            let jj   = clamp_index c j
            let newc = c - 1
            let mk_row (row: []i32) : [newc]i32 =
              tabulate newc (\k ->
                let src = if k < jj then k else k + 1
                in row[src])
            in (map mk_row xss) :> [][]i32

-- Shrink a scalar toward 0 (simple and terminating).
-- If already 0 -> unchanged.
-- If abs <= 1 -> goes to 0.
-- Else -> halves toward 0 (keeps sign).
let shrink_i32 (v: i32) : i32 =
  if v == 0i32 then 0i32
  else if i32.abs v <= 1i32 then 0i32
  else v / 2i32

-- Update a single cell (ri,ci) in a rectangular matrix [r][c]i32.
-- Always returns [r][c]i32 (shape-preserving).
let update_cell_rc [r][c] (ri: i64) (ci: i64) (xss: [r][c]i32) : [r][c]i32 =
  let r_ok = 0 <= ri && ri < r
  let c_ok = 0 <= ci && ci < c
  in if !(r_ok && c_ok) then xss
     else
       let old = xss[ri][ci]
       let new = shrink_i32 old
       in if new == old then xss
          else
            tabulate r (\i ->
              tabulate c (\j ->
                if i == ri && j == ci then new else xss[i][j]))




entry shrink_matrix (xss: [][]i32) (tactic: i32) : ([][]i32, i8) =
  let r : i64 = length xss
  let c : i64 = if r == 0 then 0 else length xss[0]

  -- rectangular check (needed for column removal + cell shrink)
  let rect : bool =
    if r == 0 then true
    else reduce (&&) true (map (\row -> length row == c) xss)

  let rowCount : i64 = if r > 1 then r else 0
  let colCount : i64 = if rect && c > 1 then c else 0
  let scalarCount : i64 = if rect then r * c else 0

  let t : i64 = if tactic < 0i32 then 0i64 else i64.i32 tactic

  let s0 : i8 = i8.i32 0
  let s1 : i8 = i8.i32 1
  let s2 : i8 = i8.i32 2

  in if t < rowCount then
       -- remove row t
       (remove_row (i32.i64 t) xss, s0)

     else if t < rowCount + colCount then
       -- remove column j = t-rowCount
       let j : i32 = i32.i64 (t - rowCount)
       in (remove_col j xss, s0)

     else if t < rowCount + colCount + scalarCount then
       -- shrink one cell k = t-rowCount-colCount
       let k : i64 = t - rowCount - colCount
       in if r == 0 || c == 0 then
            (xss, s1)
          else
            let ri : i64 = k / c
            let ci : i64 = k % c
            let xss_rc : [r][c]i32 = xss :> [r][c]i32
            let old : i32 = xss_rc[ri][ci]
            let new : i32 = shrink_i32 old
            in if new == old then
                 -- no-op: advance tactic
                 (xss, s1)
               else
                 let xss_rc' : [r][c]i32 =
                   tabulate r (\i ->
                     tabulate c (\j ->
                       if i == ri && j == ci then new else xss_rc[i][j]))
                 let xss' : [][]i32 = xss_rc' :> [][]i32
                 in (xss', s0)

     else
       -- exhausted tactics
       (xss, s2)


#[prop(gen(gen_matrix_sums_bad), shrink(shrink_matrix))]
entry prop_matrix_sums_fail (input: [][]i32) : bool =
  nondecreasing (row_sums input)