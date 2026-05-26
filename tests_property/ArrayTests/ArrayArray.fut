-- ==
-- property: prop_matrix_sums_succ

-- ==
-- property: prop_matrix_sums_fail

-- Property target:
-- For a rectangular matrix, compute the sum of each row.
-- Check these row-sums are nondecreasing: sums[0] <= sums[1] <= ... <= sums[r-1].

def row_sums (xss: [][]i32) : []i32 =
  map (\xs -> reduce (+) 0i32 xs) xss

def nondecreasing (xs: []i32) : bool =
  let n = length xs
  in if n < 2
     then true
     else map2 (<=) ((take (n - 1) xs) :> [n - 1]i32) ((drop 1 xs) :> [n - 1]i32)
          |> reduce (&&) true

#[prop(size(1))]
entry prop_matrix_sums_succ (input: [][]i32) : bool =
  nondecreasing (row_sums input)

#[prop]
entry prop_matrix_sums_fail (input: [][]i32) : bool =
  nondecreasing (row_sums input)
