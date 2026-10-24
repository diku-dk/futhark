entry main (xs: []i32) (ys: []bool) = map2 (\x y -> {i32 = x, bool = y}) xs ys

-- Used to check that commands that fail do not leave behind
-- uninitialised variables.
type sum = #foo i32 | #bar i32 bool

entry sum (x: i32) (b: bool) : sum = #bar x b

-- Used to check that array shapes whose size overflows are rejected.
entry mat (xss: [][]i32) = xss
