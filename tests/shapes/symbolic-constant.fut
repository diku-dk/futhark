-- A symbolic constant in a type abbreviation should be respected.
-- ==
-- input { 2 } output { [0,1] }
-- input { 3 } error: cannot match shape of type `m_ints`

let m = 2
type m_ints = [m]i32

let main(n: i32) = iota n :> m_ints
