-- A non-recursive higher-order function applied *nested* within itself
-- at the same arity, but with different function arguments: the inner
-- application is an argument to the outer one. Each application must be
-- specialised independently. (A regression guard: memoising function
-- liftings by name-and-arity alone would make the outer application
-- wrongly reuse the inner one's specialisation.)
-- ==
-- input { 3 } output { 14 }

def twice (f: i32 -> i32) (x: i32) : i32 = f (f x)

-- twice (*2) 3 = 12, then twice (+1) 12 = 14.
entry main (x: i32) : i32 = twice (\y -> y + 1) (twice (\y -> y * 2) x)
